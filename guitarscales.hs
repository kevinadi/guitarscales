{-# LANGUAGE DeriveDataTypeable #-}

import Control.Monad
import Data.Char
import Data.List
import Data.List.Split
import Data.Maybe
import System.Console.CmdArgs

scales = [  ("Chromatic"        , "1,b2,2,b3,3,4,b5,5,b6,6,b7,7"),
            ("Major"            , "1,2,3,4,5,6,7"),
            ("Minor"            , "1,2,b3,4,5,b6,b7"),
            ("Harmonic Minor"   , "1,2,b3,4,5,b6,7"),
            ("Major Pentatonic" , "1,2,3,5,6"),
            ("Minor Pentatonic" , "1,b3,4,5,b7"),
            ("Blues Pentatonic" , "1,b3,4,b5,5,b7") ]

modes = [   ("Ionian"            , "1,2,3,4,5,6,7"),
            ("Dorian"            , "1,2,b3,4,5,6,b7"),
            ("Phrygian"          , "1,b2,b3,4,5,b6,b7"),
            ("Lydian"            , "1,2,3,#4,5,6,7"),
            ("Mixolydian"        , "1,2,3,4,5,6,b7"),
            ("Aeolian"           , "1,2,b3,4,5,b6,b7"),
            ("Locrian"           , "1,b2,b3,4,b5,b6,b7") ]

chords = [  ("Major"            , "1,3,5"),
            ("Minor"            , "1,b3,5"),
            ("7th"              , "1,3,5,b7"),
            ("Major 7th"        , "1,3,5,7"),
            ("Minor 7th"        , "1,b3,5,b7"),
            ("6th"              , "1,3,5,6"),
            ("Augmented"        , "1,3,#5"),
            ("Diminished"       , "1,b3,b5") ]

scaledict=[ ("Scale", scales),
            ("Mode" , Main.modes),
            ("Chord", chords) ]

notes_flat =  ["C", "Db", "D", "Eb", "E", "F", "Gb", "G", "Ab", "A", "Bb", "B"]
notes_sharp = ["C", "C#", "D", "D#", "E", "F", "F#", "G", "G#", "A", "A#", "B"]
strings = [4, 11, 7, 2, 9, 4] --standard tuning

interval_flat =  ["1", "b2", "2", "b3", "3", "4", "b5", "5", "b6", "6", "b7", "7"]
interval_sharp = ["1", "#1", "2", "#2", "3", "4", "#4", "5", "#5", "6", "#6", "7"]

noteindex :: String -> Int
noteindex n = fromMaybe 0 . elemIndex n $ case last n == '#' of
    True  -> notes_sharp
    False -> notes_flat

scaledegree :: Int -> String -> [Int]
scaledegree tr s =
    let s'      = splitOn "," s
        intvl t = map (\n -> (n+tr) `mod` 12) . catMaybes . map (flip elemIndex t) $ s'
    in intvl $ case any (== '#') s of
        True  -> interval_sharp
        False -> interval_flat

scaleinterval :: (Integral a, Fractional b) => [a] -> [b]
scaleinterval s =
    let scl (n:ns) = n : [ if x < n then x+12 else x | x<-ns ] ++ [n+12]
        intvl s    = zipWith (-) (tail (scl s)) (scl s)
    in map (\i -> (fromIntegral i) / 2) (intvl s)

scalelist :: String -> [String]
scalelist s = fromJust $ lookup s scaledict >>= \a -> return (map fst a)

fretboard :: [Int] -> [[Maybe Int]]
fretboard scale = [ [ if fret s f `elem` scale then Just (fret s f) else Nothing | f <- [0..14] ] | s <- strings ]
    where fret s f = (s+f) `mod` 12

shownote :: Maybe Int -> String -> String -> String
shownote mnote pre post =
    let stringnote i = notes_flat !! (i `mod` 12)
    in case mnote of
        Just note ->
            let n = stringnote note
            in case length n of
                2 -> (pre ++ n ++ (tail post))
                _ -> (pre ++ n ++ post)
        Nothing -> (pre ++ [head post] ++ post)

printstring :: [Maybe Int] -> String
printstring str =
    let printstring' out (s:ss) = printstring' (out ++ (shownote s "|--" "--")) ss
        printstring' out []     = out
    in printstring' [] str

printfretboard :: [[Maybe Int]] -> String
printfretboard frets =
    let dots                   = "                  o           o           o           o                o o"
        printnut n             = shownote n "" " |"
        printfrets' out (f:fs) = printfrets' ((printnut (head f) ++ printstring (tail f)) : out) fs
        printfrets' out []     = intercalate "\n" (reverse (dots:out))
    in printfrets' [] frets

lookupscale :: [String] -> Maybe [Int]
lookupscale cfg = lookup (head cfg) scaledict >>= lookup (last cfg) >>= return . scaledegree (noteindex (cfg!!1))

printscale :: [String] -> String
printscale cfg =
    let scale     = fromJust $ lookupscale cfg >>= return . map Just
        intervals = fromJust $ lookupscale cfg >>= return . scaleinterval
        frets     = fromJust $ lookupscale cfg >>= return . fretboard
    in (init . concatMap (\i -> shownote i "  " "  -") $ scale) ++ "\n" ++
       (intercalate " > " (map show intervals)) ++ "\n" ++
       (printfretboard frets)

data Options = Options {
  mode :: String,
  key :: String,
  qual :: String
} deriving (Data, Typeable)

options :: Options
options = Options {
  mode = "Scale" &= typ "MODE" &= help "scale or chord",
  key = "C" &= typ "KEY",
  qual = "Major" &= typ "QUAL" &= help "major\nminor\nmajor pentatonic\nminor pentatonic"
  }
  &= summary "Print guitar scales"
  &= program "guitarscales"

capitalized :: String -> String
capitalized s = zipWith upper (' ':s) s where
  upper c1 c2 | isSpace c1 && isLower c2 = toUpper c2
  upper c1 c2 = c2

scale_or_chord :: String -> String
scale_or_chord "s" = "Scale"
scale_or_chord "c" = "Chord"
scale_or_chord x = capitalized x

main :: IO ()
main = do
    opts <- cmdArgs options
    putStrLn $ (capitalized . key) opts ++ " " ++ (capitalized . qual) opts ++ " " ++ (scale_or_chord . mode) opts
    putStrLn $ printscale [(scale_or_chord . mode) opts, (capitalized . key) opts, (capitalized . qual) opts]
