import Data.List
import Data.List.Split
import Data.Maybe
import Control.Monad

scales = [  ("Chromatic"        , "1,b2,2,b3,3,4,b5,5,b6,6,b7,7"),
            ("Major"            , "1,2,3,4,5,6,7"),
            ("Natural Minor"    , "1,2,b3,4,5,b6,b7"),
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
            ("Mode" , modes),
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
    
scaleintvl :: Int -> String -> [Int]
scaleintvl tr s = 
    let s' = splitOn "," s
        intvl t = map (\n -> (n+tr) `mod` 12) . catMaybes . map (flip elemIndex t) $ s'
    in intvl $ case any (== '#') s of
        True  -> interval_sharp
        False -> interval_flat     

scalelist :: String -> [String]
scalelist s = fromJust $ lookup s scaledict >>= \a -> return (map fst a)

fretboard :: [Int] -> [[Maybe Int]]
fretboard scale = [ [ if fret s f `elem` scale then Just (fret s f) else Nothing | f <- [0..14] ] | s <- strings ] 
    where fret s f = (s+f) `mod` 12

shownote :: Maybe Int -> String -> String -> String
shownote mnote pre post = 
    let stringnote i = notes_flat !! i
    in case mnote of
        Just note ->
            let n = stringnote note
            in case length n of
                2 -> (pre ++ n ++ (tail post))
                _ -> (pre ++ n ++ post)
        Nothing -> (pre ++ [head post] ++ post)

printfretboard :: [[Maybe Int]] -> IO ()
printfretboard (f:fs) = putStr (shownote (head f) "" " |") >> printstring (tail f) >> printfretboard fs
printfretboard []     = putStrLn dots

printstring :: [Maybe Int] -> IO ()
printstring (s:ss) = putStr (shownote s "|--" "--") >> printstring ss
printstring []     = putStrLn []   

lookupscale :: [String] -> Maybe [Int]
lookupscale cfg = lookup (head cfg) scaledict >>= lookup (last cfg) >>= return . scaleintvl (noteindex (cfg!!1))

dots :: String
dots = "                  o           o           o           o                o o"

main :: IO ()
main = do
    let cfg = ["Scale","G","Major"]
    let cfg = ["Chord","G","Major"]
    let key = noteindex (cfg!!1)
    let scale = fromJust $ lookupscale cfg >>= return . map Just
    let frets = fromJust $ lookupscale cfg >>= return . fretboard

    putStrLn . init . concatMap (\i -> shownote i "  " "  -") $ scale
    printfretboard frets
