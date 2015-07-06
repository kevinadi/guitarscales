import Data.List
import Data.List.Split
import Data.Maybe

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

scaledict=[ ("scales", scales),
            ("modes" , modes),
            ("chords", chords) ]

notes_flat =  ["C", "Db", "D", "Eb", "E", "F", "Gb", "G", "Ab", "A", "Bb", "B"]
notes_sharp = ["C", "C#", "D", "D#", "E", "F", "F#", "G", "G#", "A", "A#", "B"]
strings = [4, 11, 7, 2, 9, 4] --standard tuning

interval_flat =  ["1", "b2", "2", "b3", "3", "4", "b5", "5", "b6", "6", "b7", "7"]
interval_sharp = ["1", "#1", "2", "#2", "3", "4", "#4", "5", "#5", "6", "#6", "7"]

noteindex :: String -> Int
noteindex n = fromMaybe 0 . elemIndex n $ case last n == '#' of
    True  -> notes_sharp
    False -> notes_flat

scaleintvl :: String -> [Int]
scaleintvl s = 
    let s' = splitOn "," s
        intvl t = catMaybes . map (flip elemIndex t) $ s'
    in intvl $ case any (== '#') s of
        True  -> interval_sharp
        False -> interval_flat        

scaleintvl' :: Int -> String -> [Int]
scaleintvl' tr s = 
    let s' = splitOn "," s
        intvl t = map (\n -> (n+tr) `mod` 12) . catMaybes . map (flip elemIndex t) $ s'
    in intvl $ case any (== '#') s of
        True  -> interval_sharp
        False -> interval_flat     

scalenotes :: String -> String -> [String]
scalenotes t s =
    let s' = scaleintvl s
        t' = noteindex t
        s'' = map (\n -> (n+t') `mod` 12) s'
        notelist = flip map s'' . (!!)  --notelist notes = map ((!!) notes) s''
    in notelist $ case any (== '#') s of
        True  -> notes_sharp
        False -> notes_flat

scalelist :: String -> [String]
scalelist s = fromJust $ lookup s scaledict >>= \a -> return (map fst a)


fretboard = [[ (s+f) `mod` 12 | f<-[0..15] ] | s<-strings ]


main :: IO ()
main = do
    print $ fromJust $ lookup "scales" scaledict >>= return . map fst
    print $ fromJust $ lookup "scales" scaledict >>= lookup "Major" >>= return . scalenotes "C"
    print $ fromJust $ lookup "scales" scaledict >>= lookup "Natural Minor" >>= return . scalenotes "A"