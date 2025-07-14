{-# LANGUAGE MagicHash #-}

module Music where

import qualified Data.Set as S

data Note = A | A# | B | C | C# | D | D# | E | F | F# | G | G#
    deriving (Show, Eq, Ord, Enum)

as, bs, cs, ds, es, fs, gs, bA, bB, bC, bD, bE, bF, bG :: Note
as = A#
bs = C
cs = C#
ds = D#
es = F
fs = F#
gs = G#
bA = G#
bB = A#
bC = B
bD = C#
bE = D#
bF = E
bG = F#

type Interval = Note -> Note

type Scale = [Interval]

type TriadIntervals = (Interval, Interval)

newtype Triad = Triad (Note, S.Set Note) deriving (Ord)

instance Show Interval where
    show i = let x = i A in intervalText $ fromEnum x - fromEnum A

instance Eq Interval where
    i == j = x == y where x = i A ; y = j A

instance Ord Interval where
    i <= j = x <= y where x = i A ; y = j A

intervalText :: Int -> String
intervalText x = case x of
        0 -> "root"
        1 -> "semitone"
        2 -> "tone"
        3 -> "minor third"
        4 -> "major third"
        5 -> "perfect fourth"
        6 -> "tritone"
        7 -> "perfect fifth"
        8 -> "minor sixth"
        9 -> "major sixth"
        10 -> "minor seventh"
        11 -> "major seventh"

instance Show Triad where
    show triad@(Triad (r, _))
        | triad == majorTriad r = (show r) ++ "maj"  ++ (showTriadNotes triad)
        | triad == minorTriad r = (show r) ++ "min"  ++ (showTriadNotes triad)
        | triad == diminTriad r = (show r) ++ "dim"  ++ (showTriadNotes triad)
        | triad == sus4Triad  r = (show r) ++ "sus4" ++ (showTriadNotes triad)
        | otherwise = showTriadNotes triad

instance Eq Triad where
    Triad (_, s1) == Triad (_, s2) = s1 == s2

showTriadNotes :: Triad -> String
showTriadNotes (Triad (root, set)) = show (S.toList set)

triad :: [Note] -> Triad
triad notes@(root:_) = Triad (root, S.fromList notes)

data ChordRef = I | II | III | IV | V | VI | VII deriving (Show, Eq, Ord, Enum)

type ChordProgression = [ChordRef]

semitone :: Interval
semitone G# = A
semitone x = succ x

flattened :: Interval
flattened A = G#
flattened x = pred x

interval :: Int -> Interval
interval n x = (iterate semitone x) !! (n `mod` 12)

inverse :: Interval -> Interval
inverse i = let x = i A in interval $ negate $ fromEnum x

tone, majorSecond, minorThird, majorThird, perfectFourth, tritone, perfectFifth,
    minorSixth, majorSixth, minorSeventh, majorSeventh, root :: Interval
tone          = interval 2
majorSecond   = tone
minorThird    = interval 3
majorThird    = interval 4
perfectFourth = interval 5
tritone       = interval 6
perfectFifth  = interval 7
minorSixth    = interval 8
majorSixth    = interval 9
minorSeventh  = interval 10
majorSeventh  = interval 11
octave        = interval 12
root          = interval 0

to :: Note -> Note -> Interval
to x y = interval ((fromEnum y - fromEnum x) `mod` 12)

applyScale :: Note -> Scale -> [Note]
applyScale r = map ($ r)

majorScale :: Scale
majorScale =
    [ root
    , majorSecond
    , majorThird
    , perfectFourth
    , perfectFifth
    , majorSixth
    , majorSeventh
    ]

without :: [a] -> [Int] -> [a]
without (h:t) idxs | 0 `elem` idxs = without t (filterZeroesAndSubOne idxs)
                   | otherwise     = h : (without t (map pred idxs))
    where filterZeroesAndSubOne = map pred . filter (/= 0)
without l     [] = l

majorPentatonic :: Scale
majorPentatonic = majorScale `without` [3, 6]

extended :: Scale -> Scale
extended scale = scale ++ extended scale

majorTd, minorTd, diminTd, sus4Td :: TriadIntervals
majorTd = (majorThird, perfectFifth)
minorTd = (minorThird, perfectFifth)
diminTd = (minorThird, tritone)
sus4Td  = (perfectFourth, perfectFifth)

applyTd :: TriadIntervals -> Note -> Triad
applyTd (i1, i2) r = triad [r, i1 r, i2 r]

majorTriad, minorTriad, diminTriad :: Note -> Triad
majorTriad = applyTd majorTd
minorTriad = applyTd minorTd
diminTriad = applyTd diminTd
sus4Triad  = applyTd sus4Td

buildKey :: Scale -> Note -> [Triad]
buildKey scale r = take (length scale) $ td (applyScale r (extended scale))
    where 
    td :: [Note] -> [Triad]
    td scl = triad [scl !! 0, scl !! 2, scl !! 4] : td (tail scl)

majorKey :: Note -> [Triad]
majorKey = buildKey majorScale

perfectCadence :: ChordProgression
perfectCadence = [V, I]

applyProgression :: [Triad] -> ChordProgression -> [Triad]
applyProgression key = map (\ref -> key !! (fromEnum ref))

main :: IO ()
main = do
    putStrLn $ "C major scale: " ++ show (map ($ C) majorScale)
    putStrLn $ "Key of C: " ++ show (majorKey C)
