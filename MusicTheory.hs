{-# LANGUAGE MagicHash #-}

module Music where

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

type Td = (Interval, Interval, Interval)

newtype Triad = Triad (Note, Note, Note) deriving (Eq, Ord)

instance Show Interval where
    show i = let x = i A in intervalText $ fromEnum x - fromEnum A

intervalText :: Int -> String
intervalText x = case x of
        0 -> "octave"
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
    show triad@(Triad tup@(r, x, y))
        | triad == majorTriad r = (show r) ++ "maj " ++ (show tup)
        | triad == minorTriad r = (show r) ++ "min " ++ (show tup)
        | triad == diminTriad r = (show r) ++ "dim " ++ (show tup)
        | otherwise = show tup

semitone :: Interval
semitone G# = A
semitone x = succ x

flattened :: Interval
flattened A = G#
flattened x = pred x

interval :: Int -> Interval
interval n x = (iterate semitone x) !! n

tone, minorThird, majorThird, perfectFourth, tritone, perfectFifth, minorSixth,
    majorSixth, minorSeventh, majorSeventh, root :: Interval
tone          = interval 2
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

applyScale :: Note -> Scale -> [Note]
applyScale root (i:s) = let r' = i root in root : applyScale r' s
applyScale _ []    = []

majorScale :: Scale
majorScale = [tone, tone, semitone, tone, tone, tone, semitone]

majorScaleExt :: Scale
majorScaleExt = majorScale ++ majorScaleExt

majorTd, minorTd, diminTd :: Td
majorTd = (root, majorThird, perfectFifth)
minorTd = (root, minorThird, perfectFifth)
diminTd = (root, minorThird, tritone)

applyTd :: Td -> Note -> Triad
applyTd (i1, i2, i3) r = Triad (i1 r, i2 r, i3 r)

majorTriad, minorTriad, diminTriad :: Note -> Triad
majorTriad = applyTd majorTd
minorTriad = applyTd minorTd
diminTriad = applyTd diminTd

majorKey :: Note -> [Triad]
majorKey r = let rMajorScale = applyScale r majorScaleExt in
    take 7 $ td rMajorScale
    where 
    td :: [Note] -> [Triad]
    td scl = Triad (scl !! 0, scl !! 2, scl !! 4) : td (tail scl)

main :: IO ()
main = do
    putStrLn $ "C major scale: " ++ show (applyScale C majorScale)
    putStrLn $ "Key of C: " ++ show (majorKey C)
