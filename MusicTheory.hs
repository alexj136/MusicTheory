{-# LANGUAGE MagicHash #-}

module Music where

import Prelude hiding ((<>))
import qualified Data.Set as S
import qualified Data.Map as M
import Data.List (find)

data Note = A | A# | B | C | C# | D | D# | E | F | F# | G | G#
    deriving (Show, Eq, Ord, Enum)

as, bs, cs, ds, es, fs, gs, bA, bB, bC, bD, bE, bF, bG :: Note
as = A# ; bs = C  ; cs = C# ; ds = D# ; es = F  ; fs = F# ; gs = G#
bA = G# ; bB = A# ; bC = B  ; bD = C# ; bE = D# ; bF = E  ; bG = F#

newtype Interval = Interval Int

type Scale = [Interval]

type TriadTemplate = (Interval, Interval)

newtype Triad = Triad (Note, S.Set Note) deriving (Ord)

instance Show Interval where
    show (Interval x) = case (x `mod` 12, x `div` 12) of
        (0 , 0) ->         "unison (" ++ (show x) ++ ")"
        (1 , 0) ->   "minor second (" ++ (show x) ++ ")"
        (2 , 0) ->   "major second (" ++ (show x) ++ ")"
        (3 , 0) ->    "minor third (" ++ (show x) ++ ")"
        (4 , 0) ->    "major third (" ++ (show x) ++ ")"
        (5 , 0) -> "perfect fourth (" ++ (show x) ++ ")"
        (6 , 0) ->        "tritone (" ++ (show x) ++ ")"
        (7 , 0) ->  "perfect fifth (" ++ (show x) ++ ")"
        (8 , 0) ->    "minor sixth (" ++ (show x) ++ ")"
        (9 , 0) ->    "major sixth (" ++ (show x) ++ ")"
        (10, 0) ->  "minor seventh (" ++ (show x) ++ ")"
        (11, 0) ->  "major seventh (" ++ (show x) ++ ")"
        (0 , 1) -> "1 octave"
        (0 , o) -> (show o) ++ " octaves"
        (i , o) -> (show (Interval i)) ++ " and " ++ show (Interval (12 * o))

instance Eq Interval where
    Interval i == Interval j = (i `mod` 12) == (j `mod` 12)

instance Ord Interval where
    Interval i <= Interval j = (i `mod` 12) <= (j `mod` 12)

instance Show Triad where
    show triad@(Triad (r, ns)) =
        (case find (isTriad triad) (M.keys triadTemplates) of
            Just triadName -> (show r) ++ (triadTemplates M.! triadName)
            Nothing -> ""
        ) ++ show (S.toList ns)

instance Eq Triad where
    Triad (_, s1) == Triad (_, s2) = s1 == s2

isTriad :: Triad -> TriadTemplate -> Bool
isTriad triad td = any (triad ==) (map (applyTd td) (notes triad))

triad :: [Note] -> Triad
triad notes@(root:_) = Triad (root, S.fromList notes)

notes :: Triad -> [Note]
notes (Triad (_, s)) = S.toList s

data ChordRef = I | II | III | IV | V | VI | VII deriving (Show, Eq, Ord, Enum)

type ChordProgression = [ChordRef]

tone, majorSecond, minorThird, majorThird, perfectFourth, tritone, perfectFifth,
    minorSixth, majorSixth, minorSeventh, majorSeventh, root :: Interval
tone          = Interval 2
majorSecond   = tone
minorThird    = Interval 3
majorThird    = Interval 4
perfectFourth = Interval 5
tritone       = Interval 6
perfectFifth  = Interval 7
minorSixth    = Interval 8
majorSixth    = Interval 9
minorSeventh  = Interval 10
majorSeventh  = Interval 11
octave        = Interval 12
root          = Interval 0

(<>) :: Note -> Interval -> Note
n <> (Interval i) = toEnum (((fromEnum n) + i) `mod` 12)

applyScale :: Note -> Scale -> [Note]
applyScale r = map (r <>)

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

triadTemplates :: M.Map TriadTemplate String
triadTemplates = M.fromList
    [ ((majorThird   , perfectFifth), "maj" )
    , ((minorThird   , perfectFifth), "min" )
    , ((minorThird   , tritone     ), "dim" )
    , ((perfectFourth, perfectFifth), "sus4")
    , ((majorSecond  , perfectFifth), "sus2")
    ]

applyTd :: TriadTemplate -> Note -> Triad
applyTd (i1, i2) r = triad [r, r <> i1, r <> i2]

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

putLines :: Show a => [a] -> IO ()
putLines [] = return ()
putLines (l:ls) = do putStrLn (show l) ; putLines ls

main :: IO ()
main = do
    putStrLn $ "C major scale: " ++ show (map (C <>) majorScale)
    putStrLn $ "\nKey of C:"
    putLines $ majorKey C
    putStrLn $ "\nMajor Pentatonic Key of C:"
    putLines $ buildKey majorPentatonic C
