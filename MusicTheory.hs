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

type Triad = (Note, Note, Note)

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

showTriad :: Triad -> String
showTriad triad@(r, _3, _5) =
    (case find (isTriad triad) (M.keys triadTemplates) of
        Just name -> (show r) ++ ' ' : (triadTemplates M.! name) ++ " "
        Nothing -> ""
    ) ++ show triad

isTriad :: Triad -> TriadTemplate -> Bool
isTriad triad tt = any (triad ==) (map (applyTemplate tt) (notes triad))

notes :: Triad -> [Note]
notes (r, _3, _5) = [r, _3, _5]

matches :: TriadTemplate -> Triad -> Maybe Note
matches tt triad@(r, _, _)
    | applyTemplate tt r == triad = Just r
    | otherwise                   = Nothing

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
    [ ((majorThird   , perfectFifth), "major")
    , ((minorThird   , perfectFifth), "minor")
    , ((minorThird   , tritone     ), "dimin")
    , ((perfectFourth, perfectFifth), "sus 4")
    , ((majorSecond  , perfectFifth), "sus 2")
    ]

applyTemplate :: TriadTemplate -> Note -> Triad
applyTemplate (i1, i2) r = (r, r <> i1, r <> i2)

buildKey :: Scale -> Note -> [Triad]
buildKey scale r = take (length scale) $ td (applyScale r (extended scale))
    where 
    td :: [Note] -> [Triad]
    td scl = (scl !! 0, scl !! 2, scl !! 4) : td (tail scl)

majorKey :: Note -> [Triad]
majorKey = buildKey majorScale

perfectCadence :: ChordProgression
perfectCadence = [V, I]

applyProgression :: [Triad] -> ChordProgression -> [Triad]
applyProgression key = map (\ref -> key !! (fromEnum ref))

putLines :: (a -> String) -> [a] -> IO ()
putLines _      []     = return ()
putLines showFn (l:ls) = do putStrLn (showFn l) ; putLines showFn ls

main :: IO ()
main = do
    putStrLn $ "C major scale: " ++ show (map (C <>) majorScale)
    putStrLn $ "\nKey of C:"
    putLines showTriad $ majorKey C
    putStrLn $ "\nMajor Pentatonic Key of C:"
    putLines showTriad $ buildKey majorPentatonic C
