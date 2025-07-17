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
isTriad td tt = any (\perm -> isTriadSimple perm tt) (permutations td)
    where
    isTriadSimple :: Triad -> TriadTemplate -> Bool
    isTriadSimple td@(rt, _3, _5) tt = td == applyTemplate tt rt

permutations :: Triad -> [Triad]
permutations (rt, _3, _5) =
    [ (rt, _3, _5)
    , (_5, rt, _3)
    , (_3, _5, rt)
    , (rt, _5, _3)
    , (_3, rt, _5)
    , (_5, _3, rt)
    ]

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

flatten :: Interval -> Interval
flatten (Interval i) = Interval (i - 1)

applyScale :: Note -> Scale -> [Note]
applyScale r = map (r <>)

extended :: Scale -> Scale
extended scale = scale ++ extended scale

without :: [a] -> [Int] -> [a]
without (h:t) idxs | 0 `elem` idxs = without t (filterZeroesAndSubOne idxs)
                   | otherwise     = h : (without t (map pred idxs))
    where filterZeroesAndSubOne = map pred . filter (/= 0)
without l     [] = l

-- Apply a function to the elements of the given list at the given indices.
-- Indices must be in ascending order, without duplicates.
applyAt :: [Int] -> (a -> a) -> [a] -> [a]
applyAt []    _ l     = l
applyAt (0:i) f (h:t) = (f h) : applyAt (map pred i) f t
applyAt i     f (h:t) = h : applyAt (map pred i) f t

triadTemplates :: M.Map TriadTemplate String
triadTemplates = M.fromList
    [ ((majorThird   , perfectFifth), "major")
    , ((minorThird   , perfectFifth), "minor")
    , ((minorThird   , tritone     ), "dimin")
    , ((perfectFourth, perfectFifth), "sus 4")
    , ((majorSecond  , perfectFifth), "sus 2")
    ]

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

majorPentatonicScale :: Scale
majorPentatonicScale = majorScale `without` [3, 6]
    {-[ root
    , majorSecond
    , majorThird
    , perfectFifth
    , majorSixth
    ]-}

-- AKA Aeolian mode
naturalMinorScale :: Scale
naturalMinorScale = applyAt [2, 5, 6] flatten majorScale
    {-[ root
    , majorSecond
    , minorThird
    , perfectFourth
    , perfectFifth
    , minorSixth
    , minorSeventh
    ]-}

minorPentatonicScale :: Scale
minorPentatonicScale = naturalMinorScale `without` [1, 5]
    {-[ root
    , minorThird
    , perfectFourth
    , perfectFifth
    , minorSeventh
    ]-}

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
    putStrLn $ "C major pentatonic scale: " ++ show (map (C <>) majorPentatonicScale)
    putStrLn $ "G minor pentatonic scale: " ++ show (map (G <>) minorPentatonicScale)
    putStrLn $ "E natural minor scale: " ++ show (map (E <>) naturalMinorScale)
    putStrLn $ "\nKey of C:"
    putLines showTriad $ majorKey C
    putStrLn $ "\nMajor Pentatonic Key of C:"
    putLines showTriad $ buildKey majorPentatonicScale C
