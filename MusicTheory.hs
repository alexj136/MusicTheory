{-# LANGUAGE MagicHash #-}
module MusicTheory where

import Prelude hiding ((<>))
import qualified Data.Set as S
import qualified Data.Map as M
import Data.List (find)

import Utils

data Note = A | A# | B | C | C# | D | D# | E | F | F# | G | G#
    deriving (Show, Eq, Ord, Enum)

as, bs, cs, ds, es, fs, gs, bA, bB, bC, bD, bE, bF, bG :: Note
as = A# ; bs = C  ; cs = C# ; ds = D# ; es = F  ; fs = F# ; gs = G#
bA = G# ; bB = A# ; bC = B  ; bD = C# ; bE = D# ; bF = E  ; bG = F#

data Interval = Interval { value :: Int }

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

(><) :: Note -> Note -> Interval
n >< m = Interval $ (fromEnum m) - (fromEnum n)

(-~) :: Interval -> Interval -> Interval
(Interval i) -~ (Interval j) = Interval (i - j)

(+~) :: Interval -> Interval -> Interval
(Interval i) +~ (Interval j) = Interval (i + j)

flatten :: Interval -> Interval
flatten (Interval i) = Interval (i - 1)

applyScale :: Note -> Scale -> [Note]
applyScale r = map (r <>)

octaveUp :: Scale -> Scale
octaveUp = map (+~ (Interval 12))

extended :: Scale -> Scale
extended scale = scale ++ extended (octaveUp scale)

-- Convert an absolute scale (where each interval is given relative to the root)
-- to a relative scale (where each interval is relative to the previous note in
-- the scale).
relativise :: Scale -> Scale
relativise scale = take (length scale) $ rel $ scale ++ [head (octaveUp scale)]
    where
    rel (n:[m]) = [m -~ n]
    rel (n:m:s) = m -~ n : rel (m:s)

unrelativise :: Scale -> Scale
unrelativise scale = root : unrel root scale
    where
    unrel :: Interval -> Scale -> Scale
    unrel n (m:[o]) = [n +~ m]
    unrel n (m:s)   = let o = n +~ m in o : unrel o s

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
majorPentatonicScale = without [3, 6] majorScale

naturalMinorScale :: Scale
naturalMinorScale = applyAt [2, 5, 6] flatten majorScale

minorPentatonicScale :: Scale
minorPentatonicScale = without [1, 5] naturalMinorScale

ionian, dorian, phrygian, lydian, mixolydian, aeolian, locrian :: Scale
ionian     = majorScale
dorian     = unrelativise $ rotate 1 $ relativise majorScale
phrygian   = unrelativise $ rotate 2 $ relativise majorScale
lydian     = unrelativise $ rotate 3 $ relativise majorScale
mixolydian = unrelativise $ rotate 4 $ relativise majorScale
aeolian    = unrelativise $ rotate 5 $ relativise majorScale
locrian    = unrelativise $ rotate 6 $ relativise majorScale

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
