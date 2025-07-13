module Music where

data Note = A | As | B | C | Cs | D | Ds | E | F | Fs | G | Gs
    deriving (Eq, Ord, Enum)

type Interval = Note -> Note

type Scale = [Interval]

newtype Triad = Triad (Note, Note, Note) deriving (Eq, Ord)

instance Show Note where
    show note = case note of
        A  -> "A"
        As -> "A#"
        B  -> "B"
        C  -> "C"
        Cs -> "C#"
        D  -> "D"
        Ds -> "D#"
        E  -> "E"
        F  -> "F"
        Fs -> "F#"
        G  -> "G"
        Gs -> "G#"

instance Show Interval where
    show i = let x = i A in case fromEnum x - fromEnum A of
        0 -> "unison"
        1 -> "semitone"
        2 -> "tone"
        n | n `mod` 2 == 0 ->
            (show (n `div` 2)) ++ " tones"
        n ->
            (show (n `div` 2)) ++ " tones and a semitone"

instance Show Triad where
    show triad@(Triad tup@(r, x, y))
        | triad == majorTriad r = (show r) ++ "maj " ++ (show tup)
        | triad == minorTriad r = (show r) ++ "min " ++ (show tup)
        | triad == diminTriad   r = (show r) ++ "dim " ++ (show tup)
        | otherwise = show tup

bA, bB, bD, bE, bG :: Note
bA = Gs
bB = As
bD = Cs
bE = Ds
bG = Fs

interval :: Int -> Interval
interval n x = (iterate semitone x) !! n

semitone :: Interval
semitone Gs = A
semitone x = succ x

flattened :: Interval
flattened A = Gs
flattened x = pred x

tone :: Interval
tone = semitone . semitone

applyScale :: Note -> Scale -> [Note]
applyScale root (i:s) = let r' = i root in root : applyScale r' s
applyScale _ []    = []

majorScale :: Scale
majorScale = [tone, tone, semitone, tone, tone, tone, semitone]

majorScaleExt :: Scale
majorScaleExt = majorScale ++ majorScaleExt

majorTriad :: Note -> Triad
majorTriad root = let ms = applyScale root majorScale in
    Triad (ms !! 0, ms !! 2, ms !! 4)

minorTriad :: Note -> Triad
minorTriad root = let ms = applyScale root majorScale in
    Triad (ms !! 0, flattened (ms !! 2), ms !! 4)

diminTriad :: Note -> Triad
diminTriad root = Triad (root, interval 3 root, interval 6 root)

majorKey :: Note -> [Triad]
majorKey r = let rMajorScale = applyScale r majorScaleExt in
    take 7 $ td rMajorScale
    where 
    td :: [Note] -> [Triad]
    td scl = Triad (scl !! 0, scl !! 2, scl !! 4) : td (tail scl)
