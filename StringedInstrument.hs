module StringedInstrument where

import Prelude hiding (String, (<>))
import qualified Prelude as P (String)
import Data.List (intercalate)

import MusicTheory

-- A string has a tuning (open note), number of frets, and an offset (usually
-- zero, but e.g. the 5th string on a banjo played at the 1st fret is at the
-- same distance from the headstock as the 6th fret for the other strings, so
-- has an offset of 5 (6th minus 1st).
data String = String { open :: Note, frets :: Int, offset :: Int }

type Instrument = [String]

tuning :: Instrument -> [Note]
tuning = map open

-- A simple instrument is an instrument where each string starts at the
-- headstock and has the same number of frets, e.g. a guitar, ukulele or
-- mandolin.
simpleInstrument :: Int -> [Note] -> Instrument
simpleInstrument frets = map $ \n -> String n frets 0

stdGuitar :: Instrument
stdGuitar = simpleInstrument 22 [E, A, D, G, B, E]

dadgadGuitar :: Instrument
dadgadGuitar = simpleInstrument 22 [D, A, D, G, A, D]

stdBanjo :: Instrument
stdBanjo = String G 17 5 : simpleInstrument 22 [D, G, B, D]

fretboardScaleDiagram :: Scale -> Note -> Instrument -> P.String
fretboardScaleDiagram scale note strings =
    intercalate "\n" $ map strDiag (reverse strings)
    where
    scl :: [Note]
    scl = applyScale note scale

    strDiag :: String -> P.String
    strDiag (String _    0     _) = ""
    strDiag (String open frets 0) = this ++ strDiag next where
        next = String (open <> semitone) (frets - 1) 0
        this = if open `elem` scl
            then "|-" ++ showNote open ++ "-"
            else "|----"
    strDiag (String open frets n) =
            "     " ++ strDiag (String open frets (n-1))

    showNote :: Note -> P.String
    showNote n = case show n of { [c1, c2] -> [c1, c2] ; [c] -> [c , '-'] }

inTriadFingerings :: Triad -> String -> [Interval]
inTriadFingerings = itf 0
    where
    itf :: Int -> Triad -> String -> [Interval]
    itf from t@(r, _3, _5) s@(String n f o)
        | from <= f = [n >< r, n >< _3, n >< _5] ++ itf (from + 12) t s
        | otherwise = []
