module StringedInstrument where

import MusicTheory

-- A string has a tuning (open note), number of frets, and an offset (usually
-- zero, but e.g. the 5th string on a banjo played at the 1st fret is at the
-- same distance from the headstock as the 6th fret for the other strings, so
-- has an offset of 5 (6th minus 1st).
type Strg = (Note, Int, Int)

type Instrument = [Strg]

tuning :: Instrument -> [Note]
tuning = map $ \(n, _, _) -> n

-- A simple instrument is an instrument where each string starts at the
-- headstock and has the same number of frets, e.g. a guitar, ukulele or
-- mandolin.
simpleInstrument :: [Note] -> Instrument
simpleInstrument = map $ \n -> (n, 0, 0)

stdGuitar :: Instrument
stdGuitar = simpleInstrument [E, A, D, G, B, E]

dadgadGuitar :: Instrument
dadgadGuitar = simpleInstrument [D, A, D, G, A, D]
