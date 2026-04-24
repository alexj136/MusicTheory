from dataclasses import dataclass
from typing import List

from MusicTheory import (
    A,
    B,
    C,
    Cs,
    Chord,
    D,
    E,
    G,
    PitchClass,
    Scale,
    addPitchInterval,
    applyScale,
    semitone,
    subtractPitchClasses,
)


@dataclass(frozen=True)
class String:
    open: PitchClass
    frets: int
    offset: int


Instrument = List[String]


def tuning(instrument: Instrument) -> List[PitchClass]:
    return [s.open for s in instrument]


def simpleInstrument(frets: int, notes: List[PitchClass]) -> Instrument:
    return [String(n, frets, 0) for n in notes]


stdGuitar: Instrument = simpleInstrument(22, [E, A, D, G, B, E])
dadgadGuitar: Instrument = simpleInstrument(22, [D, A, D, G, A, D])
stdBanjo: Instrument = [String(G, 17, 5)] + simpleInstrument(22, [D, G, B, D])
stdIrishBouzouki: Instrument = simpleInstrument(22, [G, D, A, D])
openAIrishBouzouki: Instrument = simpleInstrument(22, [A, Cs, A, E])


def fretboardScaleDiagram(scale: Scale, note: PitchClass, strings: Instrument) -> str:
    scl = applyScale(note, scale)

    def showNote(n: PitchClass) -> str:
        text = str(n)
        if len(text) == 1:
            return f"{text}-"
        return text

    def strDiag(s: String) -> str:
        if s.frets == 0:
            return ""
        if s.offset == 0:
            nxt = String(addPitchInterval(s.open, semitone), s.frets - 1, 0)
            this = f"{showNote(s.open)}-|- " if s.open in scl else "---|- "
            return this + strDiag(nxt)
        return "     " + strDiag(String(s.open, s.frets, s.offset - 1))

    return "\n".join(strDiag(s) for s in reversed(strings))


def inChordFingerings(chord: Chord, string: String) -> List:
    return _icf(0, chord, string)


def _icf(start: int, chord: Chord, s: String) -> List:
    if start <= s.frets:
        return [subtractPitchClasses(n, s.open) for n in chord] + _icf(start + 12, chord, s)
    return []
