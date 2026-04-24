from dataclasses import dataclass
from enum import Enum, IntEnum
from itertools import permutations
from typing import Dict, List, Tuple

from Utils import applyAt, rotate, subscript, without


class PitchClass(IntEnum):
    C = 0
    Cs = 1
    D = 2
    Ds = 3
    E = 4
    F = 5
    Fs = 6
    G = 7
    Gs = 8
    A = 9
    As = 10
    B = 11

    def __str__(self) -> str:
        names = {
            PitchClass.C: "C",
            PitchClass.Cs: "C#",
            PitchClass.D: "D",
            PitchClass.Ds: "D#",
            PitchClass.E: "E",
            PitchClass.F: "F",
            PitchClass.Fs: "F#",
            PitchClass.G: "G",
            PitchClass.Gs: "G#",
            PitchClass.A: "A",
            PitchClass.As: "A#",
            PitchClass.B: "B",
        }
        return names[self]


C = PitchClass.C
Cs = PitchClass.Cs
D = PitchClass.D
Ds = PitchClass.Ds
E = PitchClass.E
F = PitchClass.F
Fs = PitchClass.Fs
G = PitchClass.G
Gs = PitchClass.Gs
A = PitchClass.A
As = PitchClass.As
B = PitchClass.B

as_ = As
bs = C
cs = Cs
ds = Ds
es = F
fs = Fs
gs = Gs
bA = Gs
bB = As
bC = B
bD = Cs
bE = Ds
bF = E
bG = Fs


@dataclass(frozen=True, order=True)
class Interval:
    interval: int

    def __str__(self) -> str:
        x = self.interval
        i, o = x % 12, x // 12
        if i == 0 and o == 0:
            return f"unison ({x})"
        if i == 1 and o == 0:
            return f"minor second ({x})"
        if i == 2 and o == 0:
            return f"major second ({x})"
        if i == 3 and o == 0:
            return f"minor third ({x})"
        if i == 4 and o == 0:
            return f"major third ({x})"
        if i == 5 and o == 0:
            return f"perfect fourth ({x})"
        if i == 6 and o == 0:
            return f"tritone ({x})"
        if i == 7 and o == 0:
            return f"perfect fifth ({x})"
        if i == 8 and o == 0:
            return f"minor sixth ({x})"
        if i == 9 and o == 0:
            return f"major sixth ({x})"
        if i == 10 and o == 0:
            return f"minor seventh ({x})"
        if i == 11 and o == 0:
            return f"major seventh ({x})"
        if i == 0 and o == 1:
            return "1 octave"
        if i == 0:
            return f"{o} octaves"
        return f"{Interval(i)} and {Interval(12 * o)}"


@dataclass(frozen=True, order=True)
class Note:
    note: int

    def __str__(self) -> str:
        return f"{pitchClass(self)}{subscript(octave(self))}"


Scale = List[Interval]
ChordTemplate = List[Interval]
Chord = List[PitchClass]


def pitchClass(n: Note) -> PitchClass:
    return PitchClass(n.note % 12)


def octave(n: Note) -> int:
    return n.note // 12


def showChord(chord: Chord) -> str:
    if not chord:
        return str(chord)
    root_note = chord[0]
    for template, name in chordTemplates.items():
        if isChord(chord, list(template)):
            return f"{root_note} {name} {chord}"
    return str(chord)


chordTemplates: Dict[Tuple[Interval, ...], str] = {}


def isChord(cd: Chord, tt: ChordTemplate) -> bool:
    return any(isChordSimple(list(perm), tt) for perm in permutations(cd))


def isChordSimple(cd: Chord, tt: ChordTemplate) -> bool:
    return bool(cd) and cd == applyTemplate(tt, cd[0])


class ChordRef(Enum):
    I = 0
    II = 1
    III = 2
    IV = 3
    V = 4
    VI = 5
    VII = 6


ChordProgression = List[ChordRef]

semitone = Interval(1)
tone = Interval(2)
minorSecond = semitone
majorSecond = tone
minorThird = Interval(3)
majorThird = Interval(4)
perfectFourth = Interval(5)
tritone = Interval(6)
perfectFifth = Interval(7)
minorSixth = Interval(8)
majorSixth = Interval(9)
minorSeventh = Interval(10)
majorSeventh = Interval(11)
fullOctave = Interval(12)
root = Interval(0)

chordTemplates = {
    tuple([root, majorThird, perfectFifth]): "major",
    tuple([root, majorThird, perfectFifth, majorSeventh]): "major 7th",
    tuple([root, majorThird, perfectFifth, minorSeventh]): "dominant 7th",
    tuple([root, minorThird, perfectFifth]): "minor",
    tuple([root, minorThird, perfectFifth, minorSeventh]): "minor 7th",
    tuple([root, minorThird, tritone]): "diminshed",
    tuple([root, minorThird, tritone, minorSeventh]): "half-diminshed",
    tuple([root, minorThird, tritone, majorSixth]): "diminshed 7th",
    tuple([root, majorThird, minorSixth]): "augmented",
    tuple([root, perfectFourth, perfectFifth]): "suspended 4th",
    tuple([root, majorSecond, perfectFifth]): "suspended 2nd",
}


def addPitchInterval(n: PitchClass, i: Interval) -> PitchClass:
    return PitchClass((int(n) + i.interval) % 12)


def subtractPitchClasses(n: PitchClass, m: PitchClass) -> Interval:
    return Interval(int(n) - int(m))


def intervalSub(i: Interval, j: Interval) -> Interval:
    return Interval(i.interval - j.interval)


def intervalAdd(i: Interval, j: Interval) -> Interval:
    return Interval(i.interval + j.interval)


def flatten(i: Interval) -> Interval:
    return Interval(i.interval - 1)


def applyScale(r: PitchClass, scale: Scale) -> List[PitchClass]:
    return [addPitchInterval(r, i) for i in scale]


def octaveUp(scale: Scale) -> Scale:
    return [intervalAdd(i, Interval(12)) for i in scale]


def extended(scale: Scale) -> Scale:
    return scale + octaveUp(scale)


def relativise(scale: Scale) -> Scale:
    if not scale:
        return []
    loop_input = scale + [octaveUp(scale)[0]]
    rel: List[Interval] = []
    for idx in range(len(loop_input) - 1):
        rel.append(intervalSub(loop_input[idx + 1], loop_input[idx]))
    return rel[: len(scale)]


def unrelativise(scale: Scale) -> Scale:
    output: List[Interval] = [root]
    current = root
    for step in scale:
        current = intervalAdd(current, step)
        output.append(current)
    return output[:-1]


chromaticScale: Scale = [
    root,
    minorSecond,
    majorSecond,
    minorThird,
    majorThird,
    perfectFourth,
    tritone,
    perfectFifth,
    minorSixth,
    majorSixth,
    minorSeventh,
    majorSeventh,
]

majorScale: Scale = [
    root,
    majorSecond,
    majorThird,
    perfectFourth,
    perfectFifth,
    majorSixth,
    majorSeventh,
]

majorPentatonicScale: Scale = without([3, 6], majorScale)
naturalMinorScale: Scale = applyAt([2, 5, 6], flatten, majorScale)
minorPentatonicScale: Scale = without([1, 5], naturalMinorScale)

ionian = majorScale
dorian = unrelativise(rotate(1, relativise(majorScale)))
phrygian = unrelativise(rotate(2, relativise(majorScale)))
lydian = unrelativise(rotate(3, relativise(majorScale)))
mixolydian = unrelativise(rotate(4, relativise(majorScale)))
aeolian = unrelativise(rotate(5, relativise(majorScale)))
locrian = unrelativise(rotate(6, relativise(majorScale)))


def applyTemplate(intervals: ChordTemplate, rootNote: PitchClass) -> Chord:
    return [addPitchInterval(rootNote, i) for i in intervals]


def buildKey(scale: Scale, r: PitchClass) -> List[Chord]:
    notes = applyScale(r, extended(scale))
    return [[notes[i], notes[i + 2], notes[i + 4]] for i in range(len(scale))]


def majorKey(r: PitchClass) -> List[Chord]:
    return buildKey(majorScale, r)


perfectCadence: ChordProgression = [ChordRef.V, ChordRef.I]


def applyProgression(key: List[Chord], progression: ChordProgression) -> List[Chord]:
    return [key[ref.value] for ref in progression]
