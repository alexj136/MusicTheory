from MusicTheory import *
from TestFramework import *
from Utils import *


tests = [
    Test(
        "C major scale is correct.",
        [addPitchInterval(C, i) for i in majorScale] == [C, D, E, F, G, A, B],
    ),
    Test(
        "C major pentatonic scale is correct.",
        [addPitchInterval(C, i) for i in majorPentatonicScale] == [C, D, E, G, A],
    ),
    Test(
        "G minor pentatonic scale is correct.",
        [addPitchInterval(G, i) for i in minorPentatonicScale] == [G, As, C, D, F],
    ),
    Test(
        "E natural minor scale is correct.",
        [addPitchInterval(E, i) for i in naturalMinorScale] == [E, Fs, G, A, B, C, D],
    ),
    Test(
        "Major pentatonic scale has correct intervals.",
        majorPentatonicScale == [root, majorSecond, majorThird, perfectFifth, majorSixth],
    ),
    Test(
        "Natural minor scale has correct intervals.",
        naturalMinorScale
        == [root, majorSecond, minorThird, perfectFourth, perfectFifth, minorSixth, minorSeventh],
    ),
    Test(
        "Minor pentatonic scale has correct intervals.",
        minorPentatonicScale == [root, minorThird, perfectFourth, perfectFifth, minorSeventh],
    ),
    Test(
        "Composing relativise and unrelativise is identity.",
        all(
            [
                unrelativise(relativise(majorScale)) == majorScale,
                unrelativise(relativise(naturalMinorScale)) == naturalMinorScale,
                unrelativise(relativise(majorPentatonicScale)) == majorPentatonicScale,
            ]
        ),
    ),
    testEqual(
        "Mixolydian mode definitions agree.",
        applyAt([6], flatten, majorScale),
        mixolydian,
    ),
    testEqual(
        "Dorian mode definitions agree.",
        applyAt([2, 6], flatten, majorScale),
        dorian,
    ),
    testEqual("Natural minor scale is Aeolian mode", naturalMinorScale, aeolian),
]
