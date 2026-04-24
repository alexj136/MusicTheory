import MusicTheory
import StringedInstrument
import TestFramework
import TestMusicTheory
import Utils


def main() -> None:
    print("\nMUSIC THEORY")
    TestFramework.testMain(TestMusicTheory.tests)
    print("\nKey of C:")
    Utils.putLines(MusicTheory.showChord, MusicTheory.majorKey(MusicTheory.C))
    print("\nMajor Pentatonic Key of C:")
    Utils.putLines(
        MusicTheory.showChord,
        MusicTheory.buildKey(MusicTheory.majorPentatonicScale, MusicTheory.C),
    )
    print("\nFingerings in the [D, F, A] triad on the G string:")
    print(
        StringedInstrument.inChordFingerings(
            [MusicTheory.D, MusicTheory.F, MusicTheory.A],
            StringedInstrument.String(open=MusicTheory.G, frets=22, offset=0),
        )
    )
    print("\nC major scale on banjo:")
    print(
        StringedInstrument.fretboardScaleDiagram(
            MusicTheory.majorScale, MusicTheory.C, StringedInstrument.stdBanjo
        )
    )
    print("\nKey of A:")
    Utils.putLines(MusicTheory.showChord, MusicTheory.majorKey(MusicTheory.A))
    print("\nA major scale on Irish bouzouki in A-C#-A-E (open A) tuning:")
    print(
        StringedInstrument.fretboardScaleDiagram(
            MusicTheory.majorScale, MusicTheory.A, StringedInstrument.openAIrishBouzouki
        )
    )


if __name__ == "__main__":
    main()
