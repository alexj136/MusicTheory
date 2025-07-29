{-# LANGUAGE MagicHash #-}
module TestMusicTheory where

import Prelude hiding ((<>))
import TestFramework
import MusicTheory
import Utils

tests :: [Test]
tests =

    [ Test "C major scale is correct." $
        (map (C <>) majorScale) == [C, D, E, F, G, A, B]

    , Test "C major pentatonic scale is correct." $
        (map (C <>) majorPentatonicScale) == [C, D, E, G, A]

    , Test "G minor pentatonic scale is correct." $
        (map (G <>) minorPentatonicScale) == [G, A#, C, D, F]

    , Test "E natural minor scale is correct." $
        (map (E <>) naturalMinorScale) == [E, F#, G, A, B, C, D]

    , Test "Major pentatonic scale has correct intervals." $
        majorPentatonicScale ==
            [ root
            , majorSecond
            , majorThird
            , perfectFifth
            , majorSixth
            ]

    , Test "Natural minor scale has correct intervals." $
        naturalMinorScale ==
            [ root
            , majorSecond
            , minorThird
            , perfectFourth
            , perfectFifth
            , minorSixth
            , minorSeventh
            ]

    , Test "Minor pentatonic scale has correct intervals." $
        minorPentatonicScale ==
            [ root
            , minorThird
            , perfectFourth
            , perfectFifth
            , minorSeventh
            ]

    , Test "Composing relativise and unrelativise is identity." $ and $
        [ unrelativise (relativise majorScale) == majorScale
        , unrelativise (relativise naturalMinorScale) == naturalMinorScale
        , unrelativise (relativise majorPentatonicScale) == majorPentatonicScale
        ]

    , let mixolydian' :: Scale
          mixolydian' = applyAt [6] flatten majorScale
      in testEqual "Mixolydian mode definitions agree." mixolydian' mixolydian

    , let dorian' :: Scale
          dorian' = applyAt [2, 6] flatten majorScale
      in testEqual "Dorian mode definitions agree." dorian' dorian

    , testEqual "Natural minor scale is Aeolian mode" naturalMinorScale aeolian

    ]
