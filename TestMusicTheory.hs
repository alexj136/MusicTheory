{-# LANGUAGE MagicHash #-}
module TestMusicTheory where

import Prelude hiding ((<>))
import TestFramework
import MusicTheory

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

    , Test "relativise and unrelativise are identity" $ and $
        [ unrelativise (relativise majorScale) == majorScale
        , unrelativise (relativise naturalMinorScale) == naturalMinorScale
        , unrelativise (relativise majorPentatonicScale) == majorPentatonicScale
        ]

    ]
