module Main where

import Prelude hiding ((<>))

import Utils
import MusicTheory
import StringedInstrument
import qualified TestMusicTheory as TMT
import TestFramework

main :: IO ()
main = do
    putStrLn "\nMUSIC THEORY"
    testMain TMT.tests
    putStrLn $ "\nKey of C:"
    putLines showTriad $ majorKey C
    putStrLn $ "\nMajor Pentatonic Key of C:"
    putLines showTriad $ buildKey majorPentatonicScale C
    putStrLn $ "\nFingerings in the (D, F, A) triad on the G string:"
    putStrLn $ show $ inTriadFingerings (D, F, A) (String G 22 0)
