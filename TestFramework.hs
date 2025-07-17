module TestFramework where

data Test = Test String Bool

testMain :: [Test] -> IO ()
testMain = tm 0 0
    where
    tm :: Int -> Int -> [Test] -> IO ()
    tm total pass [] = putStrLn $
        "\nTests run: " ++
        (show pass) ++ " passed / " ++
        (show (total - pass)) ++ " failed / " ++
        (show total) ++ " total. "
    tm total pass (Test _ True:rest) = tm (total + 1) (pass + 1) rest
    tm total pass (Test msg False:rest) = do
        putStrLn $ "FAIL: " ++ msg
        tm (total + 1) pass rest
