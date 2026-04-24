module TestFramework where

data Test = Test
    { failMsg :: String
    , runTest :: Bool
    }

testEqual :: (Show a, Eq a) => String -> a -> a -> Test
testEqual msg expected actual = Test elaborateMsg (expected == actual)
    where
    elaborateMsg :: String
    elaborateMsg = msg ++ "\n    Expected: " ++ show expected
        ++ "\n    Actual: " ++ show actual

testMain :: [Test] -> IO ()
testMain = tm 0 0
    where
    tm :: Int -> Int -> [Test] -> IO ()
    tm total pass [] = putStrLn $
        "\nTests run: " ++
        (show pass) ++ " passed / " ++
        (show (total - pass)) ++ " failed / " ++
        (show total) ++ " total. "
    tm total pass (test:rest) = case runTest test of
        True -> tm (total + 1) (pass + 1) rest
        False -> do
            putStrLn $ "FAIL: " ++ (failMsg test)
            tm (total + 1) pass rest
