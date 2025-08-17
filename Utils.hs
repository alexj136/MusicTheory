module Utils where
--
-- Generic utility functions that don't depend on any music theory.
--

rotate :: Int -> [a] -> [a]
rotate _ [] = error "empty list"
rotate 0 l     = l
rotate n (h:t) = rotate (n-1) (t ++ [h])

-- Given a list of elements and a list of indices, return the list of elements
-- with the elements at the given indices removed.
without :: [Int] -> [a] -> [a]
without i = applyAtMaybe i (\_ -> Nothing)

-- Apply a function to the elements of the given list at the given indices.
-- Indices must be in ascending order, without duplicates.
applyAt :: [Int] -> (a -> a) -> [a] -> [a]
applyAt i f = applyAtMaybe i (\e -> Just (f e))

-- Apply a function to the elements of a list at the specified index only. The
-- function should return a Maybe - Just values go back in the list at their
-- original spot; Nothing values get removed.
applyAtMaybe :: [Int] -> (a -> Maybe a) -> [a] -> [a]
applyAtMaybe []    _ l     = l
applyAtMaybe (0:i) f (h:t) = case f h of
    Just e  -> e : applyAtMaybe (map pred i) f t
    Nothing ->     applyAtMaybe (map pred i) f t
applyAtMaybe i     f (h:t) = h : applyAtMaybe (map pred i) f t

putLines :: (a -> String) -> [a] -> IO ()
putLines _      []     = return ()
putLines showFn (l:ls) = do putStrLn (showFn l) ; putLines showFn ls
