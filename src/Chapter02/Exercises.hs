module Chapter02.Exercises
 where
    test :: String
    test = "Test"

    fact :: Int -> Int
    fact n | n == 0 = 1
           | n < 0 = -1
           | otherwise = n * fact (n - 1)

    avg :: (Num n, Fractional n, Eq n) => [n] -> n
    avg l =
        let
          sumlen [] = (0, 0)
          sumlen (x : xs) =
            let (sum, len) = sumlen xs
            in (x + sum, 1 + len)
          (sum, len) = sumlen l
        in case len of
          0 -> 0
          _ -> sum / len

    middle :: [n] -> Maybe n
    middle l = go l l 0
      where
        go [] [] _ = Nothing
        go (s : ss) [] step
            | step `mod` 2 /= 0 = Just s
            | otherwise = Nothing
        go slow@(s : ss) (f : fs) step
          | step `mod` 2 /= 0 = go ss fs (step + 1)
          | otherwise = go slow fs (step + 1)
