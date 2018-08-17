module Chapter02.Exercises
 where
    import Data.Char(isDigit, digitToInt)
    import Data.Array

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

    --2.8 b
    rep :: (Num n, Enum n) => n -> [n]
    --variants:
    --rep n = [k | k <- [1..n], _ <- [1..k]]
    rep n = [1..n] >>= (\k -> [1..k] >> return k)
    --rep n = [1..n] >>= (\k ->  (fmap (\_ -> k)  [1..k]))
    {--
    rep n = do
        k <- [1..n]
        _ <- [1..k]
        return k
    --}

    --2.9
    string2int :: String -> Int
    string2int s =
        let digits = [digitToInt c | c <- s, isDigit c]
        in foldl (\acc d -> acc * 10 + d) 0 digits

    --2.13
    ex213a = array ((1, 1), (3, 3)) [((1, 1), 2), ((1, 2), 3), ((1, 3), 4),
        ((2, 1), 5), ((2, 2), 6), ((2, 3), 7),
        ((3, 1), 8), ((3, 2),9 ), ((3, 3), 10) ]

    ex312acomp (m, n)= array((0, 0), (m - 1, n - 1)) [((i, j), 3 * i  + j + 2) | i <- [0..(m - 1)], j <- [0..(n - 1)]]
    transpose m =
        let
            flipIx (i, j) = (j, i)
            flipAssoc (ix, v) = (flipIx ix, v)
        in array(bounds m) (fmap flipAssoc (assocs m))


