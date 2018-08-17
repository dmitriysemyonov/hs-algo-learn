module Misc.Kirill where

    import Data.Array

    split n = (n `div` 10, n `mod` 10)

    add :: (Num x, Ord x, Integral x) => [x] -> x -> [x]
    add xs 0 = xs
    add [] n = [n]
    add (x : xs) n =
        let (intdiv, remainder) = split (x + n)
        in remainder : (add xs intdiv)

    multarr :: (Num x, Ord x, Integral x) => [x] -> x -> [x]
    multarr [] n = []
    multarr (x : xs) n =
        let (intdiv, remainder) = split (x * n)
        in remainder : ((multarr xs n) `add` intdiv)

    addarrs :: (Num x, Ord x, Integral x) => [x] -> [x] -> [x]
    addarrs xs [] = xs
    addarrs [] ys = ys
    addarrs (x : xs) (y : ys) =
        let (intdiv, remainder) = split (x + y)
        in remainder : ((xs `addarrs` ys) `add` intdiv)


    shift xs = 0 : xs

    multarrs :: (Num x, Ord x, Integral x) => [x] -> [x] -> [x]
    multarrs xs [] = xs
    multarrs xs [x] = multarr xs x
    multarrs [] ys = ys
    multarrs xs (y : ys) = (xs `multarr` y) `addarrs` (shift (xs `multarrs` ys))

