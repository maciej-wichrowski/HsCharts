module Graphics.HsCharts.Helpers where
import Numeric


-- | Shows a Float with the least number of decimals possible, with a maximum 
--   of d decimals.
--
-- >>> showFFloat' 3 2.0000001 
-- "2"
-- >>> showFFloat' 3 2.1234567 
-- "2.123"
-- >>> showFFloat' 3 10.000001
-- "10"
showFFloat' :: Int -> Float -> String
showFFloat' d f = wholeNumbers ++ removeTrailingZeros (take (d + 1) decimals)
    where 
        (wholeNumbers, decimals) = break ('.' ==) $ showFFloat Nothing f ""
        removeTrailingZeros []                  = []
        removeTrailingZeros xs | last xs == '0' = removeTrailingZeros (init xs)
                               | last xs == '.' = removeTrailingZeros (init xs)
                               | otherwise      = xs


-----------------------------------------------------------------------------


-- | Gets a given quantile from a list of values.
--
-- >>> quantile 0.25 [1,2,3,4,5]
-- 2.0
-- >>> quantile 0.75 [1,2,3,4,5]
-- 4.0
quantile :: (RealFrac a) => a -> [a] -> a
quantile q xs | isWhole l = 0.5 * ((xs !! (round l - 1)) + xs !! round l)
              | otherwise = xs !! (ceiling l - 1)
    where numXs     = length xs
          l         = fromIntegral numXs * q
          isWhole x = x == fromInteger (round x)


-- | Gets the median from a list of values.
median :: (RealFrac a) => [a] -> a
median = quantile 0.5


-- | Gets three values dividing a list into quartiles. (Q1, median, Q3).
--
-- >>> quartiles [1,2,3,4]
-- (1.5,2.5,3.5)
quartiles :: (RealFrac a) => [a] -> (a, a, a)
quartiles xs = (quantile 0.25 xs, median xs, quantile 0.75 xs)

-- | Gets the five-number summary from a list. (min, Q1, median, Q3, max).
--
-- >>> fiveNum [1,2,3,4]
-- (1.0,1.5,2.5,3.5,4.0)
fiveNum :: (RealFrac a) => [a] -> (a, a, a, a, a)
fiveNum xs = (minimum xs, q1, m, q3, maximum xs)
    where (q1, m, q3) = quartiles xs
