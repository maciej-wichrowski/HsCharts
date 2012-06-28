module Graphics.HsCharts.Plots.BoxPlot
(
      BoxPlot
    , boxPlot
    , defaultBoxPlot
) where
import Graphics.Gloss
import Graphics.HsCharts.Types
import Data.List

-----------------------------------------------------------------------------

data BoxPlot = BoxPlot { boxColor :: Color
                       , boxWidth :: Float
                       , points   :: [Point2] }

-- | Box plot constructor.
boxPlot :: Color -> Float -> [Point2] -> BoxPlot
boxPlot = BoxPlot

-- | Creates a box plot with the default fill color (white) and width (20)
defaultBoxPlot :: [Point2] -> BoxPlot
defaultBoxPlot = BoxPlot white 20

-----------------------------------------------------------------------------

instance ToPicture BoxPlot where
    toPicture = boxPlotToPicture


boxPlotToPicture b xAxis yAxis = pictures $ zipWith box xs yss
    where 
        groups   = groupBy (\(x1, _) (x2, _) -> x1 == x2) $ sort (points b)
        xs       = map (axisScaleFn xAxis) $ map (fst . head) groups
        yss      = map (map snd) groups
        box x ys = translate x 0 $ boxPlotToPicture' (boxWidth b) (boxColor b) yAxis ys


-- | Plots a vertical boxplot from a list of 1-dimensional values.
boxPlotToPicture' :: Float     -- ^ Box plot width
                  -> Color     -- ^ Box fill color
                  -> AxisScale -- ^ Vertical axis
                  -> [Float]   -- ^ Plot data
                  -> Picture
boxPlotToPicture' w c yAxis xs = 
    pictures [ line [((-0.25) * w, q0), (0.25 * w, q0)]
             , line [((-0.25) * w, q4), (0.25 * w, q4)]
             , line [(0, q0), (0, q1)]
             , translate 0 (q1 + 0.5 * boxH) $ pictures [ color c $ rectangleSolid w boxH
                                                        , rectangleWire w boxH]
             , line [(0, q3), (0, q4)]
             , line [((-0.5) * w, q2), (0.5 * w, q2)] ]
    where
        scaledXs             = sort $ map (axisScaleFn yAxis) xs
        (q0, q1, q2, q3, q4) = fiveNum scaledXs
        boxH                 = q3 - q1


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