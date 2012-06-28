module Graphics.HsCharts.Plots.BarChart
(
      BarChart
    , barChart
    , defaultBarChart
    , barAxisScale
    , barAxis
) where
import Graphics.Gloss
import Graphics.HsCharts.Types
import Graphics.HsCharts.Components.Axis
import Data.List

-----------------------------------------------------------------------------

data BarChart = BarChart { barColor :: Color
                         , points   :: [Point2] }

-- | Bar Chart constructor.
barChart :: Color -> [Point2] -> BarChart
barChart = BarChart

defaultBarChart :: [Point2] -> BarChart
defaultBarChart = barChart black

-----------------------------------------------------------------------------

barAxisScale :: Float -> [Float] -> AxisScale
barAxisScale len xs = fixedAxisScale Linear len (minimum xs - 0.5) 
                                                (maximum xs + 0.5)

barAxis :: Axis
barAxis = axis' 0.5 0 AHorizontal 1 0 1

-----------------------------------------------------------------------------

instance ToPicture BarChart where
    toPicture = barChartToPicture
    
barChartToPicture b xAxis yAxis = color (barColor b) $ pictures $ map bar scaledPts
    where
        xScale     = axisScaleFn xAxis 1 - axisScaleFn xAxis 0
        yZero      = getAxisZero yAxis
        scaledPts  = scalePoints xAxis yAxis (points b)
        barWidth   = 0.8 * xScale
        bar (x, y) = translate x yZero $ rectangleUpperSolid barWidth (y - yZero)
