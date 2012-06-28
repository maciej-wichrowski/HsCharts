-- | A simple vertical bar chart.
--
--   Use the 'barAxis' and 'barAxisScale' functions to fit the bars nicely on 
--   the horizontal axis.
module Graphics.HsCharts.Plots.BarChart
(
      BarChart
    -- * Constructors
    , barChart
    , defaultBarChart
    -- * Axes
    , barAxisScale
    , barAxis
) where
import Graphics.Gloss
import Graphics.HsCharts.Types
import Graphics.HsCharts.Components.Axis

-----------------------------------------------------------------------------

data BarChart = BarChart { barColor :: Color
                         , points   :: [Point2] }

-- | Bar Chart constructor.
barChart :: Color -> [Point2] -> BarChart
barChart = BarChart

-- | A default bar chart, with a black bar color.
defaultBarChart :: [Point2] -> BarChart
defaultBarChart = barChart black

-----------------------------------------------------------------------------

-- | Creates a horizontal axis scale.
barAxisScale :: Float     -- ^ The length of the axis.
             -> [Float]   -- ^ The x-values to be fitted on the axis.
             -> AxisScale
barAxisScale len xs = fixedAxisScale Linear len (minimum xs - 0.5) 
                                                (maximum xs + 0.5)

-- | An axis with a 0.5 scale numbers offset.
barAxis :: Axis
barAxis = axis' 0.5 0 AHorizontal 1 0 1

-----------------------------------------------------------------------------

instance ToPicture BarChart where
    toPicture = barChartToPicture

barChartToPicture :: BarChart -> AxisScale -> AxisScale -> Picture
barChartToPicture b xAxis yAxis = color (barColor b) $ pictures $ map bar scaledPts
    where
        xScale     = axisScaleFn xAxis 1 - axisScaleFn xAxis 0
        yZero      = getAxisZero yAxis
        scaledPts  = scalePoints xAxis yAxis (points b)
        barWidth   = 0.8 * xScale
        bar (x, y) = translate x yZero $ rectangleUpperSolid barWidth (y - yZero)
