-- | Renders a set of points with a filled area between the horizontal axis and the y-values.
module Graphics.HsCharts.Plots.AreaChart
(
      AreaChart
    -- * Constructors
    , areaChart
    , defaultAreaChart
) where
import Graphics.Gloss
import Graphics.HsCharts.Types
import Data.List

-----------------------------------------------------------------------------

data AreaChart = AreaChart { areaColor  :: Color
                           , points     :: [Point2] }

-- | Creates an area chart with the given area color.
areaChart :: Color -> [Point2] -> AreaChart
areaChart = AreaChart

-- | A default area chart, with a blue, semi-transparent area color.
defaultAreaChart :: [Point2] -> AreaChart
defaultAreaChart = areaChart (makeColor 0.15 0.50 0.75 0.4)

-----------------------------------------------------------------------------

instance ToPicture AreaChart where
    toPicture = areaToPicture

areaToPicture :: AreaChart -> AxisScale -> AxisScale -> Picture
areaToPicture a xAxis yAxis = color (areaColor a) $ pictures $ polies sortedPts
    where sortedPts                    = sort $ scalePoints xAxis yAxis (points a)
          yZero                        = getAxisZero yAxis
          polies []                    = []
          polies (_:[])                = []
          polies ((x1,y1):(x2,y2):pts) = polygon [ (x1, yZero), (x1, y1)
                                                 , (x2, y2), (x2, yZero)
                                                 ] : polies ((x2,y2):pts)
