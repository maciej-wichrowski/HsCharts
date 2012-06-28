module Graphics.HsCharts.Plots.AreaChart
(
      AreaChart
    , areaChart
    , defaultAreaChart
) where
import Graphics.Gloss
import Graphics.HsCharts.Types
import Data.List

-----------------------------------------------------------------------------

data AreaChart = AreaChart { areaColor  :: Color
                           , points     :: [Point2] }

-- | Area Chart constructor.
areaChart :: Color -> [Point2] -> AreaChart
areaChart = AreaChart

defaultAreaChart :: [Point2] -> AreaChart
defaultAreaChart = areaChart black

-----------------------------------------------------------------------------

instance ToPicture AreaChart where
    toPicture = areaToPicture
    
areaToPicture a xAxis yAxis = color (areaColor a) $ pictures $ polies sortedPts
    where sortedPts                    = sort $ scalePoints xAxis yAxis (points a)
          yZero                        = getAxisZero yAxis
          polies []                    = []
          polies (_:[])                = []
          polies ((x1,y1):(x2,y2):pts) = polygon [ (x1, yZero), (x1, y1)
                                                 , (x2, y2), (x2, yZero)
                                                 ] : polies ((x2,y2):pts)
