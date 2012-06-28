module Graphics.HsCharts.Plots.BubbleChart
(
      BubbleChart
    -- * Constructors
    , bubbleChart
    , defaultBubbleChart
    -- * Other functions
    , defaultBubbleFn
) where
import Graphics.Gloss
import Graphics.HsCharts.Types

-----------------------------------------------------------------------------

data BubbleChart = BubbleChart { bubbleFn   :: Point3 -> Picture
                               , zAxis      :: AxisScale
                               , points     :: [Point3] }

-- | Bubble Chart constructor.
bubbleChart :: (Point3 -> Picture) -- ^ Rendering function for each bubble.
            -> AxisScale           -- ^ The z-axis scale.
            -> [Point3]            -- ^ Points to be plotted.
            -> BubbleChart
bubbleChart = BubbleChart

-- | Creates a bubble chart with the default rendering function.
defaultBubbleChart :: AxisScale -> [Point3] -> BubbleChart
defaultBubbleChart = bubbleChart (defaultBubbleFn defaultBubbleColor)

-- | The default bubble rendering function.
defaultBubbleFn :: Color -> Point3 -> Picture
defaultBubbleFn c (_, _, z) = pictures [ color c (circleSolid z)
                                     , thickCircle z 1.5 ]

defaultBubbleColor :: Color
defaultBubbleColor = makeColor 0.18 0.81 0.26 0.5

-----------------------------------------------------------------------------

instance ToPicture BubbleChart where
    toPicture = bubbleChartToPicture

bubbleChartToPicture :: BubbleChart -> AxisScale -> AxisScale -> Picture
bubbleChartToPicture b xAxis yAxis = pictures $ map plotPoint scaledPts
    where (xs, ys, zs)        = unzip3 (points b)
          scaledPts           = zip3 (map (axisScaleFn xAxis) xs)
                                     (map (axisScaleFn yAxis) ys)
                                     (map (axisScaleFn (zAxis b)) zs)
          plotPoint (x, y, z) = translate x y $ bubbleFn b (x, y, z)
