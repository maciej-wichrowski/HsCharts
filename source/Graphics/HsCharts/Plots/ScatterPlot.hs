module Graphics.HsCharts.Plots.ScatterPlot
(
      ScatterPlot
    -- * Constructors
    , scatterPlot
    , defaultScatterPlot
) where
import Graphics.Gloss
import Graphics.HsCharts.Types

-----------------------------------------------------------------------------

data ScatterPlot = ScatterPlot { pict   :: Picture
                               , points :: [Point2] }

-- | Scatter plot constructor.
scatterPlot :: Picture     -- ^ Point picture.
            -> [Point2]    -- ^ The data points to be plotted.
            -> ScatterPlot
scatterPlot = ScatterPlot

-- | Creates a scatter plot with the default picture (2px square)
defaultScatterPlot :: [Point2] -> ScatterPlot
defaultScatterPlot = ScatterPlot (rectangleSolid 2 2)

-----------------------------------------------------------------------------

instance ToPicture ScatterPlot where
    toPicture p xAxis yAxis = pictures $ map (\(x, y) -> translate x y (pict p))
                                             (scalePoints xAxis yAxis (points p))
