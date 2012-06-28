module Graphics.HsCharts.Plots.PolarChart
(
      PolarChart
    , polarChart
    , defaultPolarChart
    -- * Axes
    , angularAxis
    , radialAxis
    -- * Grid
    , PolarGrid
    , polarGrid
) where
import Graphics.Gloss
import Graphics.HsCharts.Types
import Graphics.HsCharts.Components.Grid

-----------------------------------------------------------------------------

-- | Creates an Axis to be used as the angular axis in a polar chart.
angularAxis :: Float -- ^ Chart width.
            -> AxisScale
angularAxis len  = AxisScale len 0 (2 * pi) id

-- | Creates an Axis to be used as the radial axis in a polar chart.
radialAxis :: AxisScaleType -> Float -> Float -> Float -> AxisScale
radialAxis t len min max = (fixedAxisScale t (len / 2) min max) { axisLength = len }

-----------------------------------------------------------------------------

data PolarChart = PolarChart { renderFn :: ([Point2] -> Picture)
                             , points   :: [Point2] }

-- | Polar Chart constructor.
polarChart :: ([Point2] -> Picture) -> [Point2] -> PolarChart
polarChart = PolarChart

defaultPolarChart :: [Point2] -> PolarChart
defaultPolarChart = PolarChart line

-----------------------------------------------------------------------------

instance ToPicture PolarChart where
    toPicture = polarChartToPicture
    
polarChartToPicture p aAxis rAxis = renderFn p cartesianPts
    where
        r               = axisLength rAxis / 2
        scaledPts       = scalePoints aAxis rAxis (points p)
        cartesianPts    = map (centerPt . polarToCartesian) scaledPts
        centerPt (x, y) = (x + r, y + r)

polarToCartesian :: Point2 -> Point2
polarToCartesian (a, r) = (r * cos a, r * sin a)

-----------------------------------------------------------------------------

data PolarGrid = PolarGrid { gridColor :: Color
                           , aStep     :: Float
                           , rStep     :: Float
                           }

polarGrid :: Color -> Float -> Float -> PolarGrid
polarGrid = PolarGrid

instance ToPicture PolarGrid where
    toPicture = polarGridToPicture
    
polarGridToPicture :: PolarGrid -> AxisScale -> AxisScale -> Picture
polarGridToPicture g aAxis rAxis = color (gridColor g) $ pictures $ gridA ++ gridR
   where
        r               = axisLength rAxis / 2
        gridA           = map lineA $ gridPoints aAxis (aStep g) 0
        gridR           = map lineR $ gridPoints rAxis (rStep g) 0
        lineA (_, x)    = line [ (r, r), centerPt $ polarToCartesian (x, r) ]
        lineR (_, y)    = translate r r $ circle y
        centerPt (x, y) = (x + r, y + r)
