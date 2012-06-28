module Graphics.HsCharts.Types
( 
      Point2
    , Point3
    -- * Axis Scale
    , AxisScale(..)
    , AxisScaleType(..)
    , fixedAxisScale
    , autoAxisScale
    , autoAxisScale'
    , getAxisZero
    , scalePoints
    -- * Gloss
    , ToPicture(..)
    , plotPictures
) where
import Graphics.Gloss

-----------------------------------------------------------------------------

type Point2   = (Float, Float)
type Point3   = (Float, Float, Float)

-----------------------------------------------------------------------------

class ToPicture a where
    toPicture :: a -> AxisScale -> AxisScale -> Picture


-- | Maps two axes onto a list of toPicture functions.
plotPictures :: [AxisScale -> AxisScale -> Picture] -- ^ A list of toPicture functions.
             -> AxisScale -- ^ Horizontal axis.
             -> AxisScale -- ^ Vertical axis.
             -> Picture
plotPictures cs xAxis yAxis = pictures $ map (\f -> f xAxis yAxis) cs


-----------------------------------------------------------------------------


data AxisScale = AxisScale { axisLength  :: Float -- ^ The length of the axis in pixels.
                           , axisMin     :: Float -- ^ The minimum value of the axis.
                           , axisMax     :: Float -- ^ The maximum value of the axis.
                           , axisScaleFn :: Float -> Float -- ^ A function to scale axis points to pixels.
                           }

data AxisScaleType = Linear | Log 
    deriving ( Eq, Show )


-----------------------------------------------------------------------------


-- | Creates an axis with a given length and range.
fixedAxisScale :: AxisScaleType -- ^ The type of axis scale.
               -> Float         -- ^ Axis length.
               -> Float         -- ^ Minimum axis value.
               -> Float         -- ^ Maximum axis value.
               -> AxisScale
fixedAxisScale t len min max | t == Linear = axis (linearScale cross min scale)
                             | t == Log    = axis (logScale len cross min max)
    where scale   = getAxisScale len min max
          cross   = getAxisCrossingPt len min max scale
          axis    = AxisScale len min max --scale



-- | Creates an axis with a given length, and automatically finds the range.
autoAxisScale :: AxisScaleType -- ^ The type of axis scale.
              -> Float         -- ^ Axis length.
              -> [Float]       -- ^ Points to be fitted on the axis.
              -> AxisScale
autoAxisScale t len pts = fixedAxisScale t len (minimum pts) (maximum pts)

-- | Same as 'autoScaleAxis', but always includes the origin.
autoAxisScale' :: AxisScaleType -> Float -> [Float] -> AxisScale
autoAxisScale' t len pts = fixedAxisScale t len (minimum [0, minimum pts])
                                                (maximum [0, maximum pts])

getAxisScale :: Float -> Float -> Float -> Float
getAxisScale len min max = len / (max - min)

getAxisCrossingPt :: Float -> Float -> Float -> Float -> Float
getAxisCrossingPt len min max scale | min > 0   = 0
                                    | max < 0   = len
                                    | otherwise = scale * abs min

-----------------------------------------------------------------------------


linearScale :: Float -> Float -> Float -> Float -> Float
linearScale zero min scale x = zero + shiftValue x * scale
    where shiftValue x' | min > 0   = x' - min
                        | otherwise = x'

          
logScale :: Float -> Float -> Float -> Float -> Float -> Float
logScale len zero min max x = zero + len * (log x - min') / (max' - min')
    where min' = log min
          max' = log max


-----------------------------------------------------------------------------

-- | Scales a list of 'Point2' values.
scalePoints :: AxisScale -> AxisScale -> [Point2] -> [Point2]
scalePoints xAxis yAxis pts = zip (map (axisScaleFn xAxis) xs) 
                                  (map (axisScaleFn yAxis) ys)
    where (xs, ys) = unzip pts

-- | Gets the pixel location of the 0 value on an axis.
getAxisZero :: AxisScale -> Float
getAxisZero axis | axisMin axis > 0 = 0
                 | axisMax axis < 0 = axisLength axis
                 | otherwise        = axisScaleFn axis 0

-----------------------------------------------------------------------------

