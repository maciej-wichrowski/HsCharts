-- | A renderable axis, with grid lines and scales.
--
--   The step sizes automatically adjusts to avoid the lines from getting too 
--   close to each other. If the step-size is 0, no lines will be drawn.
module Graphics.HsCharts.Components.Axis
(
      Axis
    , AxisPosition(..)
    -- * Constructors
    , axis
    , axis'
    , defaultHAxis
    , defaultVAxis
) where
import Graphics.Gloss
import Graphics.HsCharts.Types
import Graphics.HsCharts.Components.Grid
import Numeric

-----------------------------------------------------------------------------

data Axis = Axis { axisNumOffset     :: Float
                 , axisGridOffset    :: Float
                 , axisPosition      :: AxisPosition
                 , axisNumberStep    :: Float
                 , axisGridStepMajor :: Float
                 , axisGridStepMinor :: Float
                 }

data AxisPosition = AHorizontal -- ^ Horizontal axis; its position is determined by the vertical axis.
                  | AVertical   -- ^ Vertical axis; its position is determined by the horizontal axis.
                  | ALeft       -- ^ Vertical axis on the leftmost side of the chart.
                  | ARight      -- ^ Vertical axis on the rightmost side of the chart.
                  | ATop        -- ^ Horizontal axis at the top of the chart.
                  | ABottom     -- ^ Horizontal axis at the bottom of the chart.
    deriving ( Eq, Show )

axis :: AxisPosition -- ^ Position of the axis.
     -> Float        -- ^ Scale step size.
     -> Float        -- ^ Major grid lines step size.
     -> Float        -- ^ Minor grid lines step size.
     -> Axis
axis = axis' 0 0

axis' :: Float        -- ^ Scale offset.
      -> Float        -- ^ Grid lines offset.
      -> AxisPosition -- ^ Position of the axis.
      -> Float        -- ^ Scale step size.
      -> Float        -- ^ Major grid lines step size.
      -> Float        -- ^ Minor grid lines step size.
      -> Axis
axis' = Axis

-- | The default horizontal axis, with numbers and minor grid lines step size of 1.
defaultHAxis :: Axis
defaultHAxis = axis AHorizontal 1 0 1

-- | The default vertical axis, with numbers and minor grid lines step size of 1.
defaultVAxis :: Axis
defaultVAxis = axis AVertical 1 0 1

-----------------------------------------------------------------------------

instance ToPicture Axis where
    toPicture = axisToPicture

axisToPicture :: Axis -> AxisScale -> AxisScale -> Picture
axisToPicture a xAxis yAxis | pos == AHorizontal = hAxis a xAxis (getAxisZero yAxis)
                            | pos == AVertical   = vAxis a yAxis (getAxisZero xAxis)
                            | pos == ALeft       = vAxis a yAxis 0
                            | pos == ARight      = vAxis a yAxis (axisLength xAxis)
                            | pos == ABottom     = hAxis a xAxis 0
                            | pos == ATop        = hAxis a xAxis (axisLength yAxis)
    where pos = axisPosition a


hAxis :: Axis -> AxisScale -> Float -> Picture
hAxis a xAxis y = pictures
    [ line [ (0, y), (axisLength xAxis, y) ]
    , hGrid xAxis (axisGridStepMajor a) (axisGridOffset a) y 9
    , hGrid xAxis (axisGridStepMinor a) (axisGridOffset a) y 5
    , numOffset (axisPosition a) $
                hNum xAxis (axisNumberStep a) (axisNumOffset a) y
    ]

vAxis :: Axis -> AxisScale -> Float -> Picture
vAxis a yAxis x = pictures
    [ line [ (x, 0), (x, axisLength yAxis) ]
    , vGrid yAxis (axisGridStepMajor a) (axisGridOffset a) x 9
    , vGrid yAxis (axisGridStepMinor a) (axisGridOffset a) x 5
    , numOffset (axisPosition a) $
                vNum yAxis (axisNumberStep a) (axisNumOffset a) x
    ]

-----------------------------------------------------------------------------

hGrid :: AxisScale -> Float -> Float -> Float -> Float -> Picture
hGrid xAxis i offset y s = pictures $ map lineX $ gridPoints' offset xAxis i 6
    where lineX (_, x) = line [ (x, y + sizeHalf), (x, y - (sizeHalf + 1)) ]
          sizeHalf     = (s - 1) / 2

vGrid :: AxisScale -> Float -> Float -> Float -> Float -> Picture
vGrid yAxis i offset x s = pictures $ map lineY $ gridPoints' offset yAxis i 6
    where lineY (_, y) = line [ (x + sizeHalf + 1, y), (x - sizeHalf, y) ]
          sizeHalf     = (s - 1) / 2

-----------------------------------------------------------------------------

hNum :: AxisScale -> Float -> Float -> Float -> Picture
hNum xAxis i offset y = pictures $ map ptX $ gridPoints' offset xAxis i 12
    where 
        textSize   = 0.075
        ptX (i, x) = translate (x - 5) (y - 15) $ ptText i
        ptText i   = scale textSize textSize $ text $ showFFloat' 1 i

vNum :: AxisScale -> Float -> Float -> Float -> Picture
vNum yAxis i offset x = pictures $ map ptY $ gridPoints' offset yAxis i 12
    where 
        textSize   = 0.075
        ptY (i, y) = translate (x - 20) (y - 3) $ ptText i
        ptText i   = scale textSize textSize $ text $ showFFloat' 1 i

numOffset :: AxisPosition -> Picture -> Picture
numOffset ATop   = translate 0 20
numOffset ARight = translate 28 0
numOffset _      = id


-----------------------------------------------------------------------------


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
