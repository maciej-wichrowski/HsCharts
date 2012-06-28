-- | Renders a grid on the background area of a chart.
--
--   The step size automatically adjusts to avoid the lines from getting too 
--   close to each other. If the step-size is 0, no lines will be drawn.
module Graphics.HsCharts.Components.Grid
(
      Grid
    -- * Constructors
    , grid
    , defaultGrid
    -- * Other functions
    , gridPoints
    , gridPoints'
) where
import Graphics.Gloss
import Graphics.HsCharts.Types

-----------------------------------------------------------------------------

data Grid = Grid { gridColor :: Color
                 , hStep :: Float
                 , vStep :: Float
                 }

-- | Grid constructor
grid :: Color -- ^ Grid color.
     -> Float -- ^ Horizontal grid size.
     -> Float -- ^ Vertical grid size.
     -> Grid
grid = Grid

-- | The default (light-grey) grid, with vertical and horizontal size 1.
defaultGrid :: Grid
defaultGrid = Grid (greyN 0.8) 1 1

-----------------------------------------------------------------------------

instance ToPicture Grid where
    toPicture g = gridToPicture (gridColor g) (hStep g) (vStep g)
    
gridToPicture :: Color -> Float -> Float -> AxisScale -> AxisScale -> Picture
gridToPicture c iX iY xAxis yAxis = color c $ pictures $ gridX ++ gridY
    where
        gridX        = map lineX $ gridPoints xAxis iX 6
        gridY        = map lineY $ gridPoints yAxis iY 6
        lineX (_, x) = line [ (x, 0), (x, axisLength yAxis) ]
        lineY (_, y) = line [ (0, y), (axisLength xAxis, y) ]

-----------------------------------------------------------------------------

-- | Generates a list of points on the grid.
gridPoints :: AxisScale        -- ^ Axis scale to generate grid points for.
           -> Float            -- ^ Preferred step size.
           -> Float            -- ^ Minimal distance in pixels between the points.
           -> [(Float, Float)] -- ^ A list of grid points (axis value, scaled value).
gridPoints = gridPoints' 0


gridPoints' :: Float            -- ^ Start offset
            -> AxisScale        -- ^ Axis scale to generate grid points for.
            -> Float            -- ^ Preferred step size.
            -> Float            -- ^ Minimal distance in pixels between the points.
            -> [(Float, Float)] -- ^ A list of grid points (axis value, scaled value).
gridPoints' _ _    0 _       = []
gridPoints' o axis i minDist = reduceGrid pts []
    where
        min      = axisMin axis
        max      = axisMax axis
        xs       = [min + o, min + o + i .. max]
        xsScaled = map (axisScaleFn axis) xs
        pts      = zip xs xsScaled
        reduceGrid []         ac                   = ac
        reduceGrid (_:[])     ac                   = ac
        reduceGrid (x1:x2:xs) ac | keepPoint x1 x2 = reduceGrid (x2:xs) (ac ++ [x1, x2])
                                 | otherwise       = reduceGrid (x1:xs) ac
        keepPoint (a, a') (b, b') = a >= min && b <= max && b' - a' >= minDist

