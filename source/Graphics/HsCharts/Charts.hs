-- | A simple chart drawing module, using the Gloss drawing package.
module Graphics.HsCharts.Charts (
      Point2
    , Point3
    -- * Axes
    , AxisScaleType(..)
    , fixedScaleAxis
    , autoScaleAxis
    , autoScaleAxis'
    -- * Chart components
    , plotChartBackground
    , plotAxisX
    , plotAxisY
    , plotAxes
    , plotAxisScales
    , plotAxisScaleLabels
    , plotGrid
    -- * Chart types
    , plotBarChart
    , plotPointChart
    , plotPointChart'
    , plotLineChart
    , plotAreaChart
    , plotBubbleChart
    , plotBubbleChart'
    , boxPlot
    , boxPlots
    , plotPolarChart
    , angularAxis
    , radialAxis
    , plotPolarGrid
)
where
import Graphics.HsCharts.Helpers
import Graphics.Gloss
import Data.List


-----------------------------------------------------------------------------


type Point2   = (Float, Float)
type Point3   = (Float, Float, Float)


data Axis = Axis { axisLength :: Float
                 , axisMin :: Float
                 , axisMax :: Float
                 , axisCrossingPt :: Float
                 , axisScale :: Float
                 , axisScaleFn :: Float -> Float
                 }

data AxisScaleType = Linear | Log 
    deriving ( Eq, Show )


-----------------------------------------------------------------------------


-- | Creates an axis with a given length and range.
fixedScaleAxis :: AxisScaleType -- ^ The type of axis scale.
               -> Float         -- ^ Axis length.
               -> Float         -- ^ Minimum axis value.
               -> Float         -- ^ Maximum axis value.
               -> Axis
fixedScaleAxis t len min max | t == Linear = axis (linearScale cross min scale)
                             | t == Log    = axis (logScale len cross min max)
    where scale   = getAxisScale len min max
          cross   = getAxisCrossingPt len min max scale
          axis    = Axis len min max cross scale



-- | Creates an axis with a given length, and automatically finds the range.
autoScaleAxis :: AxisScaleType -- ^ The type of axis scale.
              -> Float         -- ^ Axis length.
              -> [Float]       -- ^ Points to be fitted on the axis.
              -> Axis
autoScaleAxis t len pts = fixedScaleAxis t len (minimum pts) (maximum pts)

-- | Same as 'autoScaleAxis', but always includes the origin.
autoScaleAxis' :: AxisScaleType -> Float -> [Float] -> Axis
autoScaleAxis' t len pts = fixedScaleAxis t len (minimum [0, minimum pts])
                                                (maximum [0, maximum pts])

getAxisScale len min max = len / (max - min)

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

                       
scalePoints :: Axis -> Axis -> [Point2] -> [Point2]
scalePoints xAxis yAxis pts = zip (map (axisScaleFn xAxis) xs) 
                                  (map (axisScaleFn yAxis) ys)
    where (xs, ys) = unzip pts

        
-----------------------------------------------------------------------------


-- | Renders the background area of a chart.
plotChartBackground :: Axis -> Axis -> Picture
plotChartBackground xAxis yAxis = polygon [(0, 0), (0, h), (w, h), (w, 0)]
    where w = axisLength xAxis
          h = axisLength yAxis


-----------------------------------------------------------------------------


-- | Renders the x-axis of a chart.
plotAxisX :: Axis -> Axis -> Picture
plotAxisX xAxis yAxis = line [ (0, axisCrossingPt yAxis)
                             , (axisLength xAxis, axisCrossingPt yAxis) ]

-- | Renders the y-axis of a chart.
plotAxisY :: Axis -> Axis -> Picture
plotAxisY xAxis yAxis = line [ (axisCrossingPt xAxis, 0)
                             , (axisCrossingPt xAxis, axisLength yAxis) ]

-- | Renders both axes of a chart.
plotAxes :: Axis -> Axis -> Picture
plotAxes xAxis yAxis = pictures [ plotAxisX xAxis yAxis, plotAxisY xAxis yAxis ]


-----------------------------------------------------------------------------


--Note: due to floating point precision, the steps may not increase by exactly i.
gridPoints :: Axis -> Float -> Float -> [(Float, Float)]
gridPoints _    0 _       = []
gridPoints axis i minDist = reduceGrid pts []
    where
        min      = axisMin axis
        max      = axisMax axis
        xs       = [min, min + i .. max]
        xsScaled = map (axisScaleFn axis) xs
        pts      = zip xs xsScaled
        reduceGrid []         ac                   = ac
        reduceGrid (_:[])     ac                   = ac
        reduceGrid (x1:x2:xs) ac | keepPoint x1 x2 = reduceGrid (x2:xs) (ac ++ [x1, x2])
                                 | otherwise       = reduceGrid (x1:xs) ac
        keepPoint (a, a') (b, b') = a >= min && b <= max && b' - a' >= minDist


-- | Renders the scale values for the chart's axes.
--
--   The step size automatically adjusts to avoid overlapping texts. 
--   If the step-size is 0, no values will be shown.
plotAxisScales :: Axis -> Axis -> (Float, Float) -> Picture
plotAxisScales xAxis yAxis (iX, iY) = pictures $ scalesX ++ scalesY
    where
        textSize   = 0.075
        xZero      = axisCrossingPt xAxis
        yZero      = axisCrossingPt yAxis
        scalesX    = map ptX $ gridPoints xAxis iX 12
        scalesY    = map ptY $ gridPoints yAxis iY 12
        ptX (i, x) = translate (x - 5) (yZero - 15) $ ptText i
        ptY (i, y) = translate (xZero - 20) (y - 3) $ ptText i
        ptText i   = scale textSize textSize $ text $ showFFloat' 1 i


-- | Renders given labels along the x-axis.
plotAxisScaleLabels :: Axis -> Axis -> [(Float, String)] -> Picture
plotAxisScaleLabels xAxis yAxis lbls = pictures labelsX
    where
        textSize        = 0.075
        xScale          = axisScale xAxis
        xZero           = axisCrossingPt xAxis
        yZero           = axisCrossingPt yAxis
        labelsX         = map ptX lbls
        ptX (x, l)      = translate (xZero + (x * xScale) - 5) (yZero - 15) $ scale textSize textSize $ text l


-- | Renders a grid on the background area of a chart.
--
--   The step size automatically adjusts to avoid the lines from getting too 
--   close to each other. If the step-size is 0, no lines will be drawn.
plotGrid :: Axis -> Axis -> (Float, Float) -> Picture
plotGrid xAxis yAxis (iX, iY) = pictures $ gridX ++ gridY
    where
        gridX        = map lineX $ gridPoints xAxis iX 6
        gridY        = map lineY $ gridPoints yAxis iY 6
        lineX (_, x) = line [ (x, 0), (x, axisLength yAxis) ]
        lineY (_, y) = line [ (0, y), (axisLength xAxis, y) ]


-----------------------------------------------------------------------------


-- | Plots a bar chart from a list of 2-dimensional values.
plotBarChart :: Axis -> Axis -> [Point2] -> Picture
plotBarChart xAxis yAxis pts = pictures $ map bar $ scalePoints xAxis yAxis pts
    where
        xScale     = axisScale xAxis
        yZero      = axisCrossingPt yAxis
        barWidth   = 0.8 * xScale
        barSpacing = 0.2 * xScale
        bar (x, y) = translate (x - (0.5 * (barWidth + barSpacing))) yZero $ 
                               rectangleUpperSolid barWidth (y - yZero)


-----------------------------------------------------------------------------


-- | Renders a scatter-plot from a list of 2-dimensional values.
plotPointChart :: Axis -> Axis -> [Point2] -> Picture
plotPointChart = plotPointChart' (rectangleSolid 2 2)

-- | Renders a scatter-plot with a custom point picture.
plotPointChart' :: Picture -> Axis -> Axis -> [Point2] -> Picture
plotPointChart' p xAxis yAxis pts = pictures $ map (\(x, y) -> translate x y p)
                                                   (scalePoints xAxis yAxis pts)


-----------------------------------------------------------------------------


-- | Plots a line chart from a list of 2-dimensional values.
plotLineChart :: Axis -> Axis -> [Point2] -> Picture
plotLineChart xAxis yAxis pts = line $ scalePoints xAxis yAxis pts


-----------------------------------------------------------------------------


-- | Plots an area chart from a list of 2-dimensional values, filling the area
--   between the x-axis and the y-values.
--
--   Note: The implementation could be optimized. Currently it creates a list of 
--   tri's, to avoid drawing issues with concave polygons, regardless of 
--   whether this is necessary or not.
plotAreaChart :: Axis -> Axis -> [Point2] -> Picture
plotAreaChart xAxis yAxis pts = pictures $ polies sortedPts
    where sortedPts                    = sort $ scalePoints xAxis yAxis pts
          yZero                        = axisCrossingPt yAxis
          polies []                    = []
          polies (_:[])                = []
          polies ((x1,y1):(x2,y2):pts) = polygon [ (x1, yZero), (x1, y1)
                                                 , (x2, y2), (x2, yZero)
                                                 ] : polies ((x2,y2):pts)


-----------------------------------------------------------------------------


-- | Plots a bubble-chart using a list of 3-dimensional values. The z-value 
--   and the z-axis range determine the size of the bubbles.
plotBubbleChart :: Axis -> Axis -> Axis -> [Point3] -> Picture
plotBubbleChart =
    plotBubbleChart' (\(_, _, z) -> pictures [ color (makeColor 0.18 0.81 0.26 0.5) (circleSolid z)
                                             , thickCircle z 1.5 ])

-- | Plots a bubble-chart using a custom function to draw the bubbles.
plotBubbleChart' :: (Point3 -> Picture) -> Axis -> Axis -> Axis -> [Point3] -> Picture
plotBubbleChart' p xAxis yAxis zAxis pts = pictures $ map plotPoint scaledPts
    where (xs, ys, zs)        = unzip3 pts
          scaledPts           = zip3 (map (axisScaleFn xAxis) xs)
                                     (map (axisScaleFn yAxis) ys)
                                     (map (axisScaleFn zAxis) zs)
          plotPoint (x, y, z) = translate x y $ p (x, y, z)


-----------------------------------------------------------------------------


-- | Plots a vertical boxplot from a list of 1-dimensional values.
boxPlot :: Float    -- ^ Box plot width
        -> Color    -- ^ Box fill color
        -> Axis     -- ^ Vertical axis
        -> [Float]  -- ^ Plot data
        -> Picture
boxPlot w c yAxis xs = 
    pictures [ line [((-0.25) * w, q0), (0.25 * w, q0)]
             , line [((-0.25) * w, q4), (0.25 * w, q4)]
             , line [(0, q0), (0, q1)]
             , translate 0 (q1 + 0.5 * boxH) $ pictures [ color c $ rectangleSolid w boxH
                                                        , rectangleWire w boxH]
             , line [(0, q3), (0, q4)]
             , line [((-0.5) * w, q2), (0.5 * w, q2)] ]
    where
        scaledXs             = sort $ map (axisScaleFn yAxis) xs
        (q0, q1, q2, q3, q4) = fiveNum scaledXs
        boxH                 = q3 - q1

-- | Renders vertical boxplots along the x-axis
boxPlots :: Axis     -- ^ Horizontal axis
         -> Axis     -- ^ Vertical axis
         -> Float    -- ^ Box plot width 
         -> Color    -- ^ Box fill color
         -> [Point2] -- ^ Plot data
         -> Picture
boxPlots xAxis yAxis w c pts = 
    pictures $ zipWith (\x ys -> translate x 0 $ boxPlot w c yAxis ys) xs yss
    where 
        groups = groupBy (\(x1, _) (x2, _) -> x1 == x2) $ sort pts
        xs     = map (axisScaleFn xAxis) $ map (fst . head) groups
        yss    = map (map snd) groups


-----------------------------------------------------------------------------

    
polarToCartesian (a, r) = (r * cos a, r * sin a)

-- | Renders a data series in a polar coordinate system.
plotPolarChart :: ([Point2] -> Picture) -- ^ Rendering function.
               -> Axis                  -- ^ Angular axis. Use 'angularAxis' function to construct an appropriate Axis.
               -> Axis                  -- ^ Radial axis. Use 'radialAxis' function to construct an appropriate Axis.
               -> [Point2]              -- ^ Plot data.
               -> Picture
plotPolarChart renderFn aAxis rAxis pts = renderFn cartesianPts
    where
        r               = axisLength rAxis / 2
        scaledPts       = scalePoints aAxis rAxis pts
        cartesianPts    = map (centerPt . polarToCartesian) scaledPts
        centerPt (x, y) = (x + r, y + r)

-- | Creates an Axis to be used as the angular axis in a polar chart.
angularAxis :: Float -- ^ Chart width.
            -> Axis
angularAxis len  = Axis len 0 (2 * pi) 0 1 id

-- | Creates an Axis to be used as the radial axis in a polar chart.
radialAxis :: AxisScaleType -> Float -> Float -> Float -> Axis
radialAxis t len min max = (fixedScaleAxis t (len / 2) min max) { axisLength = len }


-- | Renders a polar grid on the background area of a polar chart.
plotPolarGrid :: Axis -> Axis -> (Float, Float) -> Picture
plotPolarGrid aAxis rAxis (iA, iR) = pictures $ gridA ++ gridR
   where
        r               = axisLength rAxis / 2
        gridA           = map lineA $ gridPoints aAxis iA 0
        gridR           = map lineR $ gridPoints rAxis iR 0
        lineA (_, x)    = line [ (r, r), centerPt $ polarToCartesian (x, r) ]
        lineR (_, y)    = translate r r $ circle y
        centerPt (x, y) = (x + r, y + r)
