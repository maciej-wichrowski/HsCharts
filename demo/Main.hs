module Main where

import Graphics.HsCharts
import Graphics.Gloss

main :: IO ()
main = do
    let disp = InWindow "HsCharts Demo" (windowW, windowH) (50, 50)
    display disp white $ pictures [ q 0 0 pointChart
                                  , q 1 0 lineChart
                                  , q 2 0 bubbleChart
                                  , q 3 0 logScaleChart
                                  , q 0 1 barChart
                                  , q 1 1 areaChart
                                  , q 2 1 Main.boxPlot
                                  , q 3 1 polarChart
                                  ]
    
windowW = 1050
windowH = 550
chartW  = 200
chartH  = 200
chartM  = 50

q x y  = translate dx dy
    where dx = (-(fromIntegral windowW / 2)) + (chartM + x * (chartW + chartM))
          dy = (fromIntegral windowH / 2) - chartH - chartM - (y * (chartH + chartM))

bgColor      = makeColor 0.98 0.98 0.98 1
gridColor    = makeColor 0.8 0.8 0.8 1
gridAltColor = makeColor 0.8 0.8 0.8 0.4
pointColor   = makeColor 0.15 0.50 0.75 1
pointColor'  = makeColor 0.75 0.15 0.15 1
pointColor'' = makeColor 0.15 0.75 0.15 1
barColor     = makeColor 0.15 0.50 0.75 0.8
areaColor    = makeColor 0.15 0.50 0.75 0.4

-----------------------------------------------------------------------------
             
lineChart :: Picture
lineChart = 
    pictures [ color bgColor $ plotChartBackground xAxis yAxis
             , color gridColor $ plotGrid xAxis yAxis (0, 0.125)
             , plotAxes xAxis yAxis
             , plotAxisScales xAxis yAxis (2, 0.5)
             , line 1 pointColor
             , line 1.5 pointColor'
             , line 3 pointColor''
             ]
    where xAxis     = autoScaleAxis Linear chartW xs
          yAxis     = fixedScaleAxis Linear chartH 0 1
          xs        = [-6,-5.75..6]
          ys        = [sigmoid x | x <- xs]
          pts x     = zip xs (map ((+(0.5 - sigmoid 0 / x)) . (/x)) ys)
          line x c  = color c $ plotLineChart xAxis yAxis (pts x)
          sigmoid x = 1.0 / (1 + exp (-x))
          
          
-----------------------------------------------------------------------------

boxPlot :: Picture
boxPlot = 
    pictures [ color bgColor $ plotChartBackground xAxis yAxis
             , color gridColor $ plotGrid xAxis yAxis (0, 0.25)
             , plotAxes xAxis yAxis
             , plotAxisScales xAxis yAxis (0, 0.5)
             , boxPlots xAxis yAxis 25 barColor (pts ++ pts' ++ pts'' ++ pts''')
             ]
    where xAxis  = fixedScaleAxis Linear chartW 0.5 4.5
          yAxis  = fixedScaleAxis Linear chartH (-1) 1
          pts    = zip (repeat 1) (map sin [0..10])
          pts'   = zip (repeat 2) (map cos [0, 0.1..2])
          pts''  = zip (repeat 3) (map cos [1, 1.1..3])
          pts''' = zip (repeat 4) (map cos [1, 1.1..2])

-----------------------------------------------------------------------------

pointChart :: Picture
pointChart =
    pictures [ color bgColor $ plotChartBackground xAxis yAxis
             , color gridColor $ plotGrid xAxis yAxis (1, 1)
             , plotAxes xAxis yAxis
             , plotAxisScales xAxis yAxis (1, 1)
             , color pointColor $ plotPointChart' (circleSolid 3) xAxis yAxis pts
             , color pointColor' $ plotPointChart' (eqTriangleSolid 6) xAxis yAxis pts'
             ]
    where xAxis = fixedScaleAxis Linear chartW 0 10
          yAxis = fixedScaleAxis Linear chartH 0 10
          pts   = [ (0.5, 0.7), (0.9, 1.3), (1.3, 2.1), (2.3, 3.9)
                  , (3.0, 0.9), (3.2, 4.1), (3.4, 3.8), (3.7, 2.6)
                  , (4.1, 5.2), (4.3, 1.2), (4.5, 2.3), (5.1, 2.5)
                  ]
          pts'  = [ (5.2, 6.1), (5.7, 6.5), (6.1, 8.9), (6.2, 8.4)
                  , (6.3, 7.5), (6.8, 8.2), (7.1, 5.5), (7.6, 8.9)
                  , (8.0, 9.5), (8.3, 6.6), (8.4, 7.5), (8.7, 5.6)
                  , (8.7, 9.6), (9.4, 8.7), (9.9, 9.9)
                  ]
                  
-----------------------------------------------------------------------------

bubbleChart :: Picture
bubbleChart =
    pictures [ color bgColor $ plotChartBackground xAxis yAxis
             , color gridColor $ plotGrid xAxis yAxis (1, 1)
             , plotAxes xAxis yAxis
             , plotAxisScales xAxis yAxis (1, 1)
             , plotBubbleChart xAxis yAxis zAxis $ zip3 xs ys zs
             ]
    where xAxis = autoScaleAxis' Linear chartW xs
          yAxis = autoScaleAxis' Linear chartH xs
          zAxis = fixedScaleAxis Linear 20 (-1.5) 1
          xs    = [1..10]
          ys    = [0.5 + maximum xs - (x + sin x) | x <- xs]
          zs    = map cos xs

-----------------------------------------------------------------------------

barChart :: Picture
barChart = 
    pictures [ color bgColor $ plotChartBackground xAxis yAxis
             , color gridColor $ plotGrid xAxis yAxis (0, 0.1)
             , plotAxes xAxis yAxis
             , plotAxisScales xAxis yAxis (1, 0.1)
             , color barColor $ plotBarChart xAxis yAxis pts
             ]
     where xAxis = autoScaleAxis Linear chartW xs
           yAxis = fixedScaleAxis Linear chartH 0 1
           xs    = [0..18]
           ys    = [sin (x / 6) | x <- xs]
           pts   = zip xs ys
           
-----------------------------------------------------------------------------

areaChart :: Picture
areaChart = 
    pictures [ color bgColor $ plotChartBackground xAxis yAxis
             , color gridColor $ plotGrid xAxis yAxis (0, 0.1)
             , plotAxes xAxis yAxis
             , plotAxisScales xAxis yAxis (1, 0.2)
             , color areaColor $ plotAreaChart xAxis yAxis pts
             , color pointColor $ plotLineChart xAxis yAxis pts
             ]
     where xAxis = autoScaleAxis Linear chartW xs
           yAxis = fixedScaleAxis Linear chartH (-1) 1
           xs    = [0,0.1..2 * pi]
           ys    = map sin xs
           pts   = zip xs ys
           
-----------------------------------------------------------------------------

polarChart :: Picture
polarChart =
    pictures [ color bgColor $ plotChartBackground tAxis rAxis
             , color gridColor $ plotPolarGrid tAxis rAxis (pi / 4, pi / 8)
             , color pointColor $ plotPolarChart line tAxis rAxis pts
             , color pointColor' $ plotPolarChart line tAxis rAxis pts'
             ]
     where tAxis = angularAxis chartW
           rAxis = radialAxis Linear chartH 0 (pi / 2)
           xs    = map ((2 * pi / 100) * ) [0..100]
           ys    = map (\x -> sin (4 * x) ) xs
           xs'   = map ((4 * pi / 100) * ) [0..100]
           ys'   = map (/ (2.6 * pi)) $ xs'
           pts   = zip xs ys
           pts'  = zip xs' ys'
           renderPointFn = pictures . map point
           point (x, y)  = translate x y $ circleSolid 2.5

-----------------------------------------------------------------------------

logScaleChart :: Picture
logScaleChart =
    pictures [ color bgColor $ plotChartBackground xAxis yAxis
             , plotAxes xAxis yAxis
             , color pointColor $ plotAxisScales xAxis yAxis (1, 10)
             , translate (chartW + 25) 0 $ color pointColor' 
                                         $ plotAxisScales xAxis yAxis' (0, 10)
             , color pointColor $ plotLineChart xAxis yAxis $ zip xs ys
             , color pointColor' $ plotLineChart xAxis yAxis' $ zip xs ys
             ]
    where xAxis     = fixedScaleAxis Linear chartW 0 10
          yAxis     = fixedScaleAxis Linear chartH 0 1024
          yAxis'    = fixedScaleAxis Log chartH 1 1000
          xs        = [0.1, 0.25..10]
          ys        = map (2 **) xs
