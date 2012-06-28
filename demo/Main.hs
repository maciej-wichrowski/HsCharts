module Main where

import Graphics.HsCharts
import Graphics.Gloss

main :: IO ()
main = do
    let disp = InWindow "HsCharts Demo" (windowW, windowH) (50, 50)
    display disp white $ pictures [ q 0 0 scatterPlotPict
                                  , q 1 0 lineChartPict
                                  , q 2 0 bubbleChartPict
                                  , q 3 0 logScaleChart
                                  , q 0 1 barChartPict
                                  , q 1 1 areaChartPict
                                  , q 2 1 boxPlotPict
                                  , q 3 1 polarChartPict
                                  ]
    
windowW = 1050
windowH = 550
chartW  = 200
chartH  = 200
chartM  = 50

q x y  = translate dx dy
    where dx = (-(fromIntegral windowW / 2)) + (chartM + x * (chartW + chartM))
          dy = (fromIntegral windowH / 2) - chartH - chartM - (y * (chartH + chartM))

backColor      = makeColor 0.98 0.98 0.98 1
gridColor    = makeColor 0.8 0.8 0.8 1
gridAltColor = makeColor 0.8 0.8 0.8 0.4
pointColor   = makeColor 0.15 0.50 0.75 1
pointColor'  = makeColor 0.75 0.15 0.15 1
pointColor'' = makeColor 0.15 0.75 0.15 1
barColor     = makeColor 0.15 0.50 0.75 0.8
areaColor    = makeColor 0.15 0.50 0.75 0.4

-----------------------------------------------------------------------------

scatterPlotPict :: Picture
scatterPlotPict = plotPictures [ toPicture defaultBackground
                               , toPicture $ grid gridColor 2 2
                               , toPicture $ axis AHorizontal 2 2 1
                               , toPicture $ axis AVertical 2 2 1
                               , toPicture scatter
                               , toPicture scatter' ]
                               xAxis 
                               yAxis
    where xAxis    = fixedAxisScale Linear chartW 0 10
          yAxis    = fixedAxisScale Linear chartH 0 10
          scatter  = scatterPlot (color pointColor $ circleSolid 3) pts
          scatter' = scatterPlot (color pointColor' $ eqTriangleSolid 6) pts'
          pts      = [ (0.5, 0.7), (0.9, 1.3), (1.3, 2.1), (2.3, 3.9)
                     , (3.0, 0.9), (3.2, 4.1), (3.4, 3.8), (3.7, 2.6)
                     , (4.1, 5.2), (4.3, 1.2), (4.5, 2.3), (5.1, 2.5)
                     ]
          pts'     = [ (5.2, 6.1), (5.7, 6.5), (6.1, 8.9), (6.2, 8.4)
                     , (6.3, 7.5), (6.8, 8.2), (7.1, 5.5), (7.6, 8.9)
                     , (8.0, 9.5), (8.3, 6.6), (8.4, 7.5), (8.7, 5.6)
                     , (8.7, 9.6), (9.4, 8.7), (9.9, 9.9)
                     ]
                     
-----------------------------------------------------------------------------

lineChartPict :: Picture
lineChartPict = plotPictures [ toPicture defaultBackground
                             , toPicture $ grid gridColor 0 0.125
                             , toPicture $ axis AHorizontal 2 2 1
                             , toPicture $ axis AVertical 0.5 0.5 0.125
                             , toPicture $ lineChart pointColor (pts 1) 
                             , toPicture $ lineChart pointColor' (pts 1.5)
                             , toPicture $ lineChart pointColor'' (pts 3) ]
                             xAxis
                             yAxis
    where xAxis     = autoAxisScale Linear chartW xs
          yAxis     = fixedAxisScale Linear chartH 0 1
          xs        = [-6,-5.75..6]
          ys        = [sigmoid x | x <- xs]
          pts x     = zip xs (map ((+(0.5 - sigmoid 0 / x)) . (/x)) ys)
          sigmoid x = 1.0 / (1 + exp (-x))          

-----------------------------------------------------------------------------

bubbleChartPict :: Picture
bubbleChartPict = plotPictures [ toPicture defaultBackground
                               , toPicture defaultGrid 
                               , toPicture defaultHAxis
                               , toPicture defaultVAxis 
                               , toPicture $ defaultBubbleChart zAxis (zip3 xs ys zs) ]
                               xAxis
                               yAxis
    where xAxis = autoAxisScale' Linear chartW xs
          yAxis = autoAxisScale' Linear chartH xs
          zAxis = fixedAxisScale Linear 20 (-1.5) 1
          xs    = [1..10]
          ys    = [0.5 + maximum xs - (x + sin x) | x <- xs]
          zs    = map cos xs

-----------------------------------------------------------------------------

logScaleChart :: Picture
logScaleChart = pictures 
        [ toPicture defaultBackground xAxis yAxis
        , toPicture defaultHAxis xAxis yAxis
        , color pointColor $ toPicture (axis ALeft 100 0 100) xAxis yAxis
        , color pointColor' $ toPicture (axis ARight 10 0 10) xAxis yAxis'
        , toPicture (lineChart pointColor pts) xAxis yAxis
        , toPicture (lineChart pointColor' pts) xAxis yAxis'
        ]
    where xAxis     = fixedAxisScale Linear chartW 0 10
          yAxis     = fixedAxisScale Linear chartH 0 1024
          yAxis'    = fixedAxisScale Log chartH 1 1000
          xs        = [0.1, 0.25..10]
          ys        = map (2 **) xs
          pts       = zip xs ys

-----------------------------------------------------------------------------

barChartPict :: Picture
barChartPict = plotPictures [ toPicture defaultBackground
                            , toPicture $ grid gridColor 0 0.1 
                            , toPicture $ barAxis
                            , toPicture $ axis AVertical 0.1 0 0.1
                            , toPicture $ barChart barColor pts ]
                            xAxis
                            yAxis
     where xAxis = barAxisScale chartW xs
           yAxis = fixedAxisScale Linear chartH 0 1
           xs    = [1..18]
           ys    = [sin (x / 6) | x <- xs]
           pts   = zip xs ys
           
-----------------------------------------------------------------------------

areaChartPict :: Picture
areaChartPict = plotPictures [ toPicture defaultBackground
                             , toPicture $ grid gridColor 0 0.1 
                             , toPicture defaultHAxis
                             , toPicture $ axis AVertical 0.1 0 0.1
                             , toPicture $ areaChart areaColor pts
                             , toPicture $ lineChart pointColor pts ]
                             xAxis
                             yAxis
     where xAxis = autoAxisScale Linear chartW xs
           yAxis = fixedAxisScale Linear chartH (-1) 1
           xs    = [0,0.1..2 * pi]
           ys    = map sin xs
           pts   = zip xs ys

-----------------------------------------------------------------------------

boxPlotPict :: Picture
boxPlotPict = plotPictures [ toPicture defaultBackground
                           , toPicture $ grid gridColor 0 0.25 
                           , toPicture $ axis AHorizontal 0 0 1
                           , toPicture $ axis AVertical 0.5 0 0.5
                           , toPicture $ boxPlot barColor 25 allPts ]
                           xAxis
                           yAxis
    where xAxis  = fixedAxisScale Linear chartW 0.5 4.5
          yAxis  = fixedAxisScale Linear chartH (-1) 1
          pts    = zip (repeat 1) (map sin [0..10])
          pts'   = zip (repeat 2) (map cos [0, 0.1..2])
          pts''  = zip (repeat 3) (map cos [1, 1.1..3])
          pts''' = zip (repeat 4) (map cos [1, 1.1..2])
          allPts = pts ++ pts' ++ pts'' ++ pts'''

-----------------------------------------------------------------------------

polarChartPict :: Picture
polarChartPict = plotPictures [ toPicture defaultBackground
                              , toPicture $ polarGrid gridColor (pi / 4) (pi / 8)
                              , toPicture $ polarChart (color pointColor . line) pts
                              , toPicture $ polarChart (color pointColor' . line) pts' ]
                              aAxis
                              rAxis
     where aAxis = angularAxis chartW
           rAxis = radialAxis Linear chartH 0 (pi / 2)
           xs    = map ((2 * pi / 100) * ) [0..100]
           ys    = map (\x -> sin (4 * x) ) xs
           xs'   = map ((4 * pi / 100) * ) [0..100]
           ys'   = map (/ (2.6 * pi)) $ xs'
           pts   = zip xs ys
           pts'  = zip xs' ys'
           renderPointFn = pictures . map point
           point (x, y)  = translate x y $ circleSolid 2.5
