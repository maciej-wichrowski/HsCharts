module Graphics.HsCharts.Shapes where
import Graphics.Gloss

-- | Creates an equilateral triangle (all sides same length) of the specified size.
eqTrianglePath :: Float -> Path
eqTrianglePath s = [(-s', -h), (s', -h), (0, h)]
    where h  = 0.5 * sqrt ((s ** 2) - (s' ** 2))
          s' = 0.5 * s

-- | Draws a solid equilateral triangle (all sides same length) of the specified size.
eqTriangleSolid :: Float -> Picture
eqTriangleSolid s = polygon $ eqTrianglePath s

-- | Draws a cross of the specified size.
cross :: Float -> Picture
cross s = pictures [ line [(-s', s'), (s', -s')], line [(-s', -s'), (s', s')] ]
    where s' = 0.5 * s

-- | Creates a diamond with the specified size and aspect.
diamondPath :: Float -> Float -> Path
diamondPath s a = [(0, s'), (-s' * a, 0), (0, -s'), (s' * a, 0)]
    where s' = 0.5 * s

-- | Draws a solid diamond with the specified size and aspect.    
diamondSolid :: Float -> Float -> Picture
diamondSolid s a = polygon $ diamondPath s a

-- | Concave polygon - not implemented yet.
concavePolygon :: [Point] -> Picture
concavePolygon pts = error "Not implemented yet"