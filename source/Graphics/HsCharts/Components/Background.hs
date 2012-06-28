module Graphics.HsCharts.Components.Background
(
      Background
    -- * Constructors
    , background
    , defaultBackground
) where
import Graphics.Gloss
import Graphics.HsCharts.Types

-----------------------------------------------------------------------------

data Background = Background { bgColor :: Color }

-- | Creates a background with the specified color.
background :: Color -> Background
background = Background

-- | Creates a default (light-grey) background.
defaultBackground :: Background
defaultBackground = background (greyN 0.98)

-----------------------------------------------------------------------------

instance ToPicture Background where
    toPicture = bgToPicture
    

bgToPicture :: Background -> AxisScale -> AxisScale -> Picture
bgToPicture bg xAxis yAxis = color (bgColor bg) $ polygon [ (0, 0), (0, h)
                                                          , (w, h), (w, 0)]
    where
        w = axisLength xAxis
        h = axisLength yAxis