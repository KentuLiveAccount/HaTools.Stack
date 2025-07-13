import Graphics.Gloss
import Graphics.Gloss.Data.Vector
import Data.Fixed (mod')

-- Hexagon parameters
hexRadius :: Float
hexRadius = 30

-- Window dimensions
winWidth, winHeight :: Int
winWidth = 800
winHeight = 600

main :: IO ()
main = display
         (InWindow "Hex Grid (Gloss)" (winWidth, winHeight) (100, 100))
         black
         (Pictures $ map drawHexagonAt gridPositions)

-- Generate grid of hex centers
gridPositions :: [(Float, Float)]
gridPositions = [ hexPos col row
                | row <- [0..numRows]
                , col <- [0..numCols] ]
  where
    dx = sqrt 3 * hexRadius
    dy = 2 * hexRadius 
    numCols = floor (fromIntegral winWidth  / dx) + 1
    numRows = floor (fromIntegral winHeight / dy) + 1
    hexPos c r =
      let x = fromIntegral c * dx
          y = fromIntegral r * (3 * dy / 4) 
          x' = if even r then x else x + dx / 2
      in (x' - fromIntegral winWidth / 2, y - fromIntegral winHeight / 2)

-- Draw a hexagon centered at (x, y)
drawHexagonAt :: (Float, Float) -> Picture
drawHexagonAt (cx, cy) =
  --Translate cx cy $ Color black $ Polygon (hexagonPoints hexRadius)
  Translate cx cy $ Color white $ Line (hexagonPoints hexRadius ++ [head (hexagonPoints hexRadius)])


-- Create points for a regular hexagon centered at origin
hexagonPoints :: Float -> [Point]
hexagonPoints r = [ (r * cos (angle i), r * sin (angle i)) | i <- [0..5] ]
  where
    angle i = pi / 180 * (60 * fromIntegral i + 30)  -- point-topped hex

