{-# LANGUAGE OverloadedStrings #-}

-- HexGridOutline.hs  •  stack ghc -- HexGridOutline.hs && ./HexGridOutline
-- cabal users: cabal install OpenGL GLUT && runghc HexGridOutline.hs

import           Control.Monad                 (unless, forM_)
import           Foreign                       (nullPtr, sizeOf)
import           Foreign.Marshal.Array         (withArray)
import           Graphics.Rendering.OpenGL     as GL
import           Graphics.UI.GLUT              as GLUT
import           System.Exit                   (exitSuccess)
import qualified Graphics.GL.Compatibility33 as Raw
import qualified Data.ByteString.Char8 as B

--------------------------------------------------------------------------------
-- Tunables
--------------------------------------------------------------------------------
winW, winH :: Int
winW = 800
winH = 800

radius :: GLfloat      -- centre‑to‑corner distance in NDC (−1..1)
radius = 0.06

cols, rows :: Int      -- how many hexes across & down
cols = 20
rows = 15

--------------------------------------------------------------------------------
main :: IO ()
main = do
  (_prog, _args) <- GLUT.getArgsAndInitialize
  initialDisplayMode $= [DoubleBuffered]
  _win <- createWindow "Instanced Hex Outlines"
  windowSize        $= Size (fromIntegral winW) (fromIntegral winH)
  clearColor        $= Color4 1 1 1 1          -- white background
  lineWidth         $= 1.2

  putStrLn "hello!!"

  vao      <- genObjectName
  vboVerts <- genObjectName
  vboInst  <- genObjectName
  prog     <- buildProgram

  bindVertexArrayObject $= Just vao
  buildHexOutlineVBO vboVerts
  print verts
  buildInstanceVBO    vboInst
  setAttribPointers vboVerts vboInst

  let instCount = fromIntegral (cols*rows)
  print instCount
  displayCallback  $= display prog instCount
  reshapeCallback  $= Just (\(Size w h) -> viewport $= (Position 0 0, Size w h))
  keyboardMouseCallback $= Just (\_ _ _ _ -> exitSuccess)

  mainLoop

--------------------------------------------------------------------------------
-- Shader compilation / program link
--------------------------------------------------------------------------------
vertSrc, fragSrc :: String
vertSrc = unlines
  [ "#version 330 core"
  , "layout(location=0) in vec2 aPos;"        -- per‑vertex corner
  , "layout(location=1) in vec2 aOffset;"     -- per‑instance centre
  , "void main(){"
  , "  vec2 p = aPos + aOffset;"
  , "  gl_Position = vec4(p, 0.0, 1.0);"
  , "}"
  ]
fragSrc = unlines
  [ "#version 330 core"
  , "out vec4 frag;"
  , "void main(){ frag = vec4(0.0,0.0,0.0,1.0); }" -- black outline
  ]

buildProgram :: IO Program
buildProgram = do
  vs <- createShader VertexShader
  fs <- createShader FragmentShader
  shaderSourceBS vs $= B.pack vertSrc
  shaderSourceBS fs $= B.pack fragSrc
  compileShader vs
  compileShader fs
  okV <- get (compileStatus vs)
  okF <- get (compileStatus fs)
  unless (okV && okF) $
    get (shaderInfoLog vs) >>= putStrLn >> get (shaderInfoLog fs) >>= putStrLn >> exitSuccess
  p <- createProgram
  attachShader p vs >> attachShader p fs
  linkProgram p
  okP <- get (linkStatus p)
  unless okP $ get (programInfoLog p) >>= putStrLn >> exitSuccess
  currentProgram $= Just p
  return p

--------------------------------------------------------------------------------
-- VBO containing ONE hex outline (6 corners, counter‑clockwise)
--------------------------------------------------------------------------------
verts = concatMap corner [30,90..330]  -- start at 30° for flat top
  where
    corner a = let rad = a*pi/180 in [radius*cos rad, radius*sin rad] :: [GLfloat]

buildHexOutlineVBO :: BufferObject -> IO ()
buildHexOutlineVBO vbo = do
  let 
      nBytes    = length verts * sizeOf (0::GLfloat)
  bindBuffer ArrayBuffer $= Just vbo
  withArray verts $ \ptr -> bufferData ArrayBuffer $= (fromIntegral nBytes, ptr, StaticDraw)

--------------------------------------------------------------------------------
-- Instance offset VBO (centre positions for every hex)
--------------------------------------------------------------------------------
buildInstanceVBO :: BufferObject -> IO ()
buildInstanceVBO vbo = do
  let dx = (sqrt 3) * radius
      dy = 3 * radius / 2

      winW' = fromIntegral winW / 2    -- half width in pixels
      winH' = fromIntegral winH / 2    -- half height in pixels

      positions =
        [ let x = fromIntegral c * dx + (if odd r then dx/2 else 0)
              y = fromIntegral r * dy
              -- xNDC = (x - winW') / (winW' * 2)  -- convert to NDC: [-1 .. +1]
              -- yNDC = (y - winH') / (winH' * 2)
              xNDC = x - 1
              yNDC = y - 0.5
          in [xNDC, yNDC]
        | c <- [0 .. cols-1]
        , r <- [0 .. rows-1]
        ]

      nBytes = length positions * 2 * sizeOf (0 :: GLfloat)

  print positions
  print $ length positions
  bindBuffer ArrayBuffer $= Just vbo
  withArray (concat positions) $ \ptr ->
    bufferData ArrayBuffer $= (fromIntegral nBytes, ptr, StaticDraw)

--------------------------------------------------------------------------------
-- Attribute setup
--------------------------------------------------------------------------------
setAttribPointers :: BufferObject -> BufferObject -> IO ()
setAttribPointers vboVerts vboInst = do
  -- location 0: per‑vertex hex corner
  bindBuffer ArrayBuffer $= Just vboVerts
  vertexAttribArray   (AttribLocation 0) $= Enabled
  vertexAttribPointer (AttribLocation 0) $=
      (ToFloat, VertexArrayDescriptor 2 Float 0 nullPtr)

  -- location 1: per‑instance offset
  bindBuffer ArrayBuffer $= Just vboInst
  vertexAttribArray   (AttribLocation 1) $= Enabled
  vertexAttribPointer (AttribLocation 1) $=
      (ToFloat, VertexArrayDescriptor 2 Float 0 nullPtr)
  Raw.glVertexAttribDivisor 1 1
--  Raw.glVertexAttribDivisor (AttribLocation 1) $= 1  -- advance per instance

  bindBuffer ArrayBuffer $= Nothing

--------------------------------------------------------------------------------
-- Display: clear, draw instanced outlines, swap
--------------------------------------------------------------------------------
display :: Program -> GLint -> DisplayCallback
display prog instCount = do
  clear [ColorBuffer]
  currentProgram $= Just prog
  drawArraysInstanced LineLoop 0 6 instCount
  swapBuffers
  get errors >>= print
