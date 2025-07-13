{-# LANGUAGE OverloadedStrings #-}

import           Control.Monad         (unless, forM_)
import           Foreign               (nullPtr, sizeOf)
import           Foreign.Marshal.Array (withArray)
import           Graphics.Rendering.OpenGL as GL
import           Graphics.UI.GLUT      as GLUT
import           System.Exit           (exitSuccess)
import qualified Graphics.GL.Compatibility33 as Raw
import qualified Data.ByteString.Char8 as B

--------------------------------------------------------------------------------
-- PARAMETERS (tweak to taste)
--------------------------------------------------------------------------------
winWidth, winHeight :: Int
winWidth  = 800
winHeight = 600

hexRadius :: GLfloat
hexRadius = 0.06              -- measured in NDC units

gridCols, gridRows :: Int
gridCols = 20
gridRows = 15                -- enough to fill window

--------------------------------------------------------------------------------
-- MAIN
--------------------------------------------------------------------------------
main :: IO ()
main = do
  (_progName, _args) <- GLUT.getArgsAndInitialize
  initialDisplayMode $= [DoubleBuffered]
  _window <- GLUT.createWindow "Instanced Hex Grid (Haskell OpenGL)"
  windowSize $= Size (fromIntegral winWidth) (fromIntegral winHeight)

  -- OpenGL state
  clearColor $= Color4 1 1 1 1      -- white background
  lineWidth  $= 1.5

  -- Build GPU resources
  vao      <- genObjectName
  vertVbo  <- genObjectName
  instVbo  <- genObjectName
  prog     <- buildShaders

  bindVertexArrayObject $= Just vao
  buildHexagonVBO vertVbo
  buildInstanceVBO instVbo
  setAttribPointers vertVbo instVbo

  displayCallback $= display prog (fromIntegral (gridCols*gridRows))
  reshapeCallback $= Just reshape
  keyboardMouseCallback $= Just (\_ _ _ _ -> exitSuccess)

  GLUT.mainLoop

--------------------------------------------------------------------------------
-- SHADERS
--------------------------------------------------------------------------------
vertexShaderSrc, fragmentShaderSrc :: String
vertexShaderSrc = unlines
  [ "#version 330 core"
  , "layout(location = 0) in vec2 aPos;"      -- per‑vertex
  , "layout(location = 1) in vec2 aOffset;"   -- per‑instance
  , "void main()"
  , "{"
  , "    vec2 pos = aPos + aOffset;"
  , "    gl_Position = vec4(pos, 0.0, 1.0);"
  , "}"
  ]

fragmentShaderSrc = unlines
  [ "#version 330 core"
  , "out vec4 fragColor;"
  , "void main() {"
  , "    fragColor = vec4(0.0, 0.0, 0.0, 1.0);" -- black lines/fill
  , "}"
  ]

buildShaders :: IO Program
buildShaders = do
  vs <- createShader VertexShader
  fs <- createShader FragmentShader
  shaderSourceBS vs $= B.pack vertexShaderSrc
  shaderSourceBS fs $= B.pack fragmentShaderSrc
  compileShader vs
  compileShader fs
  okV <- get (compileStatus vs)
  okF <- get (compileStatus fs)
  unless (okV && okF) $ do
    logV <- get (shaderInfoLog vs)
    logF <- get (shaderInfoLog fs)
    putStrLn ("Shader error:\n" ++ logV ++ logF)
    exitSuccess
  prog <- createProgram
  attachShader prog vs
  attachShader prog fs
  linkProgram prog
  okP <- get (linkStatus prog)
  unless okP $ get (programInfoLog prog) >>= putStrLn >> exitSuccess
  currentProgram $= Just prog
  return prog

--------------------------------------------------------------------------------
-- GEOMETRY: one hexagon as triangle fan
--------------------------------------------------------------------------------
buildHexagonVBO :: BufferObject -> IO ()
buildHexagonVBO vbo = do
  let angles   = [0,60..300] :: [GLfloat]
      verts    = concatMap corner angles          -- 6 perimeter verts
      corner a = let rad   = a*pi/180
                     x     = hexRadius * cos rad
                     y     = hexRadius * sin rad
                 in [x,y]
      center   = [0,0]
      fanPairs = zip [0..5] (tail ([0..5] ++ [0]))      -- (i,i+1)
      tri i j  = center ++ drop (2*i) verts ++ take 2 (drop (2*j) verts)
      triangles = concat [ tri i j | (i,j) <- fanPairs] -- 6 triangles
      numFloats = length triangles
  bindBuffer ArrayBuffer $= Just vbo
  withArray triangles $ \ptr ->
      bufferData ArrayBuffer $= (fromIntegral (numFloats*sizeOf (0::GLfloat)), ptr, StaticDraw)

--------------------------------------------------------------------------------
-- INSTANCE OFFSETS
--------------------------------------------------------------------------------
buildInstanceVBO :: BufferObject -> IO ()
buildInstanceVBO vbo = do
  let dx = 1.5 * hexRadius
      dy = sqrt 3 * hexRadius
      offs = [ [ fromIntegral c*dx            + if odd r then 0.75*hexRadius else 0
               ,  fromIntegral r*(dy*0.5) ]
             | r <- [0..gridRows-1], c <- [0..gridCols-1] ]
      numFloats = length offs * 2
  bindBuffer ArrayBuffer $= Just vbo
  withArray (concat offs) $ \ptr ->
      bufferData ArrayBuffer $= (fromIntegral (numFloats*sizeOf (0::GLfloat)), ptr, StaticDraw)

----------------------------------------------------------------
-- ATTRIBUTE POINTERS  (vertex VBO = vboPos,  instance VBO = vboOff)
----------------------------------------------------------------
setAttribPointers :: BufferObject   -- ^ vertex‑position VBO (18 verts)
                  -> BufferObject   -- ^ per‑instance offset VBO
                  -> IO ()
setAttribPointers vboPos vboOff = do
  -- ---- location 0 : per‑vertex position (x,y) ----
  bindBuffer ArrayBuffer $= Just vboPos
  vertexAttribArray   (AttribLocation 0) $= Enabled
  vertexAttribPointer (AttribLocation 0) $=
      (ToFloat, VertexArrayDescriptor 2 Float 0 nullPtr)   -- 2 floats/vertex

  -- ---- location 1 : per‑instance offset (x,y) ----
  bindBuffer ArrayBuffer $= Just vboOff
  vertexAttribArray   (AttribLocation 1) $= Enabled
  vertexAttribPointer (AttribLocation 1) $=
      (ToFloat, VertexArrayDescriptor 2 Float 0 nullPtr)   -- 2 floats/instance
  Raw.glVertexAttribDivisor (fromIntegral (1 :: GLuint)) 1
  
  -- leave no ArrayBuffer bound (tidy, optional)
  bindBuffer ArrayBuffer $= Nothing

--------------------------------------------------------------------------------
-- DISPLAY
--------------------------------------------------------------------------------
display :: Program -> GLint -> DisplayCallback
display prog instanceCount = do
  clear [ColorBuffer]
  currentProgram $= Just prog
  drawArraysInstanced Triangles 0 18 instanceCount
  swapBuffers

reshape :: Size -> IO ()
reshape (Size w h) =
  viewport $= (Position 0 0, Size w h)
