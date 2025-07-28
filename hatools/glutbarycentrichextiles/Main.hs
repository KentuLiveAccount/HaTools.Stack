-- A GLUT-based Haskell program using raw OpenGL bindings (Graphics.GL)
-- to render instanced hexagon outlines using procedural barycentric coordinates

{-# LANGUAGE OverloadedStrings #-}

import Graphics.UI.GLUT hiding (compileShader)
import Graphics.GL
import Foreign
import Foreign.C.String
import Foreign.Marshal.Array
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS

-- Window size and hexagon tiling config
winW, winH :: Int
winW = 600
winH = 600
radius = 0.15 :: GLfloat
cols = 20
rows = 20

data ShaderType = Vertex | Fragment

main :: IO ()
main = do
  (_progName, _) <- getArgsAndInitialize
  initialDisplayMode $= [DoubleBuffered]
  initialWindowSize $= Size (fromIntegral winW) (fromIntegral winH)
  _ <- createWindow "Hexagons with Barycentric"

  renderer <- get renderer
  version  <- get glVersion
  putStrLn $ "Renderer: " ++ renderer
  putStrLn $ "OpenGL Version: " ++ version

  prog <- buildShaderProgram vertexSrc fragmentSrc

  vao <- alloca $ \ptr -> glGenVertexArrays 1 ptr >> peek ptr
  glBindVertexArray vao

  -- Vertex VBO: 6 vertices (hexagon) with barycentric coordinates
  let verts = concatMap (\i ->
              let a = fromIntegral i * pi / 3
                  x = cos a * radius
                  y = sin a * radius
                  bary = baryCoords i
              in [x, y] ++ bary) [0..5]

  vertexVBO <- alloca $ \ptr -> glGenBuffers 1 ptr >> peek ptr
  glBindBuffer GL_ARRAY_BUFFER vertexVBO
  withArray verts $ \ptr ->
    glBufferData GL_ARRAY_BUFFER (fromIntegral $ length verts * sizeOf (0 :: GLfloat)) (ptr :: Ptr GLfloat) GL_STATIC_DRAW

  let stride = 5 * sizeOf (0 :: GLfloat)
  glVertexAttribPointer 0 2 GL_FLOAT GL_FALSE (fromIntegral stride) nullPtr
  glEnableVertexAttribArray 0
  glVertexAttribPointer 1 3 GL_FLOAT GL_FALSE (fromIntegral stride) (plusPtr nullPtr (2 * sizeOf (0 :: GLfloat)))
  glEnableVertexAttribArray 1

  -- Instance offsets
  let positions = [
        let x = fromIntegral c * 1.5 * radius
            y = fromIntegral r * sqrt 3 * radius + if odd c then sqrt 3 * radius / 2 else 0
        in [x - fromIntegral winW / 2 * scaleX, y - fromIntegral winH / 2 * scaleY]
        | c <- [0..cols-1], r <- [0..rows-1] ]
      scaleX = 2 / fromIntegral winW
      scaleY = 2 / fromIntegral winH
      flatPos = concat positions

  instanceVBO <- alloca $ \ptr -> glGenBuffers 1 ptr >> peek ptr
  glBindBuffer GL_ARRAY_BUFFER instanceVBO
  withArray flatPos $ \ptr ->
    glBufferData GL_ARRAY_BUFFER (fromIntegral $ length flatPos * sizeOf (0 :: GLfloat)) ptr GL_STATIC_DRAW

  glVertexAttribPointer 2 2 GL_FLOAT GL_FALSE 0 nullPtr
  glEnableVertexAttribArray 2
  glVertexAttribDivisor 2 1  -- Advance per instance

  displayCallback $= do
    glClearColor 1 1 1 1
    glClear GL_COLOR_BUFFER_BIT

    glUseProgram prog
    glBindVertexArray vao
    glDrawArraysInstanced GL_LINE_LOOP 0 6 (fromIntegral $ length positions)

    swapBuffers

  mainLoop

-- Assign barycentric coordinates for edge highlighting (for wireframe)
baryCoords :: Int -> [GLfloat]
baryCoords i = case i `mod` 3 of
  0 -> [1,0,0]
  1 -> [0,1,0]
  _ -> [0,0,1]

-- Shader sources
vertexSrc, fragmentSrc :: ByteString
vertexSrc = BS.unlines
  [ "#version 330 core"
  , "layout(location = 0) in vec2 position;"
  , "layout(location = 1) in vec3 bary;"
  , "layout(location = 2) in vec2 offset;"
  , "out vec3 baryCoord;"
  , "void main() {"
  , "  baryCoord = bary;"
  , "  gl_Position = vec4(position + offset, 0.0, 1.0);"
  , "}"
  ]

fragmentSrc = BS.unlines
  [ "#version 330 core"
  , "in vec3 baryCoord;"
  , "out vec4 fragColor;"
  , "void main() {"
  , "  float edge = min(min(baryCoord.x, baryCoord.y), baryCoord.z);"
  , "  float d = fwidth(edge);"
  , "  float alpha = smoothstep(0.0, d, edge);"
  , "  fragColor = vec4(vec3(0.0), 1.0 - alpha);"
  , "}"
  ]

-- Shader compilation utilities
buildShaderProgram :: ByteString -> ByteString -> IO GLuint
buildShaderProgram vs fs = do
  vshader <- compileShader vs GL_VERTEX_SHADER
  fshader <- compileShader fs GL_FRAGMENT_SHADER
  prog <- glCreateProgram
  glAttachShader prog vshader
  glAttachShader prog fshader
  glLinkProgram prog
  checkLinkStatus prog
  return prog

compileShader :: ByteString -> GLenum -> IO GLuint
compileShader src shaderType = do
  shader <- glCreateShader shaderType
  BS.useAsCString src $ \ptr -> with ptr $ \pptr ->
    glShaderSource shader 1 pptr nullPtr
  glCompileShader shader
  checkCompileStatus shader
  return shader

checkCompileStatus :: GLuint -> IO ()
checkCompileStatus shader = alloca $ \ptr -> do
  glGetShaderiv shader GL_COMPILE_STATUS ptr
  status <- peek ptr
  if status == fromIntegral GL_FALSE
    then do
      alloca $ \logLenPtr -> do
        glGetShaderiv shader GL_INFO_LOG_LENGTH logLenPtr
        logLen <- peek logLenPtr
        allocaBytes (fromIntegral logLen) $ \logPtr -> do
          glGetShaderInfoLog shader logLen nullPtr logPtr
          peekCString logPtr >>= putStrLn
      fail "Shader compilation failed"
    else return ()

checkLinkStatus :: GLuint -> IO ()
checkLinkStatus prog = alloca $ \ptr -> do
  glGetProgramiv prog GL_LINK_STATUS ptr
  status <- peek ptr
  if status == fromIntegral GL_FALSE
    then do
      alloca $ \logLenPtr -> do
        glGetProgramiv prog GL_INFO_LOG_LENGTH logLenPtr
        logLen <- peek logLenPtr
        allocaBytes (fromIntegral logLen) $ \logPtr -> do
          glGetProgramInfoLog prog logLen nullPtr logPtr
          peekCString logPtr >>= putStrLn
      fail "Program linking failed"
    else return ()