{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}

import Graphics.UI.GLUT
import Graphics.GL
import Foreign
import Foreign.C.String
import Foreign.Marshal.Array
import Control.Monad (when)

-- Two equilateral triangles (covering a parallelogram)
-- Each vertex: x, y, isCorner (0 or 1), barycentric (bx, by, bz)
-- 0.8660254 == sqrt(3)/2
vertices :: [GLfloat]
vertices =
  [ 0,    0,    1,  1,0,0
  , 1,    0,    1,  0,1,0
  , 0.5,  0.866,1,  0,0,1

  , 1,    0,    0,  1,0,0
  , 1.5,  0.866,0,  0,1,0
  , 0.5,  0.866,0,  0,0,1 ]

main :: IO ()
main = do
  (_progName, _args) <- getArgsAndInitialize
  initialDisplayMode $= [DoubleBuffered, RGBAMode]
  _window <- createWindow "Hex Grid via 2 Triangles"
  program <- initShaders

  vao <- alloca $ \p -> glGenVertexArrays 1 p >> peek p
  glBindVertexArray vao

  vbo <- alloca $ \p -> glGenBuffers 1 p >> peek p
  glBindBuffer GL_ARRAY_BUFFER vbo
  withArray vertices $ \ptr ->
    glBufferData GL_ARRAY_BUFFER (fromIntegral (length vertices * sizeOf (0::GLfloat))) ptr GL_STATIC_DRAW

  let stride = fromIntegral (6 * sizeOf (0::GLfloat))
  glVertexAttribPointer 0 2 GL_FLOAT GL_FALSE stride nullPtr
  glEnableVertexAttribArray 0

  glVertexAttribPointer 1 1 GL_FLOAT GL_FALSE stride (plusPtr nullPtr (2*sizeOf(0::GLfloat)))
  glEnableVertexAttribArray 1

  glVertexAttribPointer 2 3 GL_FLOAT GL_FALSE stride (plusPtr nullPtr (3*sizeOf(0::GLfloat)))
  glEnableVertexAttribArray 2

  displayCallback $= display program vao
  mainLoop

initShaders :: IO GLuint
initShaders = do
  vshader <- glCreateShader GL_VERTEX_SHADER
  withCString vertexSrc $ \ptr -> with ptr $ \pptr -> glShaderSource vshader 1 pptr nullPtr
  glCompileShader vshader

  fshader <- glCreateShader GL_FRAGMENT_SHADER
  withCString fragmentSrc $ \ptr -> with ptr $ \pptr -> glShaderSource fshader 1 pptr nullPtr
  glCompileShader fshader

  prog <- glCreateProgram
  glAttachShader prog vshader
  glAttachShader prog fshader
  glLinkProgram prog
  return prog

vertexSrc :: String
vertexSrc = unlines
  ["#version 330 core",
   "layout(location = 0) in vec2 position;",
   "layout(location = 1) in float isCorner;",
   "layout(location = 2) in vec3 bary;",
   "uniform int uCols;",
   "uniform int uRows;",
   "out vec3 vBary;",
   "flat out int vCorner;",
   "void main() {",
   " int col = gl_InstanceID % uCols;",
   " int row = gl_InstanceID / uCols;",
   " float xoffset = 0;",
   " if (row % 2 == 1) { xoffset = 0.5;}",
   " vec2 offset = vec2(float(col) - xoffset, float(row) * 0.866);",
   " vec2 gridSize = vec2(float(uCols), float(uRows) * 0.866);",
   " vec2 pos = ((position + offset) / gridSize) * 2.0 - 1.0;",
   " vBary = bary;",
   " vCorner = int(isCorner);",
   " gl_Position = vec4(pos, 0.0, 1.0);",
   "}"]

--    "  float edgeDist = min(min(abs(vBary.x - 0.5), abs(vBary.y - 0.5)), abs(vBary.z - (sqrt(3)/4)));",

fragmentSrc1 :: String
fragmentSrc1 = unlines
  ["#version 330 core",
   "in vec3 vBary;",
   "flat in int vCorner;",
   "out vec4 fragColor;",
   "void main() {",
   " if (vCorner == 0) {",
   "  fragColor = vec4(0, 0, 1, 1); // blank triangle",
   " } else if (vBary.y < (1/3.0)) {",
   "  fragColor = vec4(0); // blank triangle",
   " } else {",
   "  fragColor = vec4(0,vBary.y,0,1);",
   " }",
   "}"]

{-
   "  if (vBary.x > (1/(1 + 1.8))) { edgeDist = abs (vBary.y - vBary.z);}",
   "  else if (vBary.y > (1/(1 + 1.8))) { edgeDist = abs (vBary.x - vBary.z);}",
   "  else if (vBary.z > (1/(1 + 1.8))){ edgeDist = abs (vBary.x - vBary.y);}",
-}

fragmentSrc :: String
fragmentSrc = unlines
  ["#version 330 core",
   "in vec3 vBary;",
   "flat in int vCorner;",
   "out vec4 fragColor;",
   "void main() {",
   " if (vCorner == 0) {",
   "  fragColor = vec4(1.0); // blank triangle",
   " } else {",
   "  float edgeDist0 = min(min(abs(vBary.x - vBary.y), abs(vBary.x - vBary.z)), abs(vBary.y - vBary.z));",
   "  float edgeDist1 = 1.0;",
   "  if (vBary.x > (1/3.0)) { edgeDist1 = abs (vBary.y - vBary.z);}",
   "  else if (vBary.y > (1/3.0)) { edgeDist1 = abs (vBary.x - vBary.z);}",
   "  else if (vBary.z > (1/3.0)){ edgeDist1 = abs (vBary.x - vBary.y);}",
   "  float edgeDist = edgeDist0 + edgeDist1;",
   "  float width = fwidth(edgeDist);",
   "  float line = smoothstep(0.0, width, edgeDist);",
   "  fragColor = mix(vec4(0.5,0,0,1), vec4(1,1,1,1), line);",
   " }",
   "}"]

fragmentSrc4 :: String
fragmentSrc4 = unlines
  ["#version 330 core",
   "in vec3 vBary;",
   "flat in int vCorner;",
   "out vec4 fragColor;",
   "void main() {",
   " if (vCorner == 0) {",
   "  fragColor = vec4(1.0); // blank triangle",
   " } else {",
   "  float edgeDist0 = min(min(abs(vBary.x - vBary.y), abs(vBary.x - vBary.z)), abs(vBary.y - vBary.z));",
   "  float edgeDist1 = 1.0;",
   "  if (vBary.x > (1/3.0)) { edgeDist1 = abs (vBary.y - vBary.z);}",
   "  else if (vBary.y > (1/3.0)) { edgeDist1 = abs (vBary.x - vBary.z);}",
   "  else if (vBary.z > (1/3.0)){ edgeDist1 = abs (vBary.x - vBary.y);}",
   "  float edgeDist = edgeDist0 + edgeDist1;",
   "  float width = fwidth(edgeDist); // estimate fragment size in screen space",
   "  float line = smoothstep(width*0.5, width*1.5, edgeDist);",
   "  fragColor = mix(vec4(0.5,0,0,1), vec4(1,1,1,1), line);",
   " }",
   "}"]

fragmentSrc3 :: String
fragmentSrc3 = unlines
  ["#version 330 core",
   "in vec3 vBary;",
   "flat in int vCorner;",
   "out vec4 fragColor;",
   "void main() {",
   " if (vCorner == 0) {",
   "  fragColor = vec4(1.0); // blank triangle",
   " } else {",
   "  float edgeDist = 1;",
   "  if (vBary.x > (1/3.0)) { edgeDist = abs (vBary.y - vBary.z);}",
   "  else if (vBary.y > (1/3.0)) { edgeDist = abs (vBary.x - vBary.z);}",
   "  else if (vBary.z > (1/3.0)){ edgeDist = abs (vBary.x - vBary.y);}",
   "  float width = fwidth(edgeDist); // estimate fragment size in screen space",
   "  float line = smoothstep(width*0.5, width*1.5, edgeDist);",
   "  fragColor = mix(vec4(0.5,0,0,1), vec4(1,1,1,1), line);",
   " }",
   "}"]

fragmentSrc2 :: String
fragmentSrc2 = unlines
  ["#version 330 core",
   "in vec3 vBary;",
   "flat in int vCorner;",
   "out vec4 fragColor;",
   "void main() {",
   " if (vCorner == 0) {",
   "  fragColor = vec4(1.0); // blank triangle",
   " } else {",
   "  float edgeDist = min(min(abs(vBary.x - vBary.y), abs(vBary.x - vBary.z)), abs(vBary.y - vBary.z));",
   "  float width = fwidth(edgeDist); // estimate fragment size in screen space",
   "  float line = smoothstep(width*0.5, width*1.5, edgeDist);",
   "  fragColor = mix(vec4(0.5,0,0,1), vec4(1,1,1,1), line);",
   " }",
   "}"]

display :: GLuint -> GLuint -> DisplayCallback
display prog vao = do
  let cols = 10
      rows = 10
  glClearColor 1 1 1 1
  glClear GL_COLOR_BUFFER_BIT
  glUseProgram prog

  locCols <- withCString "uCols" $ glGetUniformLocation prog
  locRows <- withCString "uRows" $ glGetUniformLocation prog
  glUniform1i locCols (fromIntegral cols)
  glUniform1i locRows (fromIntegral rows)

  glBindVertexArray vao
  glDrawArraysInstanced GL_TRIANGLES 0 6 (fromIntegral (cols * rows))
  swapBuffers

