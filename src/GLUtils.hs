module GLUtils
  ( processInput
  , compileShaderProgram
  , newImageTexture
  , freeVideoResources
  , setupGeometry
  , setupFramebuffer
  , loadVideo
  , setFloatUniform
  , bindInputBus
  , bindTexture
  , setupTexture
  , writeImageToTexture
  ) where


import MyPrelude
import RIO
import RIO.Partial
import RIO.List.Partial
import qualified RIO.Text as Text

import qualified Codec.FFmpeg as F
import Codec.Picture
import Control.Monad.Loops (whileM)
import Data.ByteString (ByteString)
import qualified Data.Vector as V
import qualified Data.Vector.Storable as SV
import Foreign.Marshal.Array (withArray)
import Foreign.Ptr (nullPtr, plusPtr)
import Foreign.Storable (sizeOf)
import Graphics.Rendering.OpenGL (($=))
import qualified Graphics.Rendering.OpenGL as GL
import qualified Graphics.UI.GLFW as GLFW

import Shader (uniformName)
import Types


processInput :: GLFW.Window -> Maybe GLFW.Monitor -> GLFW.VideoMode -> IORef Bool -> IO ()
processInput window monitor videoMode isFullscreen = do
  escKeyState <- GLFW.getKey window GLFW.Key'Escape
  when (escKeyState == GLFW.KeyState'Pressed) $
    GLFW.setWindowShouldClose window True

  f11KeyState <- GLFW.getKey window GLFW.Key'F11
  when (f11KeyState == GLFW.KeyState'Pressed) $ do
    fullscreen <- readIORef isFullscreen
    if fullscreen
      then let width  = GLFW.videoModeWidth  videoMode
               height = GLFW.videoModeHeight videoMode
           in GLFW.setWindowed window (width `div` 2) (height `div` 2) 0 0
      else
        case monitor of
          Just mon -> GLFW.setFullscreen window mon videoMode
          Nothing  -> return ()
    writeIORef isFullscreen $ not fullscreen


compileShaderProgram :: ByteString -> ByteString -> IO GL.Program
compileShaderProgram vertexShaderSource fragmentShaderSource = do
  vertexShader <- GL.createShader GL.VertexShader
  GL.shaderSourceBS vertexShader $= vertexShaderSource
  GL.compileShader vertexShader

  vsCompileSuccess <- GL.compileStatus vertexShader
  unless vsCompileSuccess $ do
    infoLog <- GL.shaderInfoLog vertexShader
    putStrLn "ERROR::SHADER::VERTEX::COMPILATION_FAILED"
    putStrLn infoLog

  fragmentShader <- GL.createShader GL.FragmentShader
  GL.shaderSourceBS fragmentShader $= fragmentShaderSource
  GL.compileShader fragmentShader

  fsCompileSuccess <- GL.compileStatus fragmentShader
  unless fsCompileSuccess $ do
    infoLog <- GL.shaderInfoLog fragmentShader
    putStrLn "ERROR::SHADER::FRAGMENT::COMPILATION_FAILED"
    putStrLn infoLog

  shaderProgram <- GL.createProgram
  GL.attachShader shaderProgram vertexShader
  GL.attachShader shaderProgram fragmentShader
  GL.linkProgram  shaderProgram

  programLinkSuccess <- GL.linkStatus shaderProgram
  unless programLinkSuccess $ do
    infoLog <- GL.programInfoLog shaderProgram
    putStrLn "ERROR::SHADER::PROGRAM::LINKING_FAILED"
    putStrLn infoLog

  GL.detachShader shaderProgram vertexShader
  GL.detachShader shaderProgram fragmentShader

  return shaderProgram


setupGeometry :: IO (GL.VertexArrayObject, GL.BufferObject, GL.BufferObject)
setupGeometry = do
  let vertices =
        -- positions       -- colours      -- texture coords
        [ 1.0,  1.0, 0.0,  1.0, 0.0, 0.0,  1.0, 1.0 -- top right
        , 1.0, -1.0, 0.0,  0.0, 1.0, 0.0,  1.0, 0.0 -- bottom right
        ,-1.0, -1.0, 0.0,  0.0, 0.0, 1.0,  0.0, 0.0 -- bottom left
        ,-1.0,  1.0, 0.0,  1.0, 0.0, 1.0,  0.0, 1.0 -- top left
        ] :: [GL.GLfloat]
      indices =
        [ 0, 1, 3 -- first triangle
        , 1, 2, 3 -- second triangle
        ] :: [GL.GLuint]
      vertexSize      = fromIntegral $ sizeOf (head vertices)
      numVertices     = fromIntegral $ length vertices
      vertexArraySize = fromIntegral $ numVertices * vertexSize
      numIndices      = length indices
      indexArraySize  = fromIntegral $ numIndices * sizeOf (head indices)

  vao <- GL.genObjectName
  vbo <- GL.genObjectName
  ebo <- GL.genObjectName

  GL.bindVertexArrayObject $= Just vao

  GL.bindBuffer GL.ArrayBuffer $= Just vbo
  withArray vertices $ \ptr ->
    GL.bufferData GL.ArrayBuffer $= (vertexArraySize, ptr, GL.StaticDraw)

  GL.bindBuffer GL.ElementArrayBuffer $= Just ebo
  withArray indices $ \ptr ->
    GL.bufferData GL.ElementArrayBuffer $= (indexArraySize, ptr, GL.StaticDraw)

  -- position attribute
  GL.vertexAttribPointer (GL.AttribLocation 0) $=
    ( GL.ToFloat
    , GL.VertexArrayDescriptor 3 GL.Float (vertexSize * 8) nullPtr
    )
  GL.vertexAttribArray (GL.AttribLocation 0) $= GL.Enabled

  -- color attribute
  GL.vertexAttribPointer (GL.AttribLocation 1) $=
    ( GL.ToFloat
    , GL.VertexArrayDescriptor 3 GL.Float (vertexSize * 8) $
        plusPtr nullPtr $ fromIntegral (vertexSize * 3)
    )
  GL.vertexAttribArray (GL.AttribLocation 1) $= GL.Enabled

  -- texture coord attribute
  GL.vertexAttribPointer (GL.AttribLocation 2) $=
    ( GL.ToFloat
    , GL.VertexArrayDescriptor 2 GL.Float (vertexSize * 8) $
        plusPtr nullPtr $ fromIntegral (vertexSize * 6)
    )
  GL.vertexAttribArray (GL.AttribLocation 2) $= GL.Enabled

  return (vao, vbo, ebo)


setupFramebuffer :: Int-> Int -> IO (GL.FramebufferObject, GL.TextureObject)
setupFramebuffer width height = do
  framebuffer <- GL.genObjectName :: IO (GL.FramebufferObject)
  GL.bindFramebuffer GL.Framebuffer $= framebuffer

  GL.activeTexture $= GL.TextureUnit 0
  -- create a color attachment texture
  textureColorbuffer <- GL.genObjectName
  GL.textureBinding GL.Texture2D $= Just textureColorbuffer
  GL.textureWrapMode GL.Texture2D GL.S $= (GL.Repeated, GL.ClampToBorder)
  GL.textureWrapMode GL.Texture2D GL.T $= (GL.Repeated, GL.ClampToBorder)
  GL.textureBorderColor GL.Texture2D $= GL.Color4 0.0 0.0 0.0 0.0
  GL.texImage2D GL.Texture2D GL.NoProxy 0 GL.RGB'
    (GL.TextureSize2D (fromIntegral width) (fromIntegral height)) 0
    (GL.PixelData GL.RGB GL.UnsignedByte nullPtr)
  GL.textureFilter GL.Texture2D $= ((GL.Linear', Nothing), GL.Linear')
  GL.framebufferTexture2D GL.Framebuffer (GL.ColorAttachment 0)
    GL.Texture2D textureColorbuffer 0

  fbStatus <- GL.framebufferStatus GL.Framebuffer
  if fbStatus /= GL.Complete then
    putStrLn "*** Errror: Framebuffer is not complete"
    -- TODO: actually deal with this case where the framebuffer isn't complete
    --       for whatever reason.
  else return ()

  GL.bindFramebuffer GL.Framebuffer $= GL.defaultFramebufferObject
  return (framebuffer, textureColorbuffer)



setFloatUniform :: GL.Program -> Text -> Float -> IO ()
setFloatUniform shaderProgram uName floatValue = do
  uniformLoc <- GL.get $ GL.uniformLocation shaderProgram $ Text.unpack uName
  if uniformLoc >= (GL.UniformLocation 0) then do
    GL.uniform uniformLoc $= floatValue
  else do
    -- putStrLn $ "*** DEBUG: uniform "+|uName|+" is not in this shader program ("+||shaderProgram||+")"
    return ()



initialiseFFmpeg :: FilePath -> IO ( FrameGrabber PixelRGBA8, IO () )
initialiseFFmpeg videoFile = do
  -- putStrLn $ "*** Info: Initialising FFmpeg for \"" ++ videoFile ++ "\""
  F.initFFmpeg
  (getNextFrame, cleanupFFmpeg) <- F.imageReaderTime $ F.File videoFile
  return (getNextFrame, cleanupFFmpeg)



newImageTexture :: Int -> FilePath -> IO (Either String ImageTexture)
newImageTexture imgID iPath = do
  result <- readImage iPath
  case result of
    Left str ->
      return $ Left $ "could not load image at \""+|iPath|+"\": "+|str|+""
    Right dynamicImg -> do
      textureObject <- setupTexture
      let imageRGBA8 = convertRGBA8 dynamicImg
      writeImageToTexture textureObject imageRGBA8
      putStrLn $ "*** Debug:: new image texture ID = "+|imgID|+""
      return $ Right $ ImageTexture { iTexObj      = textureObject
                                    , iAssignments = []
                                    , iID          = imgID
                                    }



freeVideoResources :: Video -> IO ()
freeVideoResources = \case
  InMemVid _ videoData -> do frames <- readIORef (vFrames videoData)
                             V.forM_ frames $ GL.deleteObjectName . fst
  _ -> return ()



writeImageToTexture :: GL.TextureObject -> Image PixelRGBA8 -> IO ()
writeImageToTexture textureObject image = do
  let texWidth  = fromIntegral $ imageWidth  image
      texHeight = fromIntegral $ imageHeight image

  -- bind texture (it shouldn't matter what texture unit is active right now)
  GL.activeTexture $= GL.TextureUnit 0
  GL.textureBinding GL.Texture2D $= Just textureObject
  -- set the texture wrapping parameters
  GL.textureWrapMode GL.Texture2D GL.S $= (GL.Repeated, GL.ClampToBorder)
  GL.textureWrapMode GL.Texture2D GL.T $= (GL.Repeated, GL.ClampToBorder)
  GL.textureBorderColor GL.Texture2D $= GL.Color4 0.0 0.0 0.0 0.0
  -- set texture filtering parameters
  GL.textureFilter GL.Texture2D $= ((GL.Linear', Just GL.Linear'), GL.Linear')
  SV.unsafeWith (imageData image) $ \ptr ->
    GL.texImage2D GL.Texture2D GL.NoProxy 0 GL.RGBA8
      (GL.TextureSize2D texWidth texHeight) 0
      (GL.PixelData GL.RGBA GL.UnsignedByte ptr)

  -- TODO: try disabling mipmaps. why would they be wanted?
  GL.generateMipmap' GL.Texture2D



setupTexture :: IO GL.TextureObject
setupTexture = do
  GL.activeTexture $= GL.TextureUnit 0
  -- load and create a texture
  textureObject <- GL.genObjectName
  GL.textureBinding GL.Texture2D $= Just textureObject
  -- set the texture wrapping parameters
  GL.textureWrapMode GL.Texture2D GL.S $= (GL.Repeated, GL.ClampToBorder)
  GL.textureWrapMode GL.Texture2D GL.T $= (GL.Repeated, GL.ClampToBorder)
  GL.textureBorderColor GL.Texture2D $= GL.Color4 0.0 0.0 0.0 0.0
  -- set texture filtering parameters
  GL.textureFilter GL.Texture2D $= ((GL.Linear', Just GL.Linear'), GL.Linear')

  return textureObject



loadVideo :: Int -> FilePath -> IO Video
loadVideo videoID videoPath = do
  (getFrame, cleanup) <- initialiseFFmpeg videoPath
  stop <- newIORef False

  frames <- whileM (fmap not $ readIORef stop) $ do
    textureObject <- setupTexture
    frame <- getFrame
    case frame of
      Nothing -> writeIORef stop True >> cleanup >> return Nothing

      Just (imageRGBA8, frameTimestamp) -> do
        let imgWidth  = fromIntegral $ imageWidth  imageRGBA8
            imgHeight = fromIntegral $ imageHeight imageRGBA8

        GL.activeTexture $= GL.TextureUnit 0
        -- bind texture
        GL.textureBinding GL.Texture2D $= Just textureObject
        -- set the texture wrapping parameters
        GL.textureWrapMode GL.Texture2D GL.S $= (GL.Repeated, GL.ClampToBorder)
        GL.textureWrapMode GL.Texture2D GL.T $= (GL.Repeated, GL.ClampToBorder)
        GL.textureBorderColor GL.Texture2D $= GL.Color4 0.0 0.0 0.0 0.0
        -- set texture filtering parameters
        GL.textureFilter GL.Texture2D $= ((GL.Linear', Just GL.Linear'), GL.Linear')
        SV.unsafeWith (imageData imageRGBA8) $ \ptr ->
          GL.texImage2D GL.Texture2D GL.NoProxy 0 GL.RGBA8
            (GL.TextureSize2D imgWidth imgHeight) 0
            (GL.PixelData GL.RGBA GL.UnsignedByte ptr)
        GL.generateMipmap' GL.Texture2D

        return $ Just (textureObject, frameTimestamp)

  let videoFrames = V.fromList $ map fromJust $ filter isJust $ frames
  videoFramesRef <- newIORef videoFrames
  let videoData = VideoData { vFrames = videoFramesRef
                            , vNumFrames = V.length videoFrames
                            }
      videoFile = VideoFile { vID = videoID
                            , vFilePath = videoPath
                            }
  return $ InMemVid videoFile videoData



bindInputBus :: WireID -> Bus -> RIO ShaderState Bool
bindInputBus wire (Bus _ tObj) = ask >>= \env -> liftIO $ do
  texUnits <- readIORef $ ssTextureUnits env
  case texUnits of
    [] -> do
      putStrLn "*** Error: not enough texture units available to display image/video."
      return False
    (t:ts) -> do
      texLocation <- GL.uniformLocation (ssShaderProgram env) $ "u_Bus_Local_" ++ (show wire)
      if texLocation >= (GL.UniformLocation 0)
        then do
          GL.currentProgram $= Just (ssShaderProgram env)
          -- bind texture
          GL.activeTexture $= t
          GL.textureBinding GL.Texture2D $= Just tObj
          -- set the uniform location to this texture unit
          GL.uniform texLocation $= t
          -- set active texture back to default
          GL.activeTexture $= GL.TextureUnit 0
          -- update the available texture units
          writeIORef (ssTextureUnits env) ts
          return True

      else return False



bindTexture :: GL.TextureObject -> Assignment -> RIO ShaderState Bool
bindTexture textureObject (gID, uID, uIn) = ask >>= \env -> liftIO $ do
  texUnits <- readIORef $ ssTextureUnits env
  case texUnits of
    [] -> do
      putStrLn "*** Error: not enough texture units available to display image/video."
      return False
    (t:ts) -> do
      let uniform = Text.unpack $ uniformName gID uID uIn
      texLocation <- GL.uniformLocation (ssShaderProgram env) uniform

      if texLocation >= (GL.UniformLocation 0) then do
        GL.currentProgram $= Just (ssShaderProgram env)
        -- bind texture
        GL.activeTexture $= t
        GL.textureBinding GL.Texture2D $= Just textureObject
        -- set the uniform location to this texture unit
        GL.uniform texLocation $= t
        -- set active texture back to default
        GL.activeTexture $= GL.TextureUnit 0
        -- update the available texture units
        writeIORef (ssTextureUnits env) ts
        return True

      else return False
