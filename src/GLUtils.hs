module GLUtils
    ( processInput
    , compileShaderProgram
    , initialiseFFmpeg
    , newImageTexture
    , newVideoTexture
    , newLoadedVideo
    , setupGeometry
    , setupFramebuffer
    -- , setupTexture
    -- , deleteTexture
    , setFloatUniform
    , setFloatUniform'
    , updateTextures
    , freeTexture
    , bindInputBus
    ) where


import Prelude (putStrLn)
import RIO
import RIO.Partial
import RIO.List.Partial
import qualified RIO.Text as Text

import qualified Codec.FFmpeg as F
import Codec.FFmpeg.Juicy (JuicyPixelFormat)
import Codec.Picture
import Control.Monad.Loops (whileM)
import Data.ByteString (ByteString)
import Data.ObjectName
import qualified Data.Vector as V
import qualified Data.Vector.Storable as SV
import Fmt
import Foreign.Marshal.Array (withArray)
import Foreign.Ptr (nullPtr, plusPtr)
import Foreign.Storable (sizeOf)
import Graphics.Rendering.OpenGL (($=))
import Graphics.GL.Functions (glDeleteTextures)
import Graphics.GL.Types (GLsizei)
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

  vao <- genObjectName
  vbo <- genObjectName
  ebo <- genObjectName

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
  framebuffer <- genObjectName :: IO (GL.FramebufferObject)
  GL.bindFramebuffer GL.Framebuffer $= framebuffer

  -- create a color attachment texture
  textureColorbuffer <- genObjectName
  GL.textureBinding GL.Texture2D $= Just textureColorbuffer
  GL.textureWrapMode GL.Texture2D GL.S $= (GL.Repeated, GL.Repeat)
  GL.textureWrapMode GL.Texture2D GL.T $= (GL.Repeated, GL.Repeat)
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
  else return ()

  return (framebuffer, textureColorbuffer)

--  fbStatus <- GL.framebufferStatus GL.Framebuffer
--  if fbStatus /= GL.Complete then do
--    --deleteObjectName framebuffer
--	--deleteObjectName textureColorbuffer
--    return $ Left "Framebuffer is not complete"
--  else do
--	  return $ Right (framebuffer, textureColorbuffer)


setFloatUniform :: GL.Program -> Text -> Float -> IO ()
setFloatUniform shaderProgram uName floatValue = do
  uniformLoc <- GL.get $ GL.uniformLocation shaderProgram $ Text.unpack uName
  GL.uniform uniformLoc $= floatValue


setFloatUniform' :: GL.Program -> Text -> Float -> IO ()
setFloatUniform' shaderProgram uName floatValue = do
  uniformLoc <- GL.get $ GL.uniformLocation shaderProgram $ Text.unpack uName
  if uniformLoc >= (GL.UniformLocation 0) then do
    GL.uniform uniformLoc $= floatValue
  else do
    -- putStrLn $ "*** DEBUG: uniform "+|uName|+" is not in this shader program ("+||shaderProgram||+")"
    return ()



{- *** Textures, images and videos ***  -}


initialiseFFmpeg :: FilePath -> IO ( FrameGrabber PixelRGB8, IO () )
initialiseFFmpeg videoFile = do
  -- putStrLn $ "*** Info: Initialising FFmpeg for \"" ++ videoFile ++ "\""
  F.initFFmpeg
  (getNextFrame, cleanupFFmpeg) <- F.imageReaderTime $ F.File videoFile
  return (getNextFrame, cleanupFFmpeg)


newImageTexture :: Int -> FilePath -> IO (Either String Texture)
newImageTexture imgID iPath = do
  {- Use the first half of the texture ID space for video textures. The maximum
     number of texture image units available to the fragment shader varies based
     on hardware and is retrieved by the GL.maxTextureImageUnits function.
  -}
  maxTexUnits <- GL.maxTextureImageUnits
  -- putStrLn $ "*** Debug: maxTexUnits = "+|maxTexUnits|+""
  -- texture unit 0 is the previous frame texture
  let textureID  = imgID + 1
      maxImageID = ((fromIntegral maxTexUnits :: Int) `div` 2) - 1

  if imgID > maxImageID || imgID < 0 then
    return $ Left $ concat [ "image ID is outside of the range allowed by the hardware. "
                           , "The range of possible image IDs is 0 to "
                           , show maxImageID
                           ]
  else do
    result <- readImage iPath
    case result of
      Left str ->
        return $ Left $ "could not load image at \""+|iPath|+"\": "+|str|+""
      Right dynamicImg -> do
        (textureObject, textureUnit) <- setupTexture textureID
        -- putStrLn $ "*** Debug:: new image texture ID = "+|textureID|+""
        return $ Right
               $ Img $ ImageTexture { iTexObj      = textureObject
                                    , iTexUnit     = textureUnit
                                    , iAssignments = []
                                    , iID          = textureID
                                    , iImageRGB8   = convertRGB8 dynamicImg
                                    , iIsBound     = False
                                    }


newVideoTexture :: Int -> FilePath -> Float -> Bool -> IO (Either String Texture)
newVideoTexture vidID vPath vPlaybackRate vShouldLoop = do
  {- Use the second half of the texture ID space for video textures. The maximum
     number of texture image units available to the fragment shader varies based
     on hardware and is retrieved by the GL.maxTextureImageUnits function.
  -}
  maxTexUnits <- GL.maxTextureImageUnits
  -- texture unit 0 is the previous frame texture
  let textureID  = ((fromIntegral maxTexUnits :: Int) `div` 2) + vidID + 1
      maxVideoID = ((fromIntegral maxTexUnits :: Int) `div` 2) - 2
  -- putStrLn $ "*** Debug: maxTexUnits = "+|maxTexUnits|+""

  if vidID > maxVideoID || vidID < 0 then
    return $ Left $ concat [ "video ID is outside of the range allowed by the hardware. "
                           , "The range of possible video IDs is 0 to "
                           , show maxVideoID
                           ]
  else do
    putStrLn $ "*** Info: Initialising FFmpeg for \"" ++ vPath ++ "\""
    (getFrame, cleanup) <- initialiseFFmpeg vPath
    (textureObject, textureUnit) <- setupTexture textureID
    -- putStrLn $ "*** Debug:: new video texture ID = "+|textureID|+""
    return $ Right
           $ Vid $ VideoTexture { vTexObj        = textureObject
                                , vTexUnit       = textureUnit
                                , vAssignments   = []
                                , vID            = vidID
                                , vLoop          = vShouldLoop
                                , vFilePath      = vPath
                                , vNextFrame     = getFrame
                                , vCleanupFFmpeg = cleanup
                                , vStartTime     = Nothing
                                , vCurrentFrame  = (-1)
                                , vFps           = Nothing
                                , vRate          = realToFrac vPlaybackRate :: Double
                                }


newLoadedVideo :: Int -> FilePath -> Float -> Bool -> IO (Either String Texture)
newLoadedVideo vidID vPath vPlaybackRate vShouldLoop = do
  {- Use the second half of the texture ID space for video textures. The maximum
     number of texture image units available to the fragment shader varies based
     on hardware and is retrieved by the GL.maxTextureImageUnits function.
  -}
  maxTexUnits <- GL.maxTextureImageUnits
  let textureID = ((fromIntegral maxTexUnits :: Int) `div` 2) + vidID
      maxVideoID = ((fromIntegral maxTexUnits :: Int) `div` 2) - 1
  -- putStrLn $ "*** Debug: maxTexUnits = "+|maxTexUnits|+""

  if vidID > maxVideoID || vidID < 0 then
    return $ Left $ concat [ "video ID is outside of the range allowed by the hardware. "
                           , "The range of possible video IDs is 0 to "
                           , show maxVideoID
                           ]
  else do
    (getFrame, cleanup) <- initialiseFFmpeg vPath
    let textureUnit = (GL.TextureUnit (fromIntegral textureID :: GL.GLuint))
    putStrLn $ "*** Info: loading video frames as texture objects for \"" ++ vPath ++ "\""
    frames <- loadVideo getFrame cleanup
    -- putStrLn $ "*** Debug:: new video texture ID = "+|textureID|+""
    return $ Right
           $ LVd $ LoadedVideo { lvTexUnit       = textureUnit
                               , lvAssignments   = []
                               , lvID            = vidID
                               , lvLoop          = vShouldLoop
                               , lvFrames        = frames
                               , lvNumFrames     = V.length frames
                               , lvStartTime     = Nothing
                               , lvCurrentFrame  = (-1)
                               , lvFps           = Nothing
                               , lvRate          = realToFrac vPlaybackRate :: Double
                               }


setupTexture :: Int -> IO ( GL.TextureObject, GL.TextureUnit )
setupTexture tID = do
  -- load and create a texture
  textureObject <- genObjectName
  let textureUnit = (GL.TextureUnit (fromIntegral tID :: GL.GLuint))
  GL.activeTexture $= textureUnit
  GL.textureBinding GL.Texture2D $= Just textureObject
  -- set the texture wrapping parameters
  GL.textureWrapMode GL.Texture2D GL.S $= (GL.Repeated, GL.Repeat)
  GL.textureWrapMode GL.Texture2D GL.T $= (GL.Repeated, GL.Repeat)
  GL.textureBorderColor GL.Texture2D $= GL.Color4 0.0 0.0 0.0 0.0
  -- set texture filtering parameters
  GL.textureFilter GL.Texture2D $= ((GL.Linear', Just GL.Linear'), GL.Linear')

  return (textureObject, textureUnit)


setupTexture' :: IO ( GL.TextureObject )
setupTexture' = do
  -- load and create a texture
  textureObject <- genObjectName
  GL.activeTexture $= GL.TextureUnit (0 :: GL.GLuint)
  GL.textureBinding GL.Texture2D $= Just textureObject
  -- set the texture wrapping parameters
  GL.textureWrapMode GL.Texture2D GL.S $= (GL.Repeated, GL.Repeat)
  GL.textureWrapMode GL.Texture2D GL.T $= (GL.Repeated, GL.Repeat)
  GL.textureBorderColor GL.Texture2D $= GL.Color4 0.0 0.0 0.0 0.0
  -- set texture filtering parameters
  GL.textureFilter GL.Texture2D $= ((GL.Linear', Just GL.Linear'), GL.Linear')

  return textureObject


-- deleteTexture :: [Texture] -> Texture -> IO [Texture]
-- deleteTexture textures texture = do
--   deleteObjectName $ texObj texture
--   return $ filter (/= texture) textures


loadVideo :: FrameGrabber PixelRGB8 -> IO () -> IO (V.Vector (GL.TextureObject, Double))
loadVideo getFrame cleanupFFmpeg = do
  stop <- newIORef False

  frames <- whileM (fmap not $ readIORef stop) $ do
    textureObject <- setupTexture'
    frame <- getFrame
    (frameRead, (dynamicImg', frameTimestamp)) <- readFrame frame
    if frameRead then do
      let textureRGB8  = convertRGB8 dynamicImg'
          texWidth     = fromIntegral $ imageWidth  textureRGB8
          texHeight    = fromIntegral $ imageHeight textureRGB8

      -- bind texture
      GL.activeTexture $= GL.TextureUnit (0 :: GL.GLuint)
      GL.textureBinding GL.Texture2D $= Just textureObject
      -- set the texture wrapping parameters
      GL.textureWrapMode GL.Texture2D GL.S $= (GL.Repeated, GL.Repeat)
      GL.textureWrapMode GL.Texture2D GL.T $= (GL.Repeated, GL.Repeat)
      GL.textureBorderColor GL.Texture2D $= GL.Color4 0.0 0.0 0.0 0.0
      -- set texture filtering parameters
      GL.textureFilter GL.Texture2D $= ((GL.Linear', Just GL.Linear'), GL.Linear')
      SV.unsafeWith (imageData textureRGB8) $ \ptr ->
        GL.texImage2D GL.Texture2D GL.NoProxy 0 GL.RGB8
          (GL.TextureSize2D texWidth texHeight) 0
          (GL.PixelData GL.RGB GL.UnsignedByte ptr)
      GL.generateMipmap' GL.Texture2D

      return (Just textureObject, frameTimestamp)
    else do
      writeIORef stop True
      cleanupFFmpeg
      return (Nothing, frameTimestamp)

  return $ V.fromList $ map (\(mbTObj, ts) -> (fromJust mbTObj, ts))
                      $ filter (\(mbTexObj, _) -> isJust mbTexObj)
                      $ frames


freeTexture :: WindowID -> Texture -> IO ()
freeTexture winID (Vid vid) = do
  putStrLn $ "*** Info: Window "+|winID|+" - running FFmpeg cleanup for texture "+|texID vid|+""
  vCleanupFFmpeg vid
  deleteObjectName (vTexObj vid)
freeTexture _wID  (Img img) = deleteObjectName (iTexObj img)
freeTexture winID (LVd vid) = do
  putStrLn $ "*** Info: Window "+|winID|+" - freeing frames for texture "+|texID vid|+""
  let frames  = lvFrames vid
      size    = V.length frames
      texObjs = SV.generate size $ \i -> (\(GL.TextureObject t) -> t) $ fst (frames V.! i)
  SV.unsafeWith texObjs $ \ptr ->
    glDeleteTextures (fromIntegral size :: GLsizei) ptr
  deleteObjectNames $ map (\t -> GL.TextureObject t) $ SV.toList texObjs


updateTextures :: GL.Program -> Int -> [Texture] -> IO [Texture]
updateTextures shaderProgram texUnitsUsed textures =
  let vids = map (\(Vid t) -> t) $ filter isVid textures
      imgs = map (\(Img t) -> t) $ filter isImg textures
      lvds = map (\(LVd t) -> t) $ filter isLVd textures
  in do
    updatedImages <- forM imgs (updateImage shaderProgram texUnitsUsed)
    updatedVideos <- updateVideos shaderProgram texUnitsUsed vids
    updatedLVids  <- forM lvds (updateLVid shaderProgram texUnitsUsed)
    return $ map fromJust $ filter isJust (updatedImages ++ updatedVideos ++ updatedLVids)
  where isVid (Vid _) = True
        isVid _       = False
        isImg (Img _) = True
        isImg _       = False
        isLVd (LVd _) = True
        isLVd _       = False

updateImage :: GL.Program -> Int -> ImageTexture -> IO (Maybe Texture)
updateImage shaderProgram texUnitsUsed texture =
  let textureUnit = (\(GL.TextureUnit n) -> GL.TextureUnit $ n + (fromIntegral texUnitsUsed)) $ texUnit texture
  in if ((length (assignments texture) == 0) || (iIsBound texture)) && (textureUnit == texUnit texture)
    then return $ Just (Img texture)
    else do
      bindTexture shaderProgram (texture {iTexUnit = textureUnit}) (iImageRGB8 texture)
      return $ Just $ Img $ ImageTexture (texObj texture)
                                         textureUnit
                                         (assignments texture)
                                         (iID texture)
                                         (iImageRGB8 texture)
                                         True -- mark texture as bound

updateVideos :: GL.Program -> Int -> [VideoTexture] -> IO [Maybe Texture]
updateVideos shaderProgram texUnitsUsed textures = do
  -- updated <- flip forkMapM textures $ \texture ->
  updated <- forM textures $ \texture ->
    if length (assignments texture) == 0 then -- if not active then
      if isNothing (vStartTime texture)       --     do nothing
        -- then return $ (Just $ Vid texture, Nothing)
        then return $ (Just $ Vid texture, False)
        -- else return $ (Just $ Vid $ texture { vStartTime = Nothing }, Nothing)
        else return $ (Just $ Vid $ texture { vStartTime = Nothing }, False)
    else if vRate texture <= 0 then           -- else if the rate is 0 or less then
      -- return $ (Just $ Vid texture, Nothing)  --     do nothing
      return $ (Just $ Vid texture, False)  --     do nothing
    else do                                   -- else
      let getFrame      = vNextFrame texture  --     display the next frame
          cleanupFFmpeg = vCleanupFFmpeg texture
          frameIndex    = (vCurrentFrame texture) + 1
          frameIndexD   = fromIntegral frameIndex :: Double
          startTime     = vStartTime texture
          fps           = vFps texture
          frameInterval = 1 / (fromMaybe 24.0 fps)
          rate          = vRate texture
          textureUnit   = (\(GL.TextureUnit n) -> GL.TextureUnit $ n + (fromIntegral texUnitsUsed)) $ texUnit texture

      glCurrentTime <- GLFW.getTime
      let currentTime = fromMaybe 0.0 $ ((fromMaybe 0.0 glCurrentTime) -) <$> startTime
          scheduledTime =  frameIndexD * frameInterval * (1 / rate)

      -- putStrLn ""
      -- putStrLn $ "*** Debug: startTime = " ++ (show startTime)
      -- putStrLn $ "*** Debug: currentTime = " ++ (show currentTime)
      -- putStrLn $ "*** Debug: scheduledTime = " ++ (show scheduledTime)
      -- putStrLn $ "*** Debug: frameIndex = " ++ (show frameIndex)

      if currentTime < scheduledTime && not (currentTime == 0.0) then do
        -- return $ (Just (Vid texture), Nothing)
        return $ (Just (Vid texture), False)
      else do
        let skippedFrames = if isJust startTime
                              then floor $ (currentTime - scheduledTime) / frameInterval
                              else 0

        -- if skippedFrames > 0 then do
        --   putStrLn $ "*** Debug: skippedFrames = " ++ (show skippedFrames)
        --   putStrLn $ "*** Debug: startTime = " ++ (show startTime)
        --   putStrLn $ "*** Debug: currentTime = " ++ (show currentTime)
        --   putStrLn $ "*** Debug: scheduledTime = " ++ (show scheduledTime)
        --   putStrLn $ "*** Debug: frameIndex = " ++ (show frameIndex)
        -- else
        --   return ()


        -- TODO: check if skipped frames go over the end of the video
        --       - if so, check if video loops and skip remaining number of frames
        --         into the start of the video.
        --       - if not, free up resources and clear texture object

        -- skip over any missed frames
        replicateM_ skippedFrames getFrame

        frame <- getFrame
        (frameRead, (dynamicImg', frameTimestamp)) <- readFrame frame
        if frameRead then do
          -- bindTexture shaderProgram texture (convertRGB8 dynamicImg')
          writeToTexture texture (convertRGB8 dynamicImg')
          -- let img = convertRGB8 dynamicImg'
          glCurrentTime' <- GLFW.getTime
          -- img `seq` return (Just $ Vid $ VideoTexture (texObj texture)
          return (Just $ Vid $ VideoTexture (texObj texture)
                                             (texUnit texture)
                                             (assignments texture)
                                             (vID texture)
                                             (vLoop texture)
                                             (vFilePath texture)
                                             getFrame
                                             cleanupFFmpeg
                                             (if isNothing startTime then
                                                (\t -> t - scheduledTime) <$> glCurrentTime'
                                              else startTime)
                                             (frameIndex + skippedFrames)
                                             (if frameIndex >= 1 && isNothing fps then
                                                Just (frameIndexD / frameTimestamp)
                                              else fps)
                                             (vRate texture)
                            -- , Just img)
                            , True)
        else if vLoop texture then do
          (getFrame', cleanupFFmpeg') <- loopVideo cleanupFFmpeg (vFilePath texture)
          frame1           <- getFrame'
          (_, (dynamicImg1, _frameTimestamp')) <- readFrame frame1
          -- bindTexture shaderProgram texture (convertRGB8 dynamicImg1)
          writeToTexture texture (convertRGB8 dynamicImg1)
          -- let img = convertRGB8 dynamicImg1
          glCurrentTime' <- GLFW.getTime
          -- img `seq` return (Just $ Vid $ VideoTexture (texObj texture)
          return (Just $ Vid $ VideoTexture (texObj texture)
                                             textureUnit
                                             (assignments texture)
                                             (vID texture)
                                             (vLoop texture)
                                             (vFilePath texture)
                                             getFrame'
                                             cleanupFFmpeg'
                                             glCurrentTime'
                                             0
                                             (vFps texture)
                                             (vRate texture)
                            -- , Just img)
                            , True)
          else do -- video has finished and is set to not loop
            cleanupFFmpeg
            -- return (Nothing, Nothing)
            return (Nothing, False)

  -- updatedTextures <- forM (zip [0..] updated) $ \vidDataOrErr ->
  --   case vidDataOrErr of
  --     (i, Left _error) -> do
  --       putStrLn "*** Debug: error in forkMapM"
  --       return $ Just $ Vid (textures !! i)
  --     (_, Right (tex, Nothing)) -> return tex
  --     (_, Right (Just tex, Just img)) -> do
  --       bindTexture shaderProgram tex img
  --       return $ Just tex
  --     (_, Right (Nothing, _)) -> return Nothing

  -- return updatedTextures

  updatedTextures <- forM (zip [(0::Int)..] updated) $ \vidData ->
    case vidData of
      (_, (tex, False)) -> return tex
      (_, (Just tex, True)) -> do
        bindTexture' shaderProgram tex (texObj tex, 0.0)
        return $ Just tex
      (_, (Nothing, _)) -> return Nothing

  return updatedTextures


updateLVid :: GL.Program -> Int -> LoadedVideo -> IO (Maybe Texture)
updateLVid shaderProgram texUnitsUsed texture =
  if length (assignments texture) == 0 then -- if not active then
    if isNothing (lvStartTime texture)       --     do nothing
      then return $ Just $ LVd texture
      else return $ Just $ LVd $ texture { lvStartTime = Nothing }
  else if lvRate texture <= 0 then           -- else if the rate is 0 or less then
    return $ Just $ LVd texture             --     do nothing
  else do                                   -- else
    let frameIndex    = (lvCurrentFrame texture) + 1
        frameIndexD   = fromIntegral frameIndex :: Double
        startTime     = lvStartTime texture
        fps           = lvFps texture
        frameInterval = 1 / (fromMaybe 24.0 fps)
        rate          = lvRate texture
        textureUnit   = (\(GL.TextureUnit n) -> GL.TextureUnit $ n + (fromIntegral texUnitsUsed)) $ texUnit texture

    glCurrentTime <- GLFW.getTime
    let currentTime = fromMaybe 0.0 $ ((fromMaybe 0.0 glCurrentTime) -) <$> startTime
        scheduledTime =  frameIndexD * frameInterval * (1 / rate)

    if currentTime < scheduledTime && not (currentTime == 0.0) then do
      return $ Just (LVd texture)
    else do
      let skippedFrames = if isJust startTime
                            then floor $ (currentTime - scheduledTime) / frameInterval
                            else 0
          newFrameIndex = frameIndex + skippedFrames

      -- skip over any missed frames
      -- replicateM_ skippedFrames getFrame

      if newFrameIndex < (lvNumFrames texture) then do
        let frame = (lvFrames texture) V.! newFrameIndex

        bindTexture' shaderProgram texture frame
        glCurrentTime' <- GLFW.getTime
        return $ Just $ LVd $ LoadedVideo (texUnit texture)
                                          (assignments texture)
                                          (lvID texture)
                                          (lvLoop texture)
                                          (lvFrames texture)
                                          (lvNumFrames texture)
                                          (if isNothing startTime then
                                             (\t -> t - scheduledTime) <$> glCurrentTime'
                                           else startTime)
                                          frameIndex
                                          (if frameIndex >= 1 && isNothing fps then
                                             Just (frameIndexD / (snd frame))
                                           else fps)
                                          (lvRate texture)
      else if lvLoop texture then do
        let frame0 = V.head (lvFrames texture)
        bindTexture' shaderProgram texture frame0
        glCurrentTime' <- GLFW.getTime
        return $ Just $ LVd $ LoadedVideo textureUnit
                                          (assignments texture)
                                          (lvID texture)
                                          (lvLoop texture)
                                          (lvFrames texture)
                                          (lvNumFrames texture)
                                          glCurrentTime'
                                          0
                                          (lvFps texture)
                                          (lvRate texture)
      else do -- video has finished and is set to not loop
        forM_ (lvFrames texture) $ \(tObj, _) ->
          deleteObjectName tObj
        return Nothing


writeToTexture :: TextureClass t => t -> Image PixelRGB8 -> IO ()
writeToTexture texture textureRGB8 = do
  let texWidth     = fromIntegral $ imageWidth  textureRGB8
      texHeight    = fromIntegral $ imageHeight textureRGB8
      -- texData      = imageData textureRGB8
      textureObject = texObj texture
      -- textureUnit   = texUnit texture

  -- bind texture
  GL.activeTexture $= GL.TextureUnit (0 :: GL.GLuint)
  GL.textureBinding GL.Texture2D $= Just textureObject
  -- set the texture wrapping parameters
  GL.textureWrapMode GL.Texture2D GL.S $= (GL.Repeated, GL.Repeat)
  GL.textureWrapMode GL.Texture2D GL.T $= (GL.Repeated, GL.Repeat)
  GL.textureBorderColor GL.Texture2D $= GL.Color4 0.0 0.0 0.0 0.0
  -- set texture filtering parameters
  GL.textureFilter GL.Texture2D $= ((GL.Linear', Just GL.Linear'), GL.Linear')
  SV.unsafeWith (imageData textureRGB8) $ \ptr ->
    GL.texImage2D GL.Texture2D GL.NoProxy 0 GL.RGB8
      (GL.TextureSize2D texWidth texHeight) 0
      (GL.PixelData GL.RGB GL.UnsignedByte ptr)
  GL.generateMipmap' GL.Texture2D


bindTexture :: TextureClass t => GL.Program -> t -> Image PixelRGB8 -> IO ()
bindTexture shaderProgram texture textureRGB8 = do
  let texWidth     = fromIntegral $ imageWidth  textureRGB8
      texHeight    = fromIntegral $ imageHeight textureRGB8
      texData      = imageData textureRGB8
      textureObject = texObj texture
      textureUnit   = texUnit texture
  forM_ (assignments texture) $ \(gID, uID, uIn) -> do
    -- must activate/use the shader before setting uniforms
    GL.currentProgram $= Just shaderProgram

    GL.activeTexture $= textureUnit
    texLocation <- GL.uniformLocation shaderProgram $ Text.unpack $ uniformName gID uID uIn
    -- putStrLn $ uniformName gID uID uIn

    if texLocation >= (GL.UniformLocation 0)
      then do
        GL.uniform texLocation $= textureUnit

        -- bind texture
        GL.activeTexture $= textureUnit
        GL.textureBinding GL.Texture2D $= Just textureObject
        -- set the texture wrapping parameters
        GL.textureWrapMode GL.Texture2D GL.S $= (GL.Repeated, GL.Repeat)
        GL.textureWrapMode GL.Texture2D GL.T $= (GL.Repeated, GL.Repeat)
        GL.textureBorderColor GL.Texture2D $= GL.Color4 0.0 0.0 0.0 0.0
        -- set texture filtering parameters
        GL.textureFilter GL.Texture2D $= ((GL.Linear', Just GL.Linear'), GL.Linear')
        SV.unsafeWith texData $ \ptr ->
          GL.texImage2D GL.Texture2D GL.NoProxy 0 GL.RGB8
            (GL.TextureSize2D texWidth texHeight) 0
            (GL.PixelData GL.RGB GL.UnsignedByte ptr)
        GL.generateMipmap' GL.Texture2D

      else return ()


bindTexture' :: TextureClass t => GL.Program -> t -> (GL.TextureObject, Double) -> IO ()
bindTexture' shaderProgram texture (textureObject, _ts) = do
  let textureUnit = texUnit texture

  forM_ (assignments texture) $ \(gID, uID, uIn) -> do
    -- !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    -- !! don't forget to activate/use the shader before setting uniforms! !!
    -- !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    GL.currentProgram $= Just shaderProgram

    GL.activeTexture $= textureUnit
    texLocation <- GL.uniformLocation shaderProgram $ Text.unpack $ uniformName gID uID uIn
    -- putStrLn $ uniformName gID uID uIn

    if texLocation >= (GL.UniformLocation 0)
      then do
        GL.uniform texLocation $= textureUnit

        -- bind texture
        GL.activeTexture $= textureUnit
        GL.textureBinding GL.Texture2D $= Just textureObject

    else return ()


bindInputBus :: GL.Program -> Int -> WireID -> GL.TextureObject -> IO ()
bindInputBus shaderProgram index wire textureObject = do
  let textureUnit = GL.TextureUnit (fromIntegral index)
  GL.activeTexture $= textureUnit
  texLocation <- GL.uniformLocation shaderProgram $ "u_Bus_Local_" ++ (show wire)
  if texLocation >= (GL.UniformLocation 0)
    then do
      GL.uniform texLocation $= textureUnit

      -- bind texture
      GL.activeTexture $= textureUnit
      GL.textureBinding GL.Texture2D $= Just textureObject

  else return ()


loopVideo :: JuicyPixelFormat p
          => IO ()
          -> FilePath
          -> IO (FrameGrabber p, IO ())
loopVideo cleanupFFmpeg videoFile = do
  cleanupFFmpeg
  (getFrame', cleanupFFmpeg') <- F.imageReaderTime $ F.File videoFile
  return (getFrame', cleanupFFmpeg')


readFrame :: Maybe (Image PixelRGB8, Double) -> IO (Bool, (DynamicImage, Double))
readFrame frame =
  case frame of
    Just (img, time) -> return (True, (ImageRGB8 img, time))
    Nothing ->
      return (False, ((ImageRGB8 $
        generateImage (\x y ->
          let x' = fromIntegral x
              y' = fromIntegral y
              z' = fromIntegral $ x + y
          in PixelRGB8 x' y' z')
        256 256), 0.0))
