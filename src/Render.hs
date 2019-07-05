{-# LANGUAGE OverloadedStrings #-}

module Render (newWindow) where


import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad (forever)
import Control.Monad.Loops
import Data.ByteString.Char8 (pack, unpack)
import Data.Foldable (forM_)
import Data.IntMap (IntMap)
import qualified Data.IntMap.Strict as IntMap
import Data.IORef
import Fmt
import Foreign.Ptr (nullPtr)
import Graphics.Rendering.OpenGL (($=))
import qualified Graphics.Rendering.OpenGL as GL
import qualified Graphics.UI.GLFW as GLFW
import System.ZMQ4 (Socket, Sub)

import GLUtils
import Msg
import Shader
import Texture
import Transform
import Types


newWindow :: Socket Sub -> TQueue Msg -> TQueue Msg -> WindowID -> Int -> Int -> IO ()
newWindow sock msgQIn msgQOut winID width height =
  withWindow width height ("window " ++ show winID) $ \window -> do
    initShaderProgram <- compileShaderProgram vertexShader defaultFragShader
    screenShader <- compileShaderProgram screenVertShader screenFragShader

    (vao, _vbo, _ebo) <- setupGeometry

    monitor <- GLFW.getPrimaryMonitor
    let videoMode = GLFW.VideoMode width height 8 8 8 60
    isFullscreen <- newIORef False

    GL.currentProgram $= Just initShaderProgram

    uniformVals   <- newTBQueueIO 1000 :: IO (TBQueue UnitData)
    textures      <- newTVarIO ([] :: [Texture])
    textureQueue  <- newTBQueueIO 1000 :: IO (TBQueue TextureUpdate)
    shaderProg    <- newIORef initShaderProgram
    shouldExit    <- newIORef False
    nodeTree      <- newIORef IntMap.empty :: IO (IORef (IntMap Node))
    iter1Ref      <- newIORef True
    threadID      <- forkIO $ forever $ receiveDataMsg sock winID uniformVals textures textureQueue

    -- create framebuffers to allow feedback textures
    (fb, texColBuf)   <- setupFramebuffer width height
    (fb', texColBuf') <- setupFramebuffer width height
    GL.bindFramebuffer GL.Framebuffer $= GL.defaultFramebufferObject

    -- main render loop
    whileM_ (fmap not $ readIORef shouldExit) $ do
      wsc <- GLFW.windowShouldClose window
      nodeTree' <- readIORef nodeTree
      let ugens         = concatMap (\(Node us) -> us) $ IntMap.elems nodeTree'
          prevFrameUGen = 0 == (length $ filter (\u -> unitName u == "GLPrevFrame" || unitName u == "GLPrevFrame2") $ ugens)

      if wsc then do
        closeWindow winID textures shouldExit
        atomically $ writeTQueue msgQOut $ InternalWindowFree winID
      else if prevFrameUGen then do

        processInput window monitor videoMode isFullscreen

        {- If there's a message on the queue, pop it off and perform it in
           some manner.
        -}
        processCommands textures shouldExit shaderProg nodeTree msgQIn

        renderNoFB textures shaderProg uniformVals textureQueue vao window

      else do

        processInput window monitor videoMode isFullscreen
        processCommands textures shouldExit shaderProg nodeTree msgQIn

        iter1 <- readIORef iter1Ref
        if iter1 then do
          -- write to fb', read texColBuf, write to texColBuf'
          renderFB fb' texColBuf texColBuf'
            screenShader textures shaderProg uniformVals textureQueue vao window
          writeIORef iter1Ref False
        else do
          -- write to fb, read texColBuf', write to texColBuf
          renderFB fb texColBuf' texColBuf
            screenShader textures shaderProg uniformVals textureQueue vao window
          writeIORef iter1Ref True

    putStrLn $ "*** Info: Window "+|winID|+" - Killing sub socket thread"
    killThread threadID


renderNoFB :: TVar [Texture]
           -> IORef GL.Program
           -> TBQueue UnitData
           -> TBQueue TextureUpdate
           -> GL.VertexArrayObject
           -> GLFW.Window
           -> IO ()
renderNoFB textures shaderProg uniformVals textureQueue vao window = do
  -- render to the default framebuffer
  GL.bindFramebuffer GL.Framebuffer $= GL.defaultFramebufferObject
  GL.clearColor $= GL.Color4 0.0 0.0 0.0 1.0
  GL.clear [GL.ColorBuffer]

  {- Update float uniforms.
  -}
  shaderProgram <- readIORef shaderProg
  GL.currentProgram $= Just shaderProgram
  uniforms <- atomically $ flushTBQueue uniformVals
  forM_ uniforms $ \u -> do
    let gID   = fromIntegral (uDataNodeID u)  :: Int
        uID   = fromIntegral (uDataUnitID u)  :: Int
        input = fromIntegral (uDataInput u)   :: Int
        name = uniformName gID uID input
    -- putStrLn $ "*** Debug: performing data msg. " ++ (show u)
    setFloatUniform shaderProgram name (uDataValue u)

  {- Update texture assignments.
  -}
  textureUpdates <- atomically $ flushTBQueue textureQueue
  forM_ textureUpdates (applyTextureUpdate textures)

  {- Update texture uniforms.
  -}
  textures'  <- readTVarIO textures
  textures'' <- updateTextures shaderProgram textures'
  atomically $ modifyTVar' textures $ \_ -> textures''

  GL.bindVertexArrayObject $= Just vao
  GL.drawElements GL.Triangles 6 GL.UnsignedInt nullPtr

  GLFW.swapBuffers window
  GLFW.pollEvents


renderFB :: GL.FramebufferObject
         -> GL.TextureObject
         -> GL.TextureObject
         -> GL.Program
         -> TVar [Texture]
         -> IORef GL.Program
         -> TBQueue UnitData
         -> TBQueue TextureUpdate
         -> GL.VertexArrayObject
         -> GLFW.Window
         -> IO ()
renderFB fb tb1 tb2 ss textures shaderProg uniformVals textureQueue vao window = do
  -- render to the background framebuffer
  GL.bindFramebuffer GL.Framebuffer $= fb
  GL.clearColor $= GL.Color4 0.0 0.0 0.0 1.0
  GL.clear [GL.ColorBuffer]

  {- Update float uniforms.
  -}
  shaderProgram <- readIORef shaderProg
  GL.currentProgram $= Just shaderProgram
  uniforms <- atomically $ flushTBQueue uniformVals
  forM_ uniforms $ \u -> do
    let gID   = fromIntegral (uDataNodeID u)  :: Int
        uID   = fromIntegral (uDataUnitID u)  :: Int
        input = fromIntegral (uDataInput u)   :: Int
        name = uniformName gID uID input
    setFloatUniform shaderProgram name (uDataValue u)

  {- Update texture assignments.
  -}
  textureUpdates <- atomically $ flushTBQueue textureQueue
  forM_ textureUpdates (applyTextureUpdate textures)

  {- Update texture uniforms.
  -}
  GL.activeTexture $= (GL.TextureUnit 0)
  GL.textureBinding GL.Texture2D $= Just tb1
  textures'  <- readTVarIO textures
  textures'' <- updateTextures shaderProgram textures'
  atomically $ modifyTVar' textures $ \_ -> textures''

  GL.bindVertexArrayObject $= Just vao
  GL.drawElements GL.Triangles 6 GL.UnsignedInt nullPtr

  -- switch back to the default framebuffer and render the background fb's contents
  GL.bindFramebuffer GL.Framebuffer $= GL.defaultFramebufferObject
  GL.clearColor $= GL.Color4 0.0 0.0 0.0 1.0
  GL.clear [GL.ColorBuffer]

  GL.currentProgram $= Just ss
  GL.bindVertexArrayObject $= Just vao
  GL.activeTexture $= (GL.TextureUnit 0)
  GL.textureBinding GL.Texture2D $= Just tb2
  GL.drawElements GL.Triangles 6 GL.UnsignedInt nullPtr

  GLFW.swapBuffers window
  GLFW.pollEvents


closeWindow :: WindowID -> TVar [Texture] -> IORef Bool -> IO ()
closeWindow winID textures shouldExit = do
  textures' <- readTVarIO textures
  forM_ textures' $ freeTexture winID
  atomically $ modifyTVar' textures (\_ -> [])
  modifyIORef' shouldExit (\_-> True)


processCommands :: TVar [Texture]
                -> IORef Bool
                -> IORef GL.Program
                -> IORef (IntMap Node)
                -> TQueue Msg
                -> IO ()
processCommands textures shouldExit shaderProg nodeTree msgQIn = do
  isEmpty <- atomically $ isEmptyTQueue msgQIn
  if isEmpty
    then return ()
    else do
      msg <- atomically $ readTQueue msgQIn
      -- putStrLn $ "*** Debug: Window "+|winID|+" - received message: "+|(show msg)|+""
      handleMsg msg
  where
    handleMsg msg = case msg of
      GLWindowFree winID -> closeWindow winID textures shouldExit

      GLVideoNew vidID vPath vPlaybackRate vShouldLoop winID -> do
        textures' <- readTVarIO textures

        -- delete any existing videos with this ID
        let matchingTextures = filterVideoTextures (\t -> texID t == vidID) textures'
        if length matchingTextures > 0 then do
          atomically $ modifyTVar' textures $ dropVideoTexture vidID
          forM_ matchingTextures $ freeTexture winID
        else
          return ()

        tex <- newVideoTexture vidID vPath vPlaybackRate vShouldLoop
        case tex of
          Left errorStr -> putStrLn $ "*** Error: "+|errorStr|+""
          Right texture -> atomically $ modifyTVar' textures $ \ts -> texture : ts


      GLVideoRead vidID vPath vPlaybackRate vShouldLoop winID -> do
        textures' <- readTVarIO textures

        -- delete any existing videos with this ID
        let matchingTextures = filterVideoTextures (\t -> texID t == vidID) textures'
        if length matchingTextures > 0 then do
          atomically $ modifyTVar' textures $ dropVideoTexture vidID
          forM_ matchingTextures $ freeTexture winID
        else
          return ()

        tex <- newLoadedVideo vidID vPath vPlaybackRate vShouldLoop
        case tex of
          Left errorStr -> putStrLn $ "*** Error: "+|errorStr|+""
          Right texture -> do
            putStrLn $ "*** Info: adding new loaded video texture - ID = " ++ (show $ texID texture)
            atomically $ modifyTVar' textures $ \ts -> texture : ts


      GLVideoFree vidID winID -> do
        textures' <- readTVarIO textures
        let matchingTextures = filterVideoTextures (\t -> texID t == vidID) textures'

        if length matchingTextures > 0 then do
          atomically $ modifyTVar' textures $ dropVideoTexture vidID
          forM_ matchingTextures $ freeTexture winID
        else
          return ()


      GLImageNew imgID iPath _winID -> do
        textures' <- readTVarIO textures

        let matchingTextures = filterImageTextures (\t -> iID t == imgID) textures'
        if length matchingTextures > 0 then
          atomically $ modifyTVar' textures $ dropImageTexture imgID
        else
          return ()

        tex <- newImageTexture imgID iPath
        case tex of
          Left errorStr -> putStrLn $ "*** Error: "+|errorStr|+""
          Right texture -> atomically $ modifyTVar' textures $ \ts -> texture : ts


      GLImageFree imgID _winID -> do
        textures' <- readTVarIO textures
        let matchingTextures = filterImageTextures (\t -> iID t == imgID) textures'

        if length matchingTextures > 0 then
          atomically $ modifyTVar' textures $ dropImageTexture imgID
        else
          return ()


      {- Add the received graph to the node tree and generate new shader program
         from that.
      -}
      GraphNew gID gUnits -> do
        let glUGenNames = map shaderName ugenShaderFns
            glUnits = filter (\u -> elem (unitName u) glUGenNames) gUnits
        modifyIORef nodeTree $ \nt -> IntMap.insert gID (Node glUnits) nt
        nodeTree' <- readIORef nodeTree
        let fs = pack $ generateGLSLCode nodeTree'

        putStrLn $ unpack fs

        newShaderProgram <- compileShaderProgram vertexShader fs

        -- allocate texture unit 0 for the previous frame texture
        tLoc <- GL.uniformLocation newShaderProgram "sc_PrevFrame"
        GL.uniform tLoc $= (GL.TextureUnit 0)

        writeIORef shaderProg newShaderProgram


      {- Delete the graph from the node tree and generate a new shader program
         from the updated node tree
      -}
      GraphFree gID -> do
        modifyIORef nodeTree $ \nt -> IntMap.delete gID nt
        nodeTree' <- readIORef nodeTree
        let fs = pack $ generateGLSLCode nodeTree'

        putStrLn $ unpack fs

        newShaderProgram <- compileShaderProgram vertexShader fs

        -- allocate texture unit 0 for the previous frame texture
        tLoc <- GL.uniformLocation newShaderProgram "sc_PrevFrame"
        GL.uniform tLoc $= (GL.TextureUnit 0)

        writeIORef shaderProg newShaderProgram

        currentTime <- GLFW.getTime
        atomically $ modifyTVar' textures $ map $ \tex ->
          case tex of
            Vid texture ->
              let newAssigns = filter (\(gID',_,_) -> gID' /= gID) (assignments texture)
              in  if length newAssigns == 0 then
                    updateStartTime currentTime (updateAssignments [] tex)
                  else (updateAssignments [] tex)
            Img texture ->
              let newAssigns = filter (\(gID',_,_) -> gID' /= gID) (assignments texture)
              in  updateAssignments newAssigns tex
            LVd texture ->
              let newAssigns = filter (\(gID',_,_) -> gID' /= gID) (assignments texture)
              in  if length newAssigns == 0 then
                    updateStartTime currentTime (updateAssignments [] tex)
                  else (updateAssignments [] tex)


      _ -> return ()


applyTextureUpdate :: TVar [Texture] -> TextureUpdate -> IO ()
applyTextureUpdate textures textureUpdate =
  case textureUpdate of
    AssignVideo assignment vidID ->
      atomically $ modifyTVar' textures $ map $ \texture ->
        case texture of
          Img _ -> texture
          Vid _ ->
            let newAssignments = newAssigns texture assignment vidID
            in  if newAssignments /= assignments texture then
                  updateAssignments (newAssigns texture assignment vidID) texture
                else
                  texture
          LVd _ ->
            let newAssignments = newAssigns texture assignment vidID
            in  if newAssignments /= assignments texture then
                  updateAssignments (newAssigns texture assignment vidID) texture
                else
                  texture

    AssignImage assignment imgID ->
      atomically $ modifyTVar' textures $ map $ \texture ->
        case texture of
          Vid _ -> texture
          LVd _ -> texture
          Img _ ->
            let newAssignments = newAssigns texture assignment imgID
            in  if newAssignments /= assignments texture then
                  updateAssignments (newAssigns texture assignment imgID) texture
                else
                  texture
  where
    oldAssigns texture = assignments texture
    newAssigns tex assignment tID
      | texID tex /= tID && elem assignment (oldAssigns tex) = filter (/= assignment) (oldAssigns tex)
      | texID tex == tID && not (elem assignment (oldAssigns tex)) = assignment : (oldAssigns tex)
      | otherwise = oldAssigns tex


filterVideoTextures :: (Texture -> Bool) -> [Texture] -> [Texture]
filterVideoTextures eqFn textures = flip filter textures $ \tex ->
  case tex of Vid texture   -> eqFn $ Vid texture
              LVd texture   -> eqFn $ LVd texture
              _imageTexture -> False

filterImageTextures :: (ImageTexture -> Bool) -> [Texture] -> [Texture]
filterImageTextures eqFn textures = flip filter textures $ \tex ->
  case tex of Img texture   -> eqFn texture
              _videoTexture -> False

dropVideoTexture :: Int -> [Texture] -> [Texture]
dropVideoTexture tID textures = flip filter textures $ \tex ->
  case tex of Vid texture   -> tID /= texID texture
              -- LVd texture   -> tID /= texID texture
              _imageTexture -> True

dropImageTexture :: Int -> [Texture] -> [Texture]
dropImageTexture tID textures = flip filter textures $ \tex ->
  case tex of Img texture   -> tID /= texID texture
              _videoTexture -> True
