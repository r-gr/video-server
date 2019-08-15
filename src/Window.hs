module Window (newWindow) where


import Prelude (putStrLn)
import RIO
import RIO.Partial (fromJust)

import Control.Concurrent
import Control.Monad.Loops
import Data.Foldable (foldlM)
import Data.IntMap (IntMap)
import qualified Data.IntMap.Strict as IntMap
import Fmt
import Graphics.Rendering.OpenGL (($=))
import qualified Graphics.Rendering.OpenGL as GL
import qualified Graphics.UI.GLFW as GLFW
import System.ZMQ4 hiding (message, monitor)
import Text.Printf (printf)

import GLUtils
import Graph
import Msg
import Render
import Shader
import Texture
import Types


newWindow :: Socket Sub -> TBQueue ExternalMsg -> TBQueue Msg -> WindowID -> Int -> Int -> RIO () ()
newWindow sock msgQIn msgQOut winID width height =
  withWindow width height ("window " ++ show winID) $ \window -> do
    shouldExit <- newIORef False

    monitor <- GLFW.getPrimaryMonitor
    let videoMode = GLFW.VideoMode width height 8 8 8 60
    isFullscreen <- newIORef False

    uniformVals  <- newTBQueueIO 1000 :: IO (TBQueue UnitData)
    textureQueue <- newTBQueueIO 1000 :: IO (TBQueue TextureUpdate)
    textures     <- newTVarIO ([] :: [Texture])
    nodeTree     <- newIORef (IntMap.empty :: IntMap Node)

    -- TODO: clearly, the state should change when the window is resized etc.
    stateRef <- newIORef $ WindowState { wsWindow = window
                                       , wsWidth = width
                                       , wsHeight = height
                                       , wsShouldExit = shouldExit
                                       , wsMsgQIn = msgQIn
                                       , wsMsgQOut = msgQOut
                                       , wsTextures = textures -- TODO: better data structure
                                       , wsNodeTree = nodeTree
                                       , wsUniformVals = uniformVals
                                       , wsTextureUpdates = textureQueue
                                       }

    -- FPS counter setup
    initTime  <- GLFW.getTime
    lastTime  <- newIORef (fromJust initTime)
    numFrames <- newIORef (0.0 :: Double)

    renderStateRef <- setupRendering stateRef >>= newIORef

    threadID <- forkIO $ forever $ receiveDataMsg sock winID uniformVals textures textureQueue

    -- main render loop
    whileM_ (fmap not $ readIORef shouldExit) $ do
      wsc <- GLFW.windowShouldClose window
      -- nodeTree' <- readIORef nodeTree
      -- let ugens         = concatMap (\(Node _ us) -> us) $ IntMap.elems nodeTree'
      --     prevFrameUGen = 0 == (length $ filter (\u -> scUnitName u == "GLPrevFrame" || scUnitName u == "GLPrevFrame2") $ ugens)

      -- set window title to current FPS
      -- TODO: handle case where getTime returns Nothing
      currentTime <- GLFW.getTime >>= return.fromJust
      modifyIORef numFrames (+ 1.0)
      lastTime' <- readIORef lastTime
      if currentTime - lastTime' >= 1.0 then do
        numFrames' <- readIORef numFrames
        setWindowTitle window $ numFrames' / (currentTime - lastTime')
        writeIORef numFrames 0.0
        writeIORef lastTime currentTime
      else return ()

      if wsc then do
        closeWindow winID textures shouldExit
        atomically $ writeTBQueue msgQOut $ Internal (WindowFree winID)
      -- else if prevFrameUGen then do

      --   processInput window monitor videoMode isFullscreen

      --   {- If there's a message on the queue, pop it off and perform it in
      --      some manner.
      --   -}
      --   processCommands textures shouldExit nodeTree msgQIn msgQOut stateRef

      --   renderNoFB textures nodeTree stateRef uniformVals textureQueue screenShader vao window

      -- else do

      --   processInput window monitor videoMode isFullscreen
      --   processCommands textures shouldExit nodeTree msgQIn msgQOut stateRef

      --   iter1 <- readIORef iter1Ref
      --   if iter1 then do
      --     -- write to fb', read texColBuf, write to texColBuf'
      --     renderFB fb' texColBuf texColBuf'
      --       screenShader textures nodeTree stateRef uniformVals textureQueue vao window
      --     writeIORef iter1Ref False
      --   else do
      --     -- write to fb, read texColBuf', write to texColBuf
      --     renderFB fb texColBuf' texColBuf
      --       screenShader textures nodeTree stateRef uniformVals textureQueue vao window
      --     writeIORef iter1Ref True
      else do
        renderState <- readIORef renderStateRef
        processInput window monitor videoMode isFullscreen
        runRIO renderState processCommands
        renderState' <- runRIO renderState renderNoFB
        writeIORef renderStateRef renderState'

    putStrLn $ "*** Info: Window "+|winID|+" - Killing sub socket thread"
    killThread threadID
  where
    setWindowTitle window fps = GLFW.setWindowTitle window $
      "window "+|winID |+" ("+|(printf "%.0f" fps :: String)|+" fps)"



withWindow :: Int -> Int -> String -> (GLFW.Window -> IO ()) -> RIO () ()
withWindow width height title fn = liftIO $ do
  successfulInit <- GLFW.init
  if not successfulInit
    then do
      putStrLn "*** Error: couldn't initialise GLFW context"
    else do
      GLFW.windowHint $ GLFW.WindowHint'ContextVersionMajor 4
      GLFW.windowHint $ GLFW.WindowHint'ContextVersionMinor 3
      GLFW.windowHint $ GLFW.WindowHint'OpenGLProfile GLFW.OpenGLProfile'Core
      -- GLFW.windowHint $ GLFW.WindowHint'RefreshRate $ Just 240

      window <- GLFW.createWindow width height ("scsynth-video '" ++ title ++ "'") Nothing Nothing
      case window of
        Nothing -> do
          GLFW.terminate
          putStrLn "*** Error: couldn't create GLFW window"
        Just w -> do
          GLFW.makeContextCurrent window
          GLFW.setFramebufferSizeCallback w $ Just framebufferSizeCallback
          fn w
          putStrLn $ "*** Info: Window '"+|title|+"' - Closing GLFW context"
          GLFW.destroyWindow w
          GLFW.terminate
  where
    framebufferSizeCallback :: GLFW.FramebufferSizeCallback
    framebufferSizeCallback _window w h =
      GL.viewport $= (GL.Position 0 0, GL.Size (fromIntegral w) (fromIntegral h))



closeWindow :: WindowID -> TVar [Texture] -> IORef Bool -> IO ()
closeWindow winID textures shouldExit = do
  textures' <- readTVarIO textures
  forM_ textures' $ freeTexture winID
  atomically $ modifyTVar' textures (\_ -> [])
  modifyIORef' shouldExit (\_-> True)



-- processCommands :: TVar [Texture]
--                 -> IORef Bool
--                 -> IORef (IntMap Node)
--                 -> TBQueue ExternalMsg
--                 -> TBQueue Msg
--                 -> IORef WindowState
--                 -> IO ()
-- processCommands textures shouldExit nodeTree msgQIn msgQOut stateRef = do
processCommands :: RIO RenderState ()
processCommands = do
  env <- ask >>= readIORef.rsWindowState
  isEmpty <- atomically $ isEmptyTBQueue $ wsMsgQIn env
  if isEmpty
    then return ()
    else do
      msg <- atomically $ readTBQueue (wsMsgQIn env)
      handleMsg msg
  where
    handleMsg :: ExternalMsg -> RIO RenderState ()
    handleMsg msg = ask >>= \rs -> (readIORef.rsWindowState) rs >>= \env -> liftIO $ case msg of
      GLWindowFree winID -> closeWindow winID (wsTextures env) (wsShouldExit env)

      GLVideoNew vidID vPath vPlaybackRate vShouldLoop winID -> do
        textures' <- readTVarIO $ wsTextures env

        -- delete any existing videos with this ID
        let matchingTextures = filterVideoTextures (\t -> texID t == vidID) textures'
        if length matchingTextures > 0 then do
          atomically $ modifyTVar' (wsTextures env) $ dropVideoTexture vidID
          forM_ matchingTextures $ freeTexture winID
        else
          return ()

        tex <- newVideoTexture vidID vPath vPlaybackRate vShouldLoop
        case tex of
          Left errorStr -> putStrLn $ "*** Error: "+|errorStr|+""
          Right texture -> atomically $ modifyTVar' (wsTextures env) $ \ts -> texture : ts


      GLVideoRead vidID vPath vPlaybackRate vShouldLoop winID -> do
        textures' <- readTVarIO (wsTextures env)

        -- delete any existing videos with this ID
        let matchingTextures = filterVideoTextures (\t -> texID t == vidID) textures'
        if length matchingTextures > 0 then do
          atomically $ modifyTVar' (wsTextures env) $ dropVideoTexture vidID
          forM_ matchingTextures $ freeTexture winID
        else
          return ()

        tex <- newLoadedVideo vidID vPath vPlaybackRate vShouldLoop
        case tex of
          Left errorStr -> putStrLn $ "*** Error: "+|errorStr|+""
          Right texture -> do
            putStrLn $ "*** Info: adding new loaded video texture - ID = " ++ (show $ texID texture)
            atomically $ modifyTVar' (wsTextures env) $ \ts -> texture : ts


      GLVideoFree vidID winID -> do
        textures' <- readTVarIO (wsTextures env)
        let matchingTextures = filterVideoTextures (\t -> texID t == vidID) textures'

        if length matchingTextures > 0 then do
          atomically $ modifyTVar' (wsTextures env) $ dropVideoTexture vidID
          forM_ matchingTextures $ freeTexture winID
        else
          return ()


      GLImageNew imgID iPath _winID -> do
        textures' <- readTVarIO (wsTextures env)

        let matchingTextures = filterImageTextures (\t -> iID t == imgID) textures'
        if length matchingTextures > 0 then
          atomically $ modifyTVar' (wsTextures env) $ dropImageTexture imgID
        else
          return ()

        tex <- newImageTexture imgID iPath
        case tex of
          Left errorStr -> putStrLn $ "*** Error: "+|errorStr|+""
          Right texture -> atomically $ modifyTVar' (wsTextures env) $ \ts -> texture : ts


      GLImageFree imgID _winID -> do
        textures' <- readTVarIO (wsTextures env)
        let matchingTextures = filterImageTextures (\t -> iID t == imgID) textures'

        if length matchingTextures > 0 then
          atomically $ modifyTVar' (wsTextures env) $ dropImageTexture imgID
        else
          return ()


      {- Add the received graph to the node tree and generate new shader program
         from that.
      -}
      GraphNew gID gUnits -> do
        let glUGenNames = map shaderName ugenShaderFns
            glUnits = filter (\u -> elem (scUnitName u) glUGenNames) gUnits

        -- convert SCGraph to SubGraphs
        subGraphs <- partition glUnits
        shaders <- foldlM (\sProgs sg -> runRIO rs (compile sg) >>= \sProg -> return $ sProgs ++ [sProg])
                          []
                          subGraphs

        modifyIORef (wsNodeTree env) $ \nt -> IntMap.insert gID (Node shaders glUnits) nt


      {- Delete the graph from the node tree and generate a new shader program
         from the updated node tree
      -}
      GraphFree gID -> do
        modifyIORef (wsNodeTree env) $ \nt -> IntMap.delete gID nt

        {- TODO: if the node tree is empty, insert a 'blank' shader program.
        -}

        {- TODO: delete the framebuffer objects associated with the graph being
                 freed. glDeleteFramebuffers.
                 also clear/delete the associated textures
        -}

        atomically $ modifyTVar' (wsTextures env) $ map $ \tex ->
          case tex of
            Vid texture ->
              let newAssigns = filter (\(gID',_,_) -> gID' /= gID) (assignments texture)
              in  if length newAssigns == 0 then
                    updateStartTime Nothing (updateAssignments [] tex)
                  else (updateAssignments [] tex)
            Img texture ->
              let newAssigns = filter (\(gID',_,_) -> gID' /= gID) (assignments texture)
              in  updateAssignments newAssigns tex
            LVd texture ->
              let newAssigns = filter (\(gID',_,_) -> gID' /= gID) (assignments texture)
              in  if length newAssigns == 0 then
                    updateStartTime Nothing (updateAssignments [] tex)
                  else (updateAssignments [] tex)


      InVidDur _reqID _vidID _winID -> undefined -- do
        -- textures' <- readTVarIO (wsTextures env)
        -- let matchingTextures = filterVideoTextures (\t -> texID t == vidID) textures'

        -- case matchingTextures of
        --   (v:_) ->
        --     let response = OutVidDur reqID $ vDur v
        --     in  atomically $ writeTBQueue msgQOut $ Internal (SendResponse response)
        --   _ -> return () -- TODO: return zero-valued message e.g. vidDur = 0


      InVidFrames _reqID _vidID _winID -> undefined


      InVidFrameRate _reqID _vidID _winID -> undefined


      _ -> return ()



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
