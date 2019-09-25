module Window
  ( newWindow
  ) where


import MyPrelude
import RIO
import qualified RIO.List as List
import qualified RIO.Map as Map
import RIO.Partial (fromJust)
import qualified RIO.Seq as Seq

import Control.Concurrent
import qualified Control.Concurrent.STM as STM
import Control.Monad.Loops
import Data.Foldable (foldlM)
import qualified Data.IntMap.Strict as IntMap
import Graphics.Rendering.OpenGL (($=))
import qualified Graphics.Rendering.OpenGL as GL
import qualified Graphics.UI.GLFW as GLFW
import System.ZMQ4 (Socket, Sub)
import Text.Printf (printf)

import GLUtils
import Graph
import Msg
import Render
import Shader
import Types


newWindow :: Socket Sub -> TBQueue ExternalMsg -> TBQueue Msg -> WindowID -> Int -> Int -> RIO () ()
newWindow sock msgQIn msgQOut winID width height =
  withWindow width height ("window " ++ show winID) $ \window -> do
    shouldExit <- newIORef False

    monitor <- GLFW.getPrimaryMonitor
    let videoMode = GLFW.VideoMode width height 8 8 8 60
    isFullscreen <- newIORef False

    uniformVals <- newTBQueueIO 1000 :: IO (TBQueue UnitData)
    updateQueue <- newTBQueueIO 1000 :: IO (TBQueue WindowUpdate)
    images      <- newIORef (IntMap.empty :: IntMap ImageTexture) -- note: this might not need to be a TVar.
    videos      <- newIORef (IntMap.empty :: IntMap Video)        -- same here
    players     <- newIORef (Map.empty :: Map Assignment Player)
    delBufs     <- newIORef (IntMap.empty :: IntMap DelBuf)
    nodeTree    <- newIORef (IntMap.empty :: IntMap Node)
    initBuses   <- newIORef Map.empty

    -- TODO: clearly, the state should change when the window is resized etc.
    stateRef <- newIORef $ WindowState { wsWindow = window
                                       , wsWindowID = winID
                                       , wsWidth = width
                                       , wsHeight = height
                                       , wsShouldExit = shouldExit
                                       , wsMsgQIn = msgQIn
                                       , wsMsgQOut = msgQOut
                                       , wsSubSocket = sock
                                       , wsImages = images
                                       , wsVideos = videos
                                       , wsPlayers = players
                                       , wsDelBufs = delBufs
                                       , wsBuses = initBuses
                                       , wsNodeTree = nodeTree
                                       , wsUniformVals = uniformVals
                                       , wsUpdateQueue = updateQueue
                                       }

    -- FPS counter setup
    initTime  <- GLFW.getTime
    lastTime  <- newIORef (fromJust initTime)
    numFrames <- newIORef (0.0 :: Double)

    renderStateRef <- setupRendering stateRef >>= newIORef

    initWindowState <- readIORef stateRef
    threadID        <- forkIO $ forever $ runRIO initWindowState receiveDataMsg

    -- main render loop
    whileM_ (fmap not $ readIORef shouldExit) $ do
      wsc <- GLFW.windowShouldClose window

      -- set window title to current FPS
      -- TODO: handle case where getTime returns Nothing
      currentTime <- GLFW.getTime >>= return.fromJust
      modifyIORef' numFrames (+ 1.0)
      lastTime' <- readIORef lastTime
      if currentTime - lastTime' >= 1.0 then do
        numFrames' <- readIORef numFrames
        setWindowTitle window $ numFrames' / (currentTime - lastTime')
        writeIORef numFrames 0.0
        writeIORef lastTime currentTime
      else return ()

      if wsc then do
        windowState <- readIORef stateRef
        runRIO windowState closeWindow
        atomically $ writeTBQueue msgQOut $ Internal (WindowFree winID)
      else do
        windowState <- readIORef stateRef
        renderState <- readIORef renderStateRef
        processInput window monitor videoMode isFullscreen
        runRIO windowState applyUpdates
        runRIO renderState processCommands
        renderState' <- runRIO renderState render
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
      GLFW.windowHint $ GLFW.WindowHint'ContextVersionMajor 3
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



closeWindow :: RIO WindowState ()
closeWindow = ask >>= \env -> liftIO $ do
  images  <- readIORef $ wsImages  env
  videos  <- readIORef $ wsVideos  env
  players <- readIORef $ wsPlayers env

  forM_ (IntMap.toList images) $ GL.deleteObjectName . iTexObj . snd
  forM_ (IntMap.toList videos) $ freeVideoResources . snd
  cleanupPlayers players

  modifyIORef' (wsShouldExit env) (\_-> True)
  where
    cleanupPlayers players =
      let basicPlayers = Map.toList players
                       |> filter (\(_, p) -> isPlayVid p)
                       |> map (\(_, PlayVid bp) -> bp)
      in  forM_ basicPlayers $ \bp -> case bpOnDiskPlaybackTools bp of
                                        Nothing  -> return ()
                                        Just pts -> odptCleanupFFmpeg pts



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
      GLWindowFree _winID -> runRIO env closeWindow

      GLVideoNew vidID vPath _winID -> liftIO $ do
        videos  <- readIORef $ wsVideos env
        players <- readIORef $ wsPlayers env

        -- ensure any BasicPlayback players have their FFmpeg cleanup run
        -- and replace the playbackTools and startTime with Nothing
        players' <- findPlayersByVidID vidID players |> resetPlayers

        case IntMap.lookup vidID videos of
          Just v  -> freeVideoResources v
          Nothing -> return ()

        let video = OnDiskVid $ VideoFile { vID = vidID, vFilePath = vPath }

        writeIORef (wsPlayers env) players'
        modifyIORef' (wsVideos env) $ IntMap.insert vidID video


      GLVideoRead vidID vPath _winID -> liftIO $ do
        videos  <- readIORef $ wsVideos env
        players <- readIORef $ wsPlayers env

        -- ensure any BasicPlayback players have their FFmpeg cleanup run
        -- and replace the playbackTools and startTime with Nothing
        players' <- findPlayersByVidID vidID players |> resetPlayers

        case IntMap.lookup vidID videos of
          Just v  -> freeVideoResources v
          Nothing -> return ()

        putStrLn $ "*** Info: reading video into memory with ID "+|vidID|+" ("+|vPath|+")"

        -- TODO: check for sufficient available memory

        video <- loadVideo vidID vPath

        putStrLn $ "*** Info: finished reading video "+|vidID|+" into memory"

        writeIORef (wsPlayers env) players'
        modifyIORef' (wsVideos env) $ IntMap.insert vidID video


      GLVideoFree vidID _winID -> liftIO $ do
        videos  <- readIORef $ wsVideos env
        players <- readIORef $ wsPlayers env

        case IntMap.lookup vidID videos of
          Just v  -> freeVideoResources v
          Nothing -> return ()

        let videos' = IntMap.delete vidID videos
        players' <- findPlayersByVidID vidID players |> deletePlayers

        writeIORef (wsVideos env) videos'
        writeIORef (wsPlayers env) players'


      GLImageNew imgID iPath _winID -> liftIO $ do
        images <- readIORef (wsImages env)

        case IntMap.lookup imgID images of
          Just image -> GL.deleteObjectName $ iTexObj image
          Nothing    -> return ()

        tex <- newImageTexture imgID iPath
        case tex of
          Left errorStr -> putStrLn $ "*** Error: "+|errorStr|+""
          Right image -> modifyIORef' (wsImages env) $ IntMap.insert imgID image


      GLImageFree imgID _winID -> liftIO $ do
        images <- readIORef (wsImages env)

        case IntMap.lookup imgID images of
          Just image -> GL.deleteObjectName $ iTexObj image
          Nothing    -> return ()

        modifyIORef' (wsImages env) $ IntMap.delete imgID


      {- Add the received graph to the node tree and generate new shader program
         from that.
      -}
      GraphNew gID gUnits -> liftIO $ do
        let glUGenNames = map shaderName ugenShaderFns
            glUnits = filter (\u -> elem (scUnitName u) glUGenNames) gUnits

        -- putStrLn $ "*** Debug: glUGenNames = " ++ (show glUGenNames)

        -- convert SCGraph to SubGraphs
        subGraphs <- partition glUnits
        shaders <- foldlM (\sProgs sg -> runRIO rs (compile sg) >>= \sProg -> return $ sProgs ++ [sProg])
                          []
                          subGraphs

        modifyIORef' (wsNodeTree env) $ \nt -> IntMap.insert gID (Node shaders) nt


      {- Delete the graph from the node tree and generate a new shader program
         from the updated node tree
      -}
      GraphFree gID -> liftIO $ do
        modifyIORef' (wsNodeTree env) $ \nt -> IntMap.delete gID nt
        players <- readIORef $ wsPlayers env

        players' <- findPlayersByGphID gID players |> deletePlayers

        {- TODO: delete the framebuffer objects associated with the graph being
                 freed. glDeleteFramebuffers.
                 also clear/delete the associated textures
        -}

        writeIORef (wsPlayers env) players'


      DelBufNew bID bLen _winID -> do
        delBufs <- readIORef $ wsDelBufs env

        case IntMap.lookup bID delBufs of
          Nothing -> return ()
          Just (DelBuf _ _ buses) -> do
            putStrLn $ "*** Info: DelBufNew - freeing previous delay buffer with id = "+|bID|+""
            forM_ buses $ \(Bus fbo tObj) -> do
              GL.deleteObjectName fbo
              GL.deleteObjectName tObj

        let w = wsWidth env
            h = wsHeight env

        putStrLn $ "*** Info: DelBufNew - allocating new delay buffer, id = "+|bID|+", length = "+|bLen|+""

        assignmentTVar <- newIORef []
        delBuf <- Seq.replicateM (bLen + 1) (setupFramebuffer w h >>= return . (uncurry Bus))
                  >>= return . (DelBuf bID assignmentTVar)

        modifyIORef' (wsDelBufs env) $ IntMap.insert bID delBuf


      DelBufFree bID _winID -> do
        delBufs <- readIORef $ wsDelBufs env

        case IntMap.lookup bID delBufs of
          Nothing -> return ()
          Just (DelBuf _ _ buses) -> do
            putStrLn $ "*** Info: DelBufFree - freeing delay buffer with id = "+|bID|+""

            forM_ buses $ \(Bus fbo tObj) -> do
              GL.deleteObjectName fbo
              GL.deleteObjectName tObj

            modifyIORef' (wsDelBufs env) $ IntMap.delete bID


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



applyUpdates :: RIO WindowState ()
applyUpdates = do
  env <- ask
  updates <- atomically $ STM.flushTBQueue $ wsUpdateQueue env
  let updates' = List.take 100 . List.reverse $ updates -- List.nub updates
  forM_ updates' $ \update -> liftIO $ do
    case update of
      WUDelBufRd bufID assignment -> do
        delBufs <- readIORef $ wsDelBufs env

        case IntMap.lookup bufID delBufs of
          Nothing -> return ()
          Just db -> do
            assignments <- readIORef $ dbAssignments db
            let assignmentExists = elem assignment assignments

            if not assignmentExists then
              modifyIORef' (dbAssignments db) $ (:) assignment
            else
              return ()


      WUDelBufWr nID bufID wireID -> do
        delBufs <- readIORef $ wsDelBufs env
        buses   <- readIORef $ wsBuses env

        case IntMap.lookup bufID delBufs of
          Nothing -> return ()
          Just db -> do
            let hd Seq.:<| _ = dbBuses db
            case Map.lookup (nID, wireID) buses of
              Nothing  -> return ()
              Just bus -> if hd == bus then return () else
                modifyIORef' (wsBuses env) $ Map.insert (nID, wireID) hd


      WUAssignImage imgID assignment -> do
        images <- readIORef $ wsImages env
        case IntMap.lookup imgID images of
          Nothing  -> return ()
          Just img -> do
            let assignments = (iAssignments img)
                assignmentExists = elem assignment assignments

            if not assignmentExists then
              modifyIORef' (wsImages env) $
                IntMap.insert imgID (img { iAssignments = assignment : assignments })
            else
              return ()


      WUPlayVid vidID assignment rate loop -> do
        players <- readIORef $ wsPlayers env
        videos  <- readIORef $ wsVideos env

        -- Despite the crazy nesting this isn't complicated. is there a better way?
        case IntMap.lookup vidID videos of
          Nothing -> return ()
          Just v  ->
            case Map.lookup assignment players of
              Nothing ->
                addBasicPlayer (wsPlayers env) v assignment rate loop
              Just (VidRd _) ->
                addBasicPlayer (wsPlayers env) v assignment rate loop
              Just (PlayVid bp) -> if getVideoID bp == vidID
                then do
                  atomically $ writeTVar (bpRate bp) rate
                  atomically $ writeTVar (bpLoop bp) loop
                else do
                  addBasicPlayer (wsPlayers env) v assignment rate loop
                  -- free resources associated with old player
                  case bpOnDiskPlaybackTools bp of
                    Nothing  -> return ()
                    Just pts -> odptCleanupFFmpeg pts


      WUVidRd vidID assignment headPos -> do
        players <- readIORef $ wsPlayers env
        videos  <- readIORef $ wsVideos env

        case IntMap.lookup vidID videos of
          Nothing -> return ()
          Just v  ->
            case Map.lookup assignment players of
              Nothing ->
                addPlaybackHead (wsPlayers env) v assignment headPos
              Just (VidRd ph) ->
                atomically $ writeTVar (phHeadPos ph) headPos
              Just (PlayVid basicPlayer) -> do
                addPlaybackHead (wsPlayers env) v assignment headPos
                -- free resources associated with old player
                case bpOnDiskPlaybackTools basicPlayer of
                  Nothing  -> return ()
                  Just pts -> odptCleanupFFmpeg pts
  where
    addBasicPlayer players video assignment vRate vLoop = do
      rate <- newTVarIO vRate
      loop <- newTVarIO vLoop
      startTime <- newTVarIO Nothing
      let player = PlayVid $ BasicPlayer { bpVideo = video
                                         , bpAssignment = assignment
                                         , bpRate = rate
                                         , bpLoop = loop
                                         , bpStartTime = startTime
                                         , bpOnDiskPlaybackTools = Nothing
                                         , bpInMemPlaybackTools = Nothing
                                         }
      modifyIORef' players $ Map.insert assignment player

    addPlaybackHead players video assignment headPos = do
      headPosTVar <- newTVarIO headPos
      let player = VidRd $ PlaybackHead { phVideo = video
                                        , phAssignment = assignment
                                        , phHeadPos = headPosTVar
                                        }
      modifyIORef' players $ Map.insert assignment player




findPlayersByVidID :: Int -> Map Assignment Player -> (Map Assignment Player, [(Assignment, Player)])
findPlayersByVidID videoID players =
  (players, filter (\(_k, v) -> videoID == getVideoID v) $ Map.toList players)


findPlayersByGphID :: Int -> Map Assignment Player -> (Map Assignment Player, [(Assignment, Player)])
findPlayersByGphID graphID players =
  (players, filter (\((gID, _, _), _v) -> gID == graphID) $ Map.toList players)


resetPlayers :: (Map Assignment Player, [(Assignment, Player)]) -> IO (Map Assignment Player)
resetPlayers (playersMap, playersToReset) = do
  let basicPlayers = playersToReset |> filter (\(_, p) -> isPlayVid p) |> map (\(_, PlayVid bp) -> bp)
  freedBasicPlayers <- forM basicPlayers $ \bp -> do
    startTime <- newTVarIO Nothing
    case bpOnDiskPlaybackTools bp of
      Nothing  -> return $ bp { bpStartTime = startTime, bpInMemPlaybackTools = Nothing }
      Just pts -> do odptCleanupFFmpeg pts
                     return $ bp { bpOnDiskPlaybackTools = Nothing
                                 , bpInMemPlaybackTools = Nothing
                                 , bpStartTime = startTime
                                 }
  return $ foldr (\bp m -> Map.insert (bpAssignment bp) (PlayVid bp) m) playersMap freedBasicPlayers


deletePlayers :: (Map Assignment Player, [(Assignment, Player)]) -> IO (Map Assignment Player)
deletePlayers (playersMap, playersToDelete) = do
  let basicPlayers = playersToDelete |> filter (\(_, p) -> isPlayVid p) |> map (\(_, PlayVid bp) -> bp)
  forM_ basicPlayers $ \bp -> case bpOnDiskPlaybackTools bp of
                                Nothing  -> return ()
                                Just pts -> odptCleanupFFmpeg pts
  return $ playersToDelete
         |> map (\(_, p) -> p)
         |> foldr (\p m -> Map.delete (playerAssignment p) m) playersMap
