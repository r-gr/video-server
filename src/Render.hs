{-# LANGUAGE OverloadedStrings #-}

module Render (newWindow) where


import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad (forever)
import Control.Monad.Loops
-- import Data.ByteString.Char8 (pack, unpack)
import Data.Foldable (forM_, foldlM)
import Data.IntMap (IntMap)
import qualified Data.IntMap.Strict as IntMap
-- import Data.Map (Map)
import qualified Data.Map.Strict as Map
import Data.IORef
import Data.Maybe
import Fmt
import Foreign.Ptr (nullPtr)
import Graphics.Rendering.OpenGL (($=))
import qualified Graphics.Rendering.OpenGL as GL
import qualified Graphics.UI.GLFW as GLFW
import System.ZMQ4 (Socket, Sub)

import GLUtils
import Graph
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
    -- shaderProg    <- newIORef initShaderProgram
    -- shaderProgs   <- newIORef ([] :: [ShaderProgram])
    nodeTree      <- newIORef (IntMap.empty :: IntMap Node)
    shouldExit    <- newIORef False
    iter1Ref      <- newIORef True
    threadID      <- forkIO $ forever $ receiveDataMsg sock winID uniformVals textures textureQueue

    -- create a default output bus
    (defaultFbo, defaultTexOut) <- setupFramebuffer width height
    -- (defaultFbo', defaultTexOut') <- setupFramebuffer width height
    let defaultBus = Bus defaultFbo defaultTexOut
        -- defaultBus' = Bus defaultFbo' defaultTexOut'

    -- nodeTree <- newIORef $ IntMap.singleton (-1)
    --                      $ Node [ ShaderProgram initShaderProgram
    --                                             []
    --                                             (OutBus (-2) defaultBus')
    --                             ] []

    -- TODO: clearly, the state should change when the window is resized etc.
    stateRef <- newIORef $ WindowState { wsWidth = width
                                       , wsHeight = height
                                       , wsBuses = Map.empty
                                       , wsDefaultOutBus = OutBus (-1) defaultBus
                                       }

    -- create framebuffers to allow feedback textures
    (fb, texColBuf)   <- setupFramebuffer width height
    (fb', texColBuf') <- setupFramebuffer width height
    GL.bindFramebuffer GL.Framebuffer $= GL.defaultFramebufferObject

    -- putStrLn "*** Debug: done with the main GL setup, entering render loop"

    -- main render loop
    whileM_ (fmap not $ readIORef shouldExit) $ do
      wsc <- GLFW.windowShouldClose window
      nodeTree' <- readIORef nodeTree
      let ugens         = concatMap (\(Node _ us) -> us) $ IntMap.elems nodeTree'
          prevFrameUGen = 0 == (length $ filter (\u -> scUnitName u == "GLPrevFrame" || scUnitName u == "GLPrevFrame2") $ ugens)

      if wsc then do
        closeWindow winID textures shouldExit
        atomically $ writeTQueue msgQOut $ InternalWindowFree winID
      else if prevFrameUGen then do

        -- putStrLn "*** Debug:   processing input"
        processInput window monitor videoMode isFullscreen

        {- If there's a message on the queue, pop it off and perform it in
           some manner.
        -}
        -- putStrLn "*** Debug:   processing server commands"
        processCommands textures shouldExit nodeTree msgQIn stateRef

        -- putStrLn "*** Debug:   rendering without feedback"
        renderNoFB textures nodeTree stateRef uniformVals textureQueue screenShader vao window

      else do

        processInput window monitor videoMode isFullscreen
        processCommands textures shouldExit nodeTree msgQIn stateRef

        iter1 <- readIORef iter1Ref
        if iter1 then do
          -- write to fb', read texColBuf, write to texColBuf'
          renderFB fb' texColBuf texColBuf'
            screenShader textures nodeTree stateRef uniformVals textureQueue vao window
          writeIORef iter1Ref False
        else do
          -- write to fb, read texColBuf', write to texColBuf
          renderFB fb texColBuf' texColBuf
            screenShader textures nodeTree stateRef uniformVals textureQueue vao window
          writeIORef iter1Ref True

    putStrLn $ "*** Info: Window "+|winID|+" - Killing sub socket thread"
    killThread threadID


renderNoFB :: TVar [Texture]
           -- -> IORef GL.Program
           -> IORef (IntMap Node)
           -> IORef WindowState
           -> TBQueue UnitData
           -> TBQueue TextureUpdate
           -> GL.Program
           -> GL.VertexArrayObject
           -> GLFW.Window
           -> IO ()
renderNoFB textures nodeTree stateRef uniformVals textureQueue ss vao window = do
  -- -- render to the default framebuffer
  -- GL.bindFramebuffer GL.Framebuffer $= GL.defaultFramebufferObject
  -- GL.clearColor $= GL.Color4 0.0 0.0 0.0 1.0
  -- GL.clear [GL.ColorBuffer]

  {- Update texture and float uniforms.
  -}
  nodeTree' <- readIORef nodeTree

  textureUpdates <- atomically $ flushTBQueue textureQueue
  forM_ textureUpdates (applyTextureUpdate textures)

  uniforms <- atomically $ flushTBQueue uniformVals
  let uniformUpdates = IntMap.fromListWith (++)
                     $ map (\unitData -> (uDataNodeID unitData, [unitData])) uniforms

  -- pPrint uniformUpdates

  forM_ (recurseNodeTree nodeTree') $ \(nID, ShaderProgram shaderProgram inBuses (OutBus _wireID (Bus out _))) -> do
    -- putStrLn "*** Debug: rendering stuff in the node tree"
    GL.currentProgram $= Just shaderProgram
    forM_ (fromMaybe [] $ IntMap.lookup nID uniformUpdates) $ \u -> do
      let gID   = fromIntegral (uDataNodeID u)  :: Int
          uID   = fromIntegral (uDataUnitID u)  :: Int
          input = fromIntegral (uDataInput u)   :: Int
          name = uniformName gID uID input
      -- putStrLn $ "*** Debug: performing data msg. " ++ (show u)

      -- TODO: note - this attempts to set every uniform for the node in each
      --       shader program. may be quite wasteful if a node is split into many
      --       shader programs. it will also spit out debug output so should fix
      --       this at some point.
      setFloatUniform' shaderProgram name (uDataValue u)

    -- TODO: note - this attempts to set every texture in every shader program
    --       and silently fails for the cases that don't work. this is also
    --       wasteful and a better solution should be devised.
    textures'  <- readTVarIO textures
    textures'' <- updateTextures shaderProgram (length inBuses) textures'
    atomically $ modifyTVar' textures $ \_ -> textures''

    -- TODO: bind input bus(es) to texture units
    forM_ (zip [0..] inBuses) $ \(i, InBus wireID (Bus _ tObj)) ->
      bindInputBus shaderProgram i wireID tObj

    GL.bindFramebuffer GL.Framebuffer $= out
    GL.clearColor $= GL.Color4 0.0 0.0 0.0 1.0
    GL.clear [GL.ColorBuffer]
    GL.currentProgram $= Just shaderProgram
    GL.bindVertexArrayObject $= Just vao
    GL.drawElements GL.Triangles 6 GL.UnsignedInt nullPtr


  textureObj <- readIORef stateRef
                >>= return . (\(OutBus _ (Bus _ tObj)) -> tObj) . wsDefaultOutBus


  GL.bindFramebuffer GL.Framebuffer $= GL.defaultFramebufferObject
  GL.clearColor $= GL.Color4 0.0 0.0 0.0 1.0
  GL.clear [GL.ColorBuffer]

  GL.currentProgram $= Just ss
  GL.bindVertexArrayObject $= Just vao
  GL.activeTexture $= (GL.TextureUnit 0)
  GL.textureBinding GL.Texture2D $= Just textureObj
  GL.drawElements GL.Triangles 6 GL.UnsignedInt nullPtr

  GLFW.swapBuffers window
  GLFW.pollEvents
  -- putStrLn "*** Debug: end of render loop"


renderFB :: GL.FramebufferObject
         -> GL.TextureObject
         -> GL.TextureObject
         -> GL.Program
         -> TVar [Texture]
         -- -> IORef GL.Program
         -> IORef (IntMap Node)
         -> IORef WindowState
         -> TBQueue UnitData
         -> TBQueue TextureUpdate
         -> GL.VertexArrayObject
         -> GLFW.Window
         -> IO ()
renderFB fb tb1 tb2 ss textures nodeTree stateRef uniformVals textureQueue vao window = do
  -- -- render to the background framebuffer
  -- GL.bindFramebuffer GL.Framebuffer $= fb
  -- GL.clearColor $= GL.Color4 0.0 0.0 0.0 1.0
  -- GL.clear [GL.ColorBuffer]

  {- Update texture and float uniforms.
  -}
  nodeTree' <- readIORef nodeTree

  textureUpdates <- atomically $ flushTBQueue textureQueue
  forM_ textureUpdates (applyTextureUpdate textures)

  uniforms <- atomically $ flushTBQueue uniformVals
  let uniformUpdates = IntMap.fromListWith (++)
                     $ map (\unitData -> (uDataNodeID unitData, [unitData])) uniforms

  forM_ (recurseNodeTree nodeTree') $ \(nID, ShaderProgram shaderProgram inBuses (OutBus _wireID (Bus out _))) -> do
    GL.currentProgram $= Just shaderProgram
    forM_ (fromMaybe [] $ IntMap.lookup nID uniformUpdates) $ \u -> do
      let gID   = fromIntegral (uDataNodeID u)  :: Int
          uID   = fromIntegral (uDataUnitID u)  :: Int
          input = fromIntegral (uDataInput u)   :: Int
          name = uniformName gID uID input
      -- putStrLn $ "*** Debug: performing data msg. " ++ (show u)

      -- TODO: note - this attempts to set every uniform for the node in each
      --       shader program. may be quite wasteful if a node is split into many
      --       shader programs. it will also spit out debug output so should fix
      --       this at some point.
      setFloatUniform' shaderProgram name (uDataValue u)

    -- TODO: note - this attempts to set every texture in every shader program
    --       and silently fails for the cases that don't work. this is also
    --       wasteful and a better solution should be devised.
    textures'  <- readTVarIO textures
    textures'' <- updateTextures shaderProgram (length inBuses + 1) textures'
    atomically $ modifyTVar' textures $ \_ -> textures''

    -- bind previous frame texture
    -- TODO: only bind it in the subgraph which contains the GLPrevFrame UGen?
    --       That seems to be an optimisation for a bad design though so probably
    --       best to redesign how feedback works.
    GL.activeTexture $= (GL.TextureUnit 0)
    GL.textureBinding GL.Texture2D $= Just tb1

    -- bind input bus(es) to texture units
    forM_ (zip [0..] inBuses) $ \(i, InBus wireID (Bus _ tObj)) ->
      bindInputBus shaderProgram i wireID tObj

    GL.bindFramebuffer GL.Framebuffer $= out
    GL.clearColor $= GL.Color4 0.0 0.0 0.0 1.0
    GL.clear [GL.ColorBuffer]
    GL.currentProgram $= Just shaderProgram
    GL.bindVertexArrayObject $= Just vao
    GL.drawElements GL.Triangles 6 GL.UnsignedInt nullPtr


  textureObj <- readIORef stateRef
                >>= return . (\(OutBus _ (Bus _ tObj)) -> tObj) . wsDefaultOutBus

  -- render to the background framebuffer
  GL.bindFramebuffer GL.Framebuffer $= fb
  GL.clearColor $= GL.Color4 0.0 0.0 0.0 1.0
  GL.clear [GL.ColorBuffer]

  GL.currentProgram $= Just ss
  GL.bindVertexArrayObject $= Just vao
  GL.activeTexture $= (GL.TextureUnit 0)
  GL.textureBinding GL.Texture2D $= Just textureObj
  GL.drawElements GL.Triangles 6 GL.UnsignedInt nullPtr

  GLFW.swapBuffers window
  GLFW.pollEvents

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
                -- -> IORef GL.Program
                -> IORef (IntMap Node)
                -> TQueue Msg
                -> IORef WindowState
                -> IO ()
processCommands textures shouldExit nodeTree msgQIn stateRef = do
  isEmpty <- atomically $ isEmptyTQueue msgQIn
  if isEmpty
    then return ()
    else do
      msg <- atomically $ readTQueue msgQIn
      -- putStrLn $ "*** Debug: Window received message: "+|(show msg)|+""
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
            glUnits = filter (\u -> elem (scUnitName u) glUGenNames) gUnits

        -- putStrLn "*** Debug: window has received GraphNew message"
        -- pPrint glUnits

        -- convert SCGraph to SubGraphs
        subGraphs <- partition glUnits
        state <- readIORef stateRef
        (state', shaders) <- foldlM (\(ws, sProgs) sg -> do
                                      (ws', sProg) <- compile ws sg
                                      return (ws', sProgs ++ [sProg])
                                    )
                                    (state, [])
                                    subGraphs

        modifyIORef nodeTree $ \nt -> IntMap.insert gID (Node shaders glUnits) nt
        writeIORef stateRef state'


      {- Delete the graph from the node tree and generate a new shader program
         from the updated node tree
      -}
      GraphFree gID -> do
        modifyIORef nodeTree $ \nt -> IntMap.delete gID nt

        {- TODO: if the node tree is empty, insert a 'blank' shader program.
        -}

        -- nodeTree' <- readIORef nodeTree

        -- shaderProgs <- generateShaderPrograms state nodeTree'

        -- writeIORef (shaderPrograms state) shaderProgs

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
