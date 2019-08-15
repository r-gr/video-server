module Render (compile, renderNoFB, renderFB, setupRendering) where


import RIO
import qualified RIO.Text as Text
import RIO.List.Partial (head)

import qualified Control.Concurrent.STM as STM
import Data.Foldable (forM_)
import Data.IntMap (IntMap)
import qualified Data.IntMap.Strict as IntMap
import qualified Data.Map.Strict as Map
-- import Fmt
import Foreign.Ptr (nullPtr)
import Graphics.Rendering.OpenGL (($=))
import qualified Graphics.Rendering.OpenGL as GL
import qualified Graphics.UI.GLFW as GLFW

import GLUtils
import Shader
import Texture
import Types


setupRendering :: IORef WindowState -> IO RenderState
setupRendering wsRef = do
  ws <- readIORef wsRef
  initShaderProgram <- compileShaderProgram vertexShader defaultFragShader
  screenShader <- compileShaderProgram screenVertShader screenFragShader

  (vao, _vbo, _ebo) <- setupGeometry

  GL.currentProgram $= Just initShaderProgram

  iter1Ref <- newIORef True
  initBuses <- newIORef Map.empty

  -- create a default output bus
  (defaultFbo, defaultTexOut) <- setupFramebuffer (wsWidth ws) (wsHeight ws)
  let defaultBus = Bus defaultFbo defaultTexOut

  -- create framebuffers to allow feedback textures
  (fb, texColBuf)   <- setupFramebuffer (wsWidth ws) (wsHeight ws)
  (fb', texColBuf') <- setupFramebuffer (wsWidth ws) (wsHeight ws)
  GL.bindFramebuffer GL.Framebuffer $= GL.defaultFramebufferObject

  return $ RenderState { rsWindowState = wsRef
                       , rsScreenShader = screenShader
                       , rsVAO = vao
                       , rsBuses = initBuses
                       , rsDefaultOutBus = defaultBus
                       , rsIter1 = iter1Ref
                       , rsTB1 = texColBuf
                       , rsFB1 = fb
                       , rsTB2 = texColBuf'
                       , rsFB2 = fb'
                       }



renderNoFB :: RIO RenderState RenderState
renderNoFB = ask >>= \env -> liftIO $ do
-- renderNoFB textures nodeTree stateRef uniformVals textureQueue ss vao window = do
  ws <- readIORef $ rsWindowState env
  {- Update texture and float uniforms.
  -}
  nodeTree' <- readIORef $ wsNodeTree ws

  textureUpdates <- atomically $ STM.flushTBQueue $ wsTextureUpdates ws
  forM_ textureUpdates $ applyTextureUpdate $ wsTextures ws

  uniforms <- atomically $ STM.flushTBQueue $ wsUniformVals ws
  let uniformUpdates = IntMap.fromListWith (++)
                     $ map (\unitData -> (uDataNodeID unitData, [unitData])) uniforms

  let Bus outBusFbo outBusTObj = rsDefaultOutBus env
  GL.bindFramebuffer GL.Framebuffer $= outBusFbo
  GL.clearNamedFramebuffer outBusFbo
    $ GL.ClearColorBufferFloat 0 $ GL.Color4 0.0 0.0 0.0 1.0

  forM_ (recurseNodeTree nodeTree') $ \(nID, ShaderProgram shaderProgram inBuses (OutBus _wireID (Bus out _))) -> do
    GL.currentProgram $= Just shaderProgram
    forM_ (fromMaybe [] $ IntMap.lookup nID uniformUpdates) $ \u -> do
      let gID   = uDataNodeID u
          uID   = uDataUnitID u
          input = uDataInput u
          name = uniformName gID uID input

      -- TODO: note - this attempts to set every uniform for the node in each
      --       shader program. may be quite wasteful if a node is split into many
      --       shader programs. it will also spit out debug output so should fix
      --       this at some point.
      setFloatUniform' shaderProgram name (uDataValue u)

    -- TODO: note - this attempts to set every texture in every shader program
    --       and silently fails for the cases that don't work. this is also
    --       wasteful and a better solution should be devised.
    textures'  <- readTVarIO $ wsTextures ws
    textures'' <- updateTextures shaderProgram (length inBuses) textures'
    atomically $ modifyTVar' (wsTextures ws) $ \_ -> textures''

    -- bind input bus(es) to texture units
    forM_ (zip [0..] inBuses) $ \(i, InBus wireID (Bus _ tObj)) ->
      bindInputBus shaderProgram i wireID tObj

    GL.bindFramebuffer GL.Framebuffer $= out
    GL.clearNamedFramebuffer out
      $ GL.ClearColorBufferFloat 0 $ GL.Color4 0.0 0.0 0.0 1.0

    GL.currentProgram $= Just shaderProgram
    GL.bindVertexArrayObject $= Just (rsVAO env)
    GL.drawElements GL.Triangles 6 GL.UnsignedInt nullPtr


  GL.bindFramebuffer GL.Framebuffer $= GL.defaultFramebufferObject
  GL.clearNamedFramebuffer GL.defaultFramebufferObject
    $ GL.ClearColorBufferFloat 0 $ GL.Color4 0.0 0.0 0.0 1.0

  GL.currentProgram $= Just (rsScreenShader env)
  GL.bindVertexArrayObject $= Just (rsVAO env)
  GL.activeTexture $= (GL.TextureUnit 0)
  GL.textureBinding GL.Texture2D $= Just outBusTObj
  GL.drawElements GL.Triangles 6 GL.UnsignedInt nullPtr

  GLFW.swapBuffers $ wsWindow ws
  GLFW.pollEvents

  return env



renderFB :: RIO RenderState RenderState
renderFB = ask >>= \env -> liftIO $ do
-- renderFB fb tb1 tb2 ss textures nodeTree stateRef uniformVals textureQueue vao window = do
  ws    <- readIORef $ rsWindowState env
  iter1 <- readIORef $ rsIter1 env

  {- Update texture and float uniforms.
  -}
  nodeTree' <- readIORef $ wsNodeTree ws

  textureUpdates <- atomically $ STM.flushTBQueue $ wsTextureUpdates ws
  forM_ textureUpdates $ applyTextureUpdate $ wsTextures ws

  uniforms <- atomically $ STM.flushTBQueue $ wsUniformVals ws
  let uniformUpdates = IntMap.fromListWith (++)
                     $ map (\unitData -> (uDataNodeID unitData, [unitData])) uniforms

  forM_ (recurseNodeTree nodeTree') $ \(nID, ShaderProgram shaderProgram inBuses (OutBus _wireID (Bus out _))) -> do
    GL.currentProgram $= Just shaderProgram
    forM_ (fromMaybe [] $ IntMap.lookup nID uniformUpdates) $ \u -> do
      let gID   = uDataNodeID u
          uID   = uDataUnitID u
          input = uDataInput u
          name = uniformName gID uID input

      -- TODO: note - this attempts to set every uniform for the node in each
      --       shader program. may be quite wasteful if a node is split into many
      --       shader programs. it will also spit out debug output so should fix
      --       this at some point.
      setFloatUniform' shaderProgram name (uDataValue u)

    -- TODO: note - this attempts to set every texture in every shader program
    --       and silently fails for the cases that don't work. this is also
    --       wasteful and a better solution should be devised.
    textures'  <- readTVarIO $ wsTextures ws
    textures'' <- updateTextures shaderProgram (length inBuses + 1) textures'
    atomically $ modifyTVar' (wsTextures ws) $ \_ -> textures''

    -- bind previous frame texture
    -- TODO: only bind it in the subgraph which contains the GLPrevFrame UGen?
    --       That seems to be an optimisation for a bad design though so probably
    --       best to redesign how feedback works.
    GL.activeTexture $= (GL.TextureUnit 0)
    GL.textureBinding GL.Texture2D $= Just (if iter1 then rsTB2 env else rsTB1 env)

    -- bind input bus(es) to texture units
    forM_ (zip [0..] inBuses) $ \(i, InBus wireID (Bus _ tObj)) ->
      bindInputBus shaderProgram i wireID tObj

    GL.bindFramebuffer GL.Framebuffer $= out
    GL.clearColor $= GL.Color4 0.0 0.0 0.0 1.0
    GL.clear [GL.ColorBuffer]
    GL.currentProgram $= Just shaderProgram
    GL.bindVertexArrayObject $= Just (rsVAO env)
    GL.drawElements GL.Triangles 6 GL.UnsignedInt nullPtr


  let Bus _ textureObj = rsDefaultOutBus env

  -- render to the background framebuffer
  GL.bindFramebuffer GL.Framebuffer $= (if iter1 then rsFB1 else rsFB2) env
  GL.clearColor $= GL.Color4 0.0 0.0 0.0 1.0
  GL.clear [GL.ColorBuffer]

  GL.currentProgram $= Just (rsScreenShader env)
  GL.bindVertexArrayObject $= Just (rsVAO env)
  GL.activeTexture $= (GL.TextureUnit 0)
  GL.textureBinding GL.Texture2D $= Just textureObj
  GL.drawElements GL.Triangles 6 GL.UnsignedInt nullPtr

  GLFW.swapBuffers $ wsWindow ws
  GLFW.pollEvents

  -- switch back to the default framebuffer and render the background fb's contents
  GL.bindFramebuffer GL.Framebuffer $= GL.defaultFramebufferObject
  GL.clearColor $= GL.Color4 0.0 0.0 0.0 1.0
  GL.clear [GL.ColorBuffer]

  GL.currentProgram $= Just (rsScreenShader env)
  GL.bindVertexArrayObject $= Just (rsVAO env)
  GL.activeTexture $= (GL.TextureUnit 0)
  GL.textureBinding GL.Texture2D $= Just textureObj
  GL.drawElements GL.Triangles 6 GL.UnsignedInt nullPtr

  GLFW.swapBuffers $ wsWindow ws
  GLFW.pollEvents

  return env



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



compile :: SubGraph -> RIO RenderState ShaderProgram
compile subGraph@(SubGraph units _ _) = ask >>= \env -> liftIO $ do
  ws    <- readIORef $ rsWindowState env
  buses <- readIORef $ rsBuses env

  let (FragShader fragShader inputs output) = generateFragShader subGraph
      nID = (unitNodeID.head) units
      inBuses = flip map inputs $ \(LBus inWireID _ _) ->
                  InBus inWireID $ buses Map.! (nID, inWireID)

  shaderProg <- compileShaderProgram vertexShader (Text.encodeUtf8 fragShader)

  case output of
    Just (LBus outWireID _ _) -> do
      (fb, tObj) <- setupFramebuffer (wsWidth ws) (wsHeight ws)
      let bus   = Bus fb tObj
          buses' = Map.insert (nID, outWireID) bus buses

      writeIORef (rsBuses env) buses'

      return $ ShaderProgram shaderProg inBuses $ OutBus outWireID bus
    _ -> return $ ShaderProgram shaderProg inBuses $ OutBus (-1) (rsDefaultOutBus env)



recurseNodeTree :: IntMap Node -> [(NodeID, ShaderProgram)]
recurseNodeTree nodeTree = IntMap.foldrWithKey recurseNodes [] nodeTree
  where
    recurseNodes :: NodeID -> Node -> [(NodeID, ShaderProgram)] -> [(NodeID, ShaderProgram)]
    recurseNodes nID node shaderProgs =
      case node of Node  sps _ -> recurseShaderProgs nID sps shaderProgs
                   Group nodes -> (recurseNodeTree nodes) ++ shaderProgs

    recurseShaderProgs _   []     shaderProgs = shaderProgs
    recurseShaderProgs nID (x:xs) shaderProgs = (nID, x) : recurseShaderProgs nID xs shaderProgs
