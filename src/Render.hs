module Render (compile, render, setupRendering) where


-- import Prelude (putStrLn)
import RIO
import qualified RIO.Text as Text
import RIO.List.Partial (head)
import qualified RIO.Map as Map
import RIO.Partial (fromJust)

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
import Text.Pretty.Simple (pPrint)

import GLUtils
import MyPrelude
import Playback
import Shader
import Types


setupRendering :: IORef WindowState -> IO RenderState
setupRendering wsRef = do
  ws <- readIORef wsRef
  initShaderProgram <- compileShaderProgram vertexShader defaultFragShader
  screenShader <- compileShaderProgram screenVertShader screenFragShader

  (vao, _vbo, _ebo) <- setupGeometry

  GL.currentProgram $= Just initShaderProgram

  -- create a default output bus
  (defaultFbo, defaultTexOut) <- setupFramebuffer (wsWidth ws) (wsHeight ws)
  let defaultBus = Bus defaultFbo defaultTexOut

  GL.bindFramebuffer GL.Framebuffer $= GL.defaultFramebufferObject

  maxTexUnits <- GL.maxTextureImageUnits

  return $ RenderState { rsWindowState = wsRef
                       , rsScreenShader = screenShader
                       , rsVAO = vao
                       , rsDefaultOutBus = defaultBus
                       , rsMaxTexUnits = maxTexUnits
                       }



render :: RIO RenderState RenderState
render = ask >>= \env -> liftIO $ do
  ws <- readIORef $ rsWindowState env
  {- Update texture and float uniforms.
  -}
  nodeTree' <- readIORef $ wsNodeTree ws
  buses <- readIORef $ wsBuses ws

  uniforms <- atomically $ STM.flushTBQueue $ wsUniformVals ws
  let uniformUpdates = IntMap.fromListWith (++)
                     $ map (\unitData -> (uDataNodeID unitData, [unitData])) uniforms

  let Bus outBusFbo outBusTObj = rsDefaultOutBus env
  GL.bindFramebuffer GL.Framebuffer $= outBusFbo
  GL.clearNamedFramebuffer outBusFbo
    $ GL.ClearColorBufferFloat 0 $ GL.Color4 0.0 0.0 0.0 1.0

  forM_ (recurseNodeTree nodeTree') $ \(nID, ShaderProgram shaderProgram inWires outWire) -> do
    GL.currentProgram $= Just shaderProgram

    -- TODO?: it may be possible to use texture unit 0 here but since it is the
    --        texture unit used for the final 'screen shader' rendering step, it
    --        causes visual glitches. I'm not exactly sure what is going on
    --        there right now and losing one possible texture unit is still a
    --        big improvement over the previous implementation.
    --        It might be possible to fix this by carefully working out how and
    --        when texture objects are bound to the texture units for the shader
    --        programs.
    let texUnitMin = (fromIntegral $ length inWires) + 1
        texUnitMax = (fromIntegral $ rsMaxTexUnits env) - 1
    textureUnits <- newIORef $ map GL.TextureUnit [texUnitMin..texUnitMax]

    let shaderState = ShaderState { ssShaderProgram = shaderProgram
                                  , ssTextureUnits  = textureUnits
                                  }

    forM_ (fromMaybe [] $ IntMap.lookup nID uniformUpdates) $ \u -> do
      let gID   = uDataNodeID u
          uID   = uDataUnitID u
          input = uDataInput u
          name = uniformName gID uID input

      -- TODO: note - this attempts to set every uniform for the node in each
      --       shader program. may be quite wasteful if a node is split into
      --       many shader programs.
      --       Maybe a good solution is to store information about the graphs
      --       and units in the shader program to allow a fast lookup by gID,
      --       uID before attempting to set the uniform.
      setFloatUniform shaderProgram name (uDataValue u)

    images  <- readIORef $ wsImages  ws
    players <- readIORef $ wsPlayers ws
    delBufs <- readIORef $ wsDelBufs ws

    -- TODO: note - this attempts to set every texture in every shader program
    --       and silently fails for the cases that don't work. this is also
    --       wasteful and a better solution should be devised.
    --       Again, the above possible solution of looking up if the gID and uID
    --       are in the shader program would work.
    runRIO shaderState $ forM_ (map snd . IntMap.toList $ images) displayImage
    players' <- runRIO shaderState $ forM (Map.toList $ players) $ \(k, p) ->
                  play p >>= \case Just p' -> return $ Just (k, p')
                                   Nothing -> return Nothing
    runRIO shaderState $ forM_ delBufs $ \db -> do
      -- liftIO $ putStrLn $ "\n\n*** Debug: displaying delay buffer "+||db||+""
      -- liftIO $ putStrLn (show db)
      displayDelBuf db

    writeIORef (wsPlayers ws) $
      players' |> filter (isJust) |> map (fromJust) |> Map.fromList

    -- bind input bus(es) to texture units
    -- TODO?: as above, it might be possible to use texture unit 0 but will need
    --        to figure out how that interacts with other texture unit
    --        assignments to avoid the visual glitches.
    forM_ (zip [1..] inWires) $ \(i, wireID) ->
      let Bus _ tObj = buses Map.! (nID, wireID)
      in  bindInputBus shaderProgram i wireID tObj

    let Bus out _ = if outWire == (-1) then rsDefaultOutBus env
                                       else buses Map.! (nID, outWire)
    -- putStrLn $ "*** Debug: rendering to wire "+|outWire|+", bus "+||b||+""
    GL.bindFramebuffer GL.Framebuffer $= out
    GL.clearNamedFramebuffer out
      $ GL.ClearColorBufferFloat 0 $ GL.Color4 0.0 0.0 0.0 1.0

    GL.currentProgram $= Just shaderProgram
    GL.bindVertexArrayObject $= Just (rsVAO env)
    GL.drawElements GL.Triangles 6 GL.UnsignedInt nullPtr

  -- rotate the frames in the delay buffers to move them along by one
  modifyIORef' (wsDelBufs ws) $ \dbs ->
                 dbs
                 |> IntMap.toList
                 |> map (\(k, db) -> (k, db { dbBuses = rotate (dbBuses db) }))
                 |> IntMap.fromList

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



compile :: SubGraph -> RIO RenderState ShaderProgram
compile subGraph@(SubGraph units _ _) = ask >>= \env -> liftIO $ do
  ws    <- readIORef $ rsWindowState env
  buses <- readIORef $ wsBuses ws

  let (FragShader fragShader inputs output) = generateFragShader subGraph
      nID = (unitNodeID.head) units
      inWires = flip map inputs $ \(LBus inWireID _ _) -> inWireID

  pPrint (FragShader fragShader inputs output)

  shaderProg <- compileShaderProgram vertexShader (Text.encodeUtf8 fragShader)

  case output of
    Just (LBus outWireID _ _) -> do
      (fb, tObj) <- setupFramebuffer (wsWidth ws) (wsHeight ws)
      let bus   = Bus fb tObj
          buses' = Map.insert (nID, outWireID) bus buses

      writeIORef (wsBuses ws) buses'

      return $ ShaderProgram shaderProg inWires outWireID
    _ -> return $ ShaderProgram shaderProg inWires (-1)



recurseNodeTree :: IntMap Node -> [(NodeID, ShaderProgram)]
recurseNodeTree nodeTree = IntMap.foldrWithKey recurseNodes [] nodeTree
  where
    recurseNodes :: NodeID -> Node -> [(NodeID, ShaderProgram)] -> [(NodeID, ShaderProgram)]
    recurseNodes nID node shaderProgs =
      case node of Node  sps _ -> recurseShaderProgs nID sps shaderProgs
                   Group nodes -> (recurseNodeTree nodes) ++ shaderProgs

    recurseShaderProgs _   []     shaderProgs = shaderProgs
    recurseShaderProgs nID (x:xs) shaderProgs = (nID, x) : recurseShaderProgs nID xs shaderProgs
