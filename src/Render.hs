module Render
  ( compile
  , render
  , setupRendering
  ) where


import MyPrelude
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
import Foreign.Ptr (nullPtr)
import Graphics.Rendering.OpenGL (($=))
import qualified Graphics.Rendering.OpenGL as GL
import qualified Graphics.UI.GLFW as GLFW
import Text.Pretty.Simple (pPrint)

import GLUtils
import Playback
import Shader
import Types


setupRendering :: IORef WindowState -> IO RenderState
setupRendering wsRef = do
  ws <- readIORef wsRef
  screenShader <- compileShaderProgram screenVertShader screenFragShader

  (vao, _vbo, _ebo) <- setupGeometry

  GL.currentProgram $= Just screenShader

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
  -- The uniform updates for each node must be ordered with the newest updates
  -- first and older updates later in the list. IntMap.fromListWith does this
  -- for us.
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
    let texUnitMin = 1
        texUnitMax = (fromIntegral $ rsMaxTexUnits env) - 1
    textureUnits <- newIORef $ map GL.TextureUnit [texUnitMin..texUnitMax]

    let shaderState = ShaderState { ssShaderProgram = shaderProgram
                                  , ssTextureUnits  = textureUnits
                                  }

    -- Apply any updates to simple Float uniforms. The uniform updates must be
    -- ordered with the newest updates first.
    runRIO shaderState $
      applyUniformUpdates [] (fromMaybe [] $ IntMap.lookup nID uniformUpdates)

    -- Bind input bus(es) to texture units
    forM_ inWires $ \wireID ->
      runRIO shaderState $ bindInputBus wireID $ buses Map.! (nID, wireID)

    images  <- readIORef $ wsImages  ws
    players <- readIORef $ wsPlayers ws
    delBufs <- readIORef $ wsDelBufs ws

    -- TODO: this attempts to set every texture in every shader program and
    --       silently fails for the cases that don't work. This is wasteful and
    --       a better solution should be devised.
    --          Again, the possible solution of looking up if the uID is in the
    --       shader program would work.
    runRIO shaderState $ forM_ (map snd . IntMap.toList $ images) displayImage
    players' <- runRIO shaderState $ forM (Map.toList $ players) $ \(k, p) ->
                  play p >>= \case Just p' -> return $ Just (k, p')
                                   Nothing -> return Nothing
    runRIO shaderState $ forM_ delBufs displayDelBuf

    writeIORef (wsPlayers ws) $
      players' |> filter (isJust) |> map (fromJust) |> Map.fromList

    let Bus out _ = if outWire == (-1) then rsDefaultOutBus env
                                       else buses Map.! (nID, outWire)

    GL.bindFramebuffer GL.Framebuffer $= out
    GL.clearNamedFramebuffer out
      $ GL.ClearColorBufferFloat 0 $ GL.Color4 0.0 0.0 0.0 1.0

    GL.bindVertexArrayObject $= Just (rsVAO env)
    GL.drawElements GL.Triangles 6 GL.UnsignedInt nullPtr

  -- Rotate the frames in the delay buffers to move them along by one
  modifyIORef' (wsDelBufs ws) $ \dbs ->
                 dbs
                 |> IntMap.toList
                 |> map (\(k, db) -> (k, db { dbBuses = rotate (dbBuses db) }))
                 |> IntMap.fromList

  GL.bindFramebuffer GL.Framebuffer $= GL.defaultFramebufferObject
  GL.clearNamedFramebuffer GL.defaultFramebufferObject
    $ GL.ClearColorBufferFloat 0 $ GL.Color4 0.0 0.0 0.0 1.0

  GL.currentProgram $= Just (rsScreenShader env)

  -- Unbind any texture units
  let texUnitMax    = (fromIntegral $ rsMaxTexUnits env) - 1
      textureUnits' = map GL.TextureUnit [0..texUnitMax]
  forM_ textureUnits' $ \t -> do GL.activeTexture $= t
                                 GL.textureBinding GL.Texture2D $= Nothing
  GL.activeTexture $= GL.TextureUnit 0

  GL.bindVertexArrayObject $= Just (rsVAO env)
  GL.activeTexture $= (GL.TextureUnit 0)
  GL.textureBinding GL.Texture2D $= Just outBusTObj
  GL.drawElements GL.Triangles 6 GL.UnsignedInt nullPtr

  GLFW.swapBuffers $ wsWindow ws
  GLFW.pollEvents

  return env
  where
    {- Note: the unit data list should be ordered with the newest updates first.

       This function recurses through the list of updates for a certain node and
       discards any updates for which a more recent update for that
       (unitID, inputID) pair has already been applied.
          Attempting to set a uniform is relatively costly so it is best to
       avoid repeating the work unnecessarily.
    -}
    applyUniformUpdates :: [(UnitID, Int)] -> [UnitData] -> RIO ShaderState ()
    applyUniformUpdates _done [] = return ()
    applyUniformUpdates done (u:us) = let uID = uDataUnitID u; uIn = uDataInput u in
      if elem (uID, uIn) done then
        applyUniformUpdates done us
      else do
        let gID  = uDataNodeID u
            name = uniformName gID uID uIn

        shaderProgram <- ask >>= return . ssShaderProgram

        -- TODO: this attempts to set the uniform regardless of whether it
        --       exists within this shader program. Given that attempting to set
        --       a uniform value is quite costly, it would be best to avoid
        --       doing this unnecessarily.
        --          Maybe a good solution is to store information about the
        --       units in the shader program to allow a fast lookup by uID
        --       before attempting to set the uniform.
        liftIO $ setFloatUniform shaderProgram name (uDataValue u)
        applyUniformUpdates ((uID, uIn) : done) us



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
      case node of Node  sps   -> recurseShaderProgs nID sps shaderProgs
                   Group nodes -> (recurseNodeTree nodes) ++ shaderProgs

    recurseShaderProgs _   []     shaderProgs = shaderProgs
    recurseShaderProgs nID (x:xs) shaderProgs = (nID, x) : recurseShaderProgs nID xs shaderProgs
