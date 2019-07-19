module Transform (WindowState(..), recurseNodeTree, compile) where


-- import Control.Monad.Extra
import Data.ByteString.Char8 (pack)
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Data.Map (Map)
import qualified Data.Map as Map
-- import Data.IORef (IORef)
-- import qualified Data.IntMap.Strict as IntMap
-- import Data.List.Split (splitWhen)
-- import qualified Graphics.Rendering.OpenGL as GL

import GLUtils
import Shader
import Types


-- move to Window. Window.State
data WindowState =
  WindowState { wsWidth  :: Int
              , wsHeight :: Int
              -- , shaderPrograms :: IORef [ShaderProgram]
              -- , defaultFBO :: GL.FramebufferObject
              , wsBuses :: Map (NodeID, WireID) Bus
              , wsDefaultOutBus :: OutBus
              } deriving (Show)


-- generateShaderPrograms :: WindowState -> IntMap Node -> IO [ShaderProgram]
-- generateShaderPrograms ws nodeTree = mapM (compile ws) (recurseNodeTree nodeTree)

recurseNodeTree :: IntMap Node -> [(NodeID, ShaderProgram)]
recurseNodeTree nodeTree = IntMap.foldrWithKey recurseNodes [] nodeTree
  where
    recurseNodes :: NodeID -> Node -> [(NodeID, ShaderProgram)] -> [(NodeID, ShaderProgram)]
    recurseNodes nID node shaderProgs =
      case node of Node  sps _ -> recurseShaderProgs nID sps shaderProgs
                   Group nodes -> (recurseNodeTree nodes) ++ shaderProgs

    recurseShaderProgs _   []     shaderProgs = shaderProgs
    recurseShaderProgs nID (x:xs) shaderProgs = (nID, x) : recurseShaderProgs nID xs shaderProgs

compile :: WindowState -> SubGraph -> IO (WindowState, ShaderProgram)
compile ws subGraph@(SubGraph units _ _) =
  let fragShader = generateFragShader subGraph
      sgNodeID   = (nodeID.head) units
  in  compileFragShader sgNodeID fragShader
  where
    compileFragShader nID (FragShader fragShader inputs output) = do
      shaderProg <- compileShaderProgram vertexShader (pack fragShader)

      let inBuses = flip map inputs $ \(LBus inWireID _ _) ->
                      InBus inWireID $ wsBuses ws Map.! (nID, inWireID)

      case output of
        Just (LBus outWireID _ _) -> do
          (fb, tObj) <- setupFramebuffer (wsWidth ws) (wsHeight ws)
          let bus   = Bus fb tObj
              buses = Map.insert (nID, outWireID) bus $ wsBuses ws

          return $ ( ws { wsBuses = buses }
                   , ShaderProgram shaderProg inBuses $ OutBus outWireID bus
                   )
        _ -> return $ (ws, ShaderProgram shaderProg inBuses (wsDefaultOutBus ws))


-- partition :: SCGraph -> [SubGraph]
-- partition graph =
--   let splitGraph = splitWhen requiresPartition graph -- [[SCUnit]]
--   in  toSubGraphs splitGraph 0 []

-- TODO: does this work with the UGen graph in the notebook?
--       can the graph be treated as a linear array while achieving the correct behaviour?
-- toSubGraphs :: [[SCUnit]] -> Int -> [Input] -> [SubGraph]
-- toSubGraphs []     _ _      = []
-- toSubGraphs [x]    _ inputs = let out = OutGlobal 0
--                                   units = scUnitsToUnits x inputs out
--                               in  [SubGraph units inputs out]
-- toSubGraphs (x:xs) i inputs = let out = OutLocal i
--                                   units = scUnitsToUnits x inputs out
--                               in  (SubGraph units inputs out) : toSubGraphs xs (i+1) [InLocal i]
