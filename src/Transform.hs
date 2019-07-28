module Transform (recurseNodeTree, compile) where


import Data.ByteString.Char8 (pack)
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import qualified Data.Map as Map

import GLUtils
import Shader
import Types


{- TODO: move everything out of this module into relevant modules, or rename
         this one to something which makes more sense.
-}


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
