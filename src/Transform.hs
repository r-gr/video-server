module Transform (compile, WindowState(..)) where


import Control.Monad.Extra
import Data.ByteString.Char8 (pack)
import Data.IntMap (IntMap)
import qualified Data.IntMap.Strict as IntMap
import Data.List.Split (splitWhen)
-- import qualified Graphics.Rendering.OpenGL as GL

import GLUtils
import Shader
import Types


-- move to Window. Window.State
data WindowState =
  WindowState { width  :: Int
              , height :: Int
              -- , defaultFBO :: GL.FramebufferObject
              , defaultOutBus :: Bus
              }


generateShaderPrograms :: WindowState -> IntMap Node -> IO [ShaderProgram]
generateShaderPrograms ws nodeTree = concatMapM (compile ws) (recurseNodeTree nodeTree)

recurseNodeTree :: IntMap Node -> [Graph]
recurseNodeTree nodeTree = IntMap.foldl collectSynths [] nodeTree
  where
    collectSynths :: [Graph] -> Node -> [Graph]
    collectSynths graphs node =
      case node of Node  graph -> graphs ++ [graph]
                   Group nodes -> graphs ++ (recurseNodeTree nodes)

compile :: WindowState -> Graph -> IO [ShaderProgram]
compile ws graph =
  let subGraphs   = partition graph
      fragShaders = map generateFragShader subGraphs
  in  mapM compileFragShader fragShaders
  where
    compileFragShader (FragShader fragShader inputs _output) = do
      shaderProg <- compileShaderProgram vertexShader (pack fragShader)
      inBuses <- forM inputs $ \_ -> do
        (fb, tObj) <- setupFramebuffer (width ws) (height ws)
        return $ Bus fb tObj
      return $ ShaderProgram shaderProg inBuses (defaultOutBus ws)

partition :: Graph -> [SubGraph]
partition graph =
  let splitGraph = splitWhen requiresPartition graph -- [[Unit]]
  -- TODO: insert a GLOut where the graph gets split
  in  toSubGraphs splitGraph 0 []

toSubGraphs :: [[Unit]] -> Int -> [Input] -> [SubGraph]
toSubGraphs []     _ _      = []
toSubGraphs [x]    _ inputs = [SubGraph x inputs (OutGlobal 0)]
toSubGraphs (x:xs) i inputs = (SubGraph x inputs (OutLocal  i)) : toSubGraphs xs (i+1) [InLocal i]
