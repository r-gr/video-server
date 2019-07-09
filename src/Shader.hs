{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Shader ( Shader(..)
              , ugenShaderFns
              -- , embeddedShaders
              , defaultFragShader
              , vertexShader
              , screenVertShader
              , screenFragShader
              , uniformName
              , generateFragShader
              -- , generateGLSLCode
              ) where


-- import Control.Exception (catch)
import Control.Monad (forM)
import Data.ByteString (ByteString)
import Data.ByteString.Char8 (pack, unpack)
import Data.FileEmbed (embedDir, embedFile)
import Data.Foldable (foldl')
-- import Data.IntMap (IntMap)
-- import qualified Data.IntMap.Strict as IntMap
import Data.List (nub)
import Data.List.Split (splitOneOf)
import Data.List.Utils (endswith)
import Fmt
import System.Directory
import System.IO.Error (catchIOError)
import System.IO.Unsafe (unsafePerformIO)

import Types


data Shader = Shader { shaderName :: String
                     , shaderGlslFn :: String
                     , shaderInputs :: [String]
                     } deriving (Show)


ugenShaderFns :: [Shader]
ugenShaderFns = unsafePerformIO $ do
  shaders <- catchIOError loadUGensFromAppDir $ \_e -> (return embeddedShaders)
  if length shaders < length embeddedShaders
    then return embeddedShaders
    else return shaders
  where
    -- loadUGensFromAppDir :: IO [Shader]
    loadUGensFromAppDir = do
      appDir       <- getXdgDirectory XdgData "SuperCollider-AV/UGens"
      appDirExists <- doesDirectoryExist appDir
      if appDirExists then do
        files <- listDirectory appDir >>= return . (filter (endswith ".frag"))
        putStrLn "*** Debug: reading UGen shader functions from XdgData dir"
        forM files $ \f -> do
          contents <- readFile f
          return $ loadShader (f, pack contents)
      else do
        putStrLn "*** Debug: reading UGen shader functions from embedded files"
        return []


{- Load the shader functions at compile time using TemplateHaskell and
   the embedDir function. The number of shaders will always be
   sufficiently small that they can easily be held in memory for the
   duration of the program's execution.
-}
embeddedShaders :: [Shader]
embeddedShaders = map loadShader $(embedDir "lib/fragment_shaders")

vertexShader :: ByteString
vertexShader = $(embedFile "lib/vertex_shader.vert")

defaultFragShader :: ByteString
defaultFragShader = $(embedFile "lib/default_frag_shader.frag")

screenVertShader :: ByteString
screenVertShader = $(embedFile "lib/screen_shader.vert")

screenFragShader :: ByteString
screenFragShader = $(embedFile "lib/screen_shader.frag")

loadShader :: (FilePath, ByteString) -> Shader
loadShader (path, fileContents) =
  let glslFn = unpack fileContents
  in  Shader { shaderName = takeWhile (/= '.') path
             , shaderGlslFn = glslFn
             , shaderInputs = parseFnSig glslFn
             }

parseFnSig :: String -> [String]
parseFnSig glslFn =
  glslFn
  |> takeWhile (/= ')')
  |> lines
  |> concat
  |> dropWhile (/= '(')
  |> splitOneOf "(), "
  |> filter (not . (flip elem) ["", "in", "out", "inout"])
  |> argTypes
  where
    x |> f = f x
    argTypes [] = []
    argTypes [_] = []
    argTypes (x:_:xs) = x : argTypes xs


{- Generate the fragment shader (GLSL) code from the sub-graph.
-}
generateFragShader :: SubGraph -> FragShader
generateFragShader (SubGraph units inputs output) =
  let fsCode = concat [ fsHeader
                      , fsUniforms inputs units
                      , fsFunctions units
                      , "void main() {\n"
                      , "    FragColor = vec4(0.0, 0.0, 0.0, 1.0);\n"
                      , fsMain units
                      , "}\n"
                      ]
  in  FragShader fsCode inputs output
  where
    fsHeader = concat [ "#version 430 core\n\n"
                      , "in  vec2 TexCoord;\n"
                      , "out vec4 FragColor;\n\n"
                      , "uniform sampler2D sc_PrevFrame;\n\n"
                      ]


fsUniforms :: [Input] -> Graph -> String
fsUniforms inputs graph = (inputBusUniforms inputs) ++ (graphUniforms graph)
  where
    inputBusUniforms :: [Input] -> String
    inputBusUniforms ins = flip concatMap ins $ \i ->
      case i of InGlobal n -> "uniform sampler2D in_Bus_Global_"+|n|+";\n"
                InLocal  n -> "uniform sampler2D in_Bus_Local_"+|n|+";\n"

    graphUniforms :: Graph -> String
    graphUniforms units =
      let outWires = concatMap unitOutputs units
      in  concatMap (unitUniforms outWires) units

    unitUniforms :: [WireID] -> Unit -> String
    unitUniforms outWires unit =
      let inWires = unitInputs unit
          args = getShaderInputs $ unitName unit
          signalInputs = map (\(i, glslType, _w) -> (i, glslType))
                       $ filter (\(_i, _type, w) -> notElem w outWires)
                       $ zip3 [0::Int ..] args inWires
      in  flip concatMap signalInputs $ \(i, glslType) ->
            "uniform "+|glslType|+" in_Graph_"+|nodeID unit|+"_Unit_"+|unitID unit|+"_"+|i|+";\n"


fsFunctions :: Graph -> String
fsFunctions units = (concatMap getShaderFn).nub $ map unitName units


fsMain :: Graph -> String
fsMain graph = graphCode graph
  where
    graphCode :: Graph -> String
    graphCode units =
      let outWires = concatMap unitOutputs units
      in  concatMap (unitCode outWires) units

    unitCode :: [WireID] -> Unit -> String
    unitCode outWires unit =
      -- Note: this assumes each UGen has at least one output, including the
      --       GLOut UGen.
      foldl' (\func str -> str ++ func) ""
             $ map (functionCall unit outWires) (unitOutputs unit)

    functionCall :: Unit -> [WireID] -> WireID -> String
    functionCall unit outWires wireID =
      let name = unitName unit
          isGLOut = name == "GLOut"
          -- !!!!!
          -- TODO: output to FragColor when isJust unitBusOut.
          --       should be a direct assignment to FragColor, not same as GLOut
          assignment = case name of
            "GLOut" -> "    FragColor = " :: String
            _       -> "    "+|fnType name|+" Graph_"+|nodeID unit|+"_Wire_"+|wireID|+" = "
      in  concat [ assignment
                 , unitName unit
                 , if isGLOut then "(FragColor, " else "("
                 , inputList (zip [0..] $ unitInputs unit)
                 , ");\n"
                 ]
      where
        -- !!!!!
        -- TODO: use uniform input bus when isJust unitBusIns.
        --       need to work this out on paper...
        inputList :: [(Int, WireID)] -> String
        inputList inWires =
          case inWires of
            [] -> ""
            (i, w):[] -> if elem w outWires
                           then "Graph_"+|nodeID unit|+"_Wire_"+|w|+""
                           else "in_Graph_"+|nodeID unit|+"_Unit_"+|unitID unit|+"_"+|i|+""
            hd:tl -> ""+|inputList [hd]|+", "+|inputList tl|+""


{- Generate the GLSL code for the fragment shader from the node tree.
-}
-- generateGLSLCode :: IntMap Node -> String
-- generateGLSLCode nodes =
--   concat [ glslHeader
--          , glslUniforms nodes
--          , glslFunctions nodes
--          , "void main() {\n"
--          , "    FragColor = vec4(0.0, 0.0, 0.0, 1.0);\n"
--          , glslMain nodes
--          , "}\n"
--          ]
--   where
--     glslHeader = concat [ "#version 430 core\n\n"
--                         , "in  vec2 TexCoord;\n"
--                         , "out vec4 FragColor;\n\n"
--                         , "uniform sampler2D sc_PrevFrame;\n\n"
--                         ]


-- glslUniforms :: IntMap Node -> String
-- glslUniforms nodes = IntMap.foldlWithKey' nodeUniforms "" nodes
--   where
--     nodeUniforms :: String -> NodeID -> Node -> String
--     nodeUniforms glslString nID node =
--       case node of Node  graph  -> glslString ++ (graphUniforms nID graph)
--                    Group nodes' -> glslString ++ (glslUniforms nodes')

--     graphUniforms :: NodeID -> Graph -> String
--     graphUniforms nID units =
--       let outWires = concatMap unitOutputs units
--       in  concatMap (unitUniforms nID outWires) units

--     unitUniforms :: NodeID -> [WireID] -> Unit -> String
--     unitUniforms nID outWires unit =
--       let inWires = unitInputs unit
--           inputs = getShaderInputs $ unitName unit
--           uniformInputs = map (\(i, glslType, _w) -> (i, glslType))
--                         $ filter (\(_i, _type, w) -> notElem w outWires)
--                             (zip3 [0::Int ..] inputs inWires)
--       in  flip concatMap uniformInputs $ \(i, glslType) ->
--             "uniform "+|glslType|+" in_Graph_"+|nID|+"_Unit_"+|unitID unit|+"_"+|i|+";\n"


-- glslFunctions :: IntMap Node -> String
-- glslFunctions nodes = (concatMap getShaderFn).nub $ functionNames nodes []
--   where
--     functionNames :: IntMap Node -> [String] -> [String]
--     functionNames nodes' funs = IntMap.foldr nodeFuncs funs nodes'

--     nodeFuncs :: Node -> [String] -> [String]
--     nodeFuncs node funs =
--       case node of Node  units   -> foldr (\u fs -> (unitName u) : fs) funs units
--                    Group nodes'' -> functionNames nodes'' funs


-- glslMain :: IntMap Node -> String
-- glslMain nodes = IntMap.foldlWithKey' nodeCode "" nodes
--   where
--     nodeCode :: String -> NodeID -> Node -> String
--     nodeCode glslString nID node =
--       case node of Node  graph  -> glslString ++ (graphCode nID graph)
--                    Group nodes' -> glslString ++ (glslMain nodes')

--     graphCode :: NodeID -> Graph -> String
--     graphCode nID units =
--       let outWires = concatMap unitOutputs units
--       in  concatMap (unitCode nID outWires) units

--     unitCode :: NodeID -> [WireID] -> Unit -> String
--     unitCode nID outWires unit =
--       -- Note: this assumes each UGen has at least one output, including the
--       --       GLOut UGen.
--       foldl' (\func str -> str ++ func) ""
--              $ map (functionCall nID unit outWires) (unitOutputs unit)

--     functionCall :: NodeID -> Unit -> [WireID] -> WireID -> String
--     functionCall nID unit outWires wireID =
--       let name = unitName unit
--           isGLOut = name == "GLOut"
--           assignment = case name of
--             "GLOut" -> "    FragColor = " :: String
--             _       -> "    "+|fnType name|+" Graph_"+|nID|+"_Wire_"+|wireID|+" = "
--       in  concat [ assignment
--                  , unitName unit
--                  , if isGLOut then "(FragColor, " else "("
--                  , inputList (zip [0..] $ unitInputs unit)
--                  , ");\n"
--                  ]
--       where
--         inputList :: [(Int, WireID)] -> String
--         inputList inWires =
--           case inWires of
--             [] -> ""
--             (i, w):[] -> if elem w outWires
--                            then "Graph_"+|nID|+"_Wire_"+|w|+""
--                            else "in_Graph_"+|nID|+"_Unit_"+|unitID unit|+"_"+|i|+""
--             hd:tl -> ""+|inputList [hd]|+", "+|inputList tl|+""


{- Utility functions -}

uniformName :: Int -> Int -> Int -> String
uniformName gID uID index = "in_Graph_"+|gID|+"_Unit_"+|uID|+"_"+|index|+""


getShaderInputs :: String -> [String]
getShaderInputs name =
  shaderInputs . head $ filter (\s -> shaderName s == name) ugenShaderFns


getShaderFn :: String -> String
getShaderFn name =
  shaderGlslFn . head $ filter (\s -> shaderName s == name) ugenShaderFns


fnType :: String -> String
fnType name =
  ugenShaderFns
  |> filter (\s -> shaderName s == name)
  |> head
  |> shaderGlslFn
  |> dropWhile (== ' ') -- strip leading whitespace
  |> takeWhile (/= ' ') -- strip space after type identifier
  where
    x |> f = f x
