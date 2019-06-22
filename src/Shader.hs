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
              , generateGLSLCode
              ) where


-- import Control.Exception (catch)
import Control.Monad (forM)
import Data.ByteString (ByteString)
import Data.ByteString.Char8 (pack, unpack)
import Data.FileEmbed (embedDir, embedFile)
import Data.Foldable (foldl')
import Data.IntMap (IntMap)
import qualified Data.IntMap.Strict as IntMap
import Data.List (nub)
import Data.List.Split (splitOneOf)
import Data.List.Utils (endswith)
import Fmt
import System.Directory
import System.IO.Error (catchIOError)
import System.IO.Unsafe (unsafePerformIO)

import Graph
import Node
import Unit


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


{- Generate the GLSL code for the fragment shader from the node tree.
-}
generateGLSLCode :: IntMap Node -> String
generateGLSLCode nodes =
  concat [ glslHeader
         , glslUniforms nodes
         , glslFunctions nodes
         , "void main() {\n"
         , "    FragColor = vec4(0.0, 0.0, 0.0, 1.0);\n"
         , glslMain nodes
         , "}\n"
         ]
  where
    glslHeader = concat [ "#version 430 core\n\n"
                        , "in  vec2 TexCoord;\n"
                        , "out vec4 FragColor;\n\n"
                        , "uniform sampler2D sc_PrevFrame;\n\n"
                        ]


glslUniforms :: IntMap Node -> String
glslUniforms nodes = IntMap.foldlWithKey' nodeUniforms "" nodes
  where
    nodeUniforms :: String -> NodeID -> Node -> String
    nodeUniforms glslString nID node =
      case node of Node  graph  -> glslString ++ (graphUniforms nID graph)
                   Group nodes' -> glslString ++ (glslUniforms nodes')

    graphUniforms :: NodeID -> Graph -> String
    graphUniforms nID (_gID, units) =
      let outWires = concatMap unitOutputs units
      in  concatMap (unitUniforms nID outWires) units

    unitUniforms :: NodeID -> [WireID] -> Unit -> String
    unitUniforms nID outWires unit =
      let inWires = unitInputs unit
          inputs = getShaderInputs $ unitName unit
          uniformInputs = map (\(i, glslType, _w) -> (i, glslType))
                        $ filter (\(_i, _type, w) -> notElem w outWires)
                            (zip3 [0::Int ..] inputs inWires)
      in  flip concatMap uniformInputs $ \(i, glslType) ->
            "uniform "+|glslType|+" in_Graph_"+|nID|+"_Unit_"+|unitID unit|+"_"+|i|+";\n"


glslFunctions :: IntMap Node -> String
glslFunctions nodes = (concatMap getShaderFn).nub $ functionNames nodes []
  where
    functionNames :: IntMap Node -> [String] -> [String]
    functionNames nodes' funs = IntMap.foldr nodeFuncs funs nodes'

    nodeFuncs :: Node -> [String] -> [String]
    nodeFuncs node funs =
      case node of Node  (_, units) -> foldr (\u fs -> (unitName u) : fs) funs units
                   Group nodes''    -> functionNames nodes'' funs


glslMain :: IntMap Node -> String
glslMain nodes = IntMap.foldlWithKey' nodeCode "" nodes
  where
    nodeCode :: String -> NodeID -> Node -> String
    nodeCode glslString nID node =
      case node of Node  graph  -> glslString ++ (graphCode nID graph)
                   Group nodes' -> glslString ++ (glslMain nodes')

    graphCode :: NodeID -> Graph -> String
    graphCode nID (_gID, units) =
      let outWires = concatMap unitOutputs units
      in  concatMap (unitCode nID outWires) units

    unitCode :: NodeID -> [WireID] -> Unit -> String
    unitCode nID outWires unit =
      -- Note: this assumes each UGen has at least one output, including the
      --       GLOut UGen.
      foldl' (\func str -> str ++ func) ""
             $ map (functionCall nID unit outWires) (unitOutputs unit)

    functionCall :: NodeID -> Unit -> [WireID] -> WireID -> String
    functionCall nID unit outWires wireID =
      let name = unitName unit
          isGLOut = name == "GLOut"
          assignment = case name of
            "GLOut" -> "    FragColor = " :: String
            _       -> "    "+|fnType name|+" Graph_"+|nID|+"_Wire_"+|wireID|+" = "
      in  concat [ assignment
                 , unitName unit
                 , if isGLOut then "(FragColor, " else "("
                 , inputList (zip [0..] $ unitInputs unit)
                 , ");\n"
                 ]
      where
        inputList :: [(Int, WireID)] -> String
        inputList inWires =
          case inWires of
            [] -> ""
            (i, w):[] -> if elem w outWires
                           then "Graph_"+|nID|+"_Wire_"+|w|+""
                           else "in_Graph_"+|nID|+"_Unit_"+|unitID unit|+"_"+|i|+""
            hd:tl -> ""+|inputList [hd]|+", "+|inputList tl|+""


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
