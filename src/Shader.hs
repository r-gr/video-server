{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Shader ( Shader(..)
              , ugenShaderFns
              , defaultFragShader
              , vertexShader
              , screenVertShader
              , screenFragShader
              , uniformName
              , generateFragShader
              ) where


import Control.Monad (forM)
import Data.ByteString (ByteString)
import Data.ByteString.Char8 (pack, unpack)
import Data.FileEmbed (embedDir, embedFile)
import Data.List (nub)
import Data.List.Split (splitOneOf)
import Data.List.Utils (endswith)
import Data.Maybe
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
    loadUGensFromAppDir :: IO [Shader]
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
                      , fsMain inputs output units
                      , "}\n"
                      ]
  in  FragShader fsCode inputs output
  where
    fsHeader = concat [ "#version 330 core\n\n"
                      , "in  vec2 TexCoord;\n"
                      , "out vec4 FragColor;\n\n"
                      , "uniform sampler2D sc_PrevFrame;\n\n"
                      ]


fsUniforms :: [Link] -> Graph -> String
fsUniforms inputs graph = (inputBusUniforms inputs) ++ (graphUniforms graph)
  where
    inputBusUniforms :: [Link] -> String
    inputBusUniforms ins = flip concatMap ins $ \i ->
      case i of LBus wireID _ _ -> "uniform sampler2D u_Bus_Local_"+|wireID|+";\n"
                Wire _      _ _ -> ""

    graphUniforms :: Graph -> String
    graphUniforms units =
      let outWires = map unitOutput units
      in  concatMap (unitUniforms outWires) units

    unitUniforms :: [WireID] -> Unit -> String
    unitUniforms outWires unit =
      let inWires = unitInputs unit
          args = getShaderInputs $ unitName unit
          signalInputs = map (\(i, glslType, _w) -> (i, glslType))
                       $ filter (\(_i, _type, w) -> notElem w outWires && (notElem w $ lBusIDs inputs))
                       $ zip3 [0::Int ..] args inWires
      in  flip concatMap signalInputs $ \(i, glslType) ->
            "uniform "+|glslType|+" in_Graph_"+|nodeID unit|+"_Unit_"+|unitID unit|+"_"+|i|+";\n"


fsFunctions :: Graph -> String
fsFunctions units = (concatMap getShaderFn).nub $ map unitName units


fsMain :: [Link] -> Maybe Link -> Graph -> String
fsMain inputs output graph = graphCode graph
  where
    graphCode :: Graph -> String
    graphCode units =
      let outWires = map unitOutput units
      in  concatMap (unitCode outWires) units

    unitCode :: [WireID] -> Unit -> String
    unitCode outWires unit = functionCall unit outWires $ unitOutput unit

    functionCall :: Unit -> [WireID] -> WireID -> String
    functionCall unit outWires wireID =
      let name = unitName unit
          isGLOut = name == "GLOut"
          -- if output isNothing then the SubGraph should contain a GLOut
          --   but if output isJust then the src unit of the SubGraph's LBus should
          --   output to FragColor
          assignment = case output of
            Just (LBus _ src _) -> if (unitID unit) == src
              then       "    FragColor = " :: String
              else       "    "+|fnType name|+" Graph_"+|nodeID unit|+"_Wire_"+|wireID|+" = "
            _ -> case name of
              "GLOut" -> "    FragColor = " :: String
              _       -> "    "+|fnType name|+" Graph_"+|nodeID unit|+"_Wire_"+|wireID|+" = "

      in  concat [ assignment
                 , unitName unit
                 , if isGLOut then "(FragColor, " else "("
                 , inputList (unitName unit) (zip [0..] $ unitInputs unit)
                 , ");\n"
                 ]
      where
        inputList :: String -> [(Int, WireID)] -> String
        inputList uName inWires =
          case inWires of
            [] -> ""
            [(i, w)] -> if elem w outWires && (notElem w $ lBusIDs inputs) then
                          "Graph_"+|nodeID unit|+"_Wire_"+|w|+""
                        else if (elem w $ lBusIDs inputs) && isJust (partitionOn uName) then
                          "u_Bus_Local_"+|w|+""
                        else if elem w $ lBusIDs inputs then
                          "texture(u_Bus_Local_"+|w|+", TexCoord)"
                        else
                          "in_Graph_"+|nodeID unit|+"_Unit_"+|unitID unit|+"_"+|i|+""
            hd:tl -> ""+|inputList uName [hd]|+", "+|inputList uName tl|+""


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


lBusIDs :: [Link] -> [WireID]
lBusIDs = (map linkID).(filter isLBus)
