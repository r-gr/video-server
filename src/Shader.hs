{-# LANGUAGE TemplateHaskell #-}

module Shader
  ( Shader(..)
  , ugenShaderFns
  , vertexShader
  , screenVertShader
  , screenFragShader
  , uniformName
  , generateFragShader
  ) where


import MyPrelude
import RIO
import RIO.ByteString (ByteString)
import RIO.List
import RIO.List.Partial
import qualified RIO.Text as Text

import Control.Monad (forM)
import qualified Data.ByteString.Char8 as C
import Data.FileEmbed (embedDir, embedFile)
import Data.List (nub)
import Data.List.Split (splitOneOf)
import Data.List.Utils (endswith)
import System.Directory
import System.IO.Error (catchIOError)
import System.IO.Unsafe (unsafePerformIO)

import Types


data Shader = Shader { shaderName :: Text
                     , shaderGlslFn :: Text
                     , shaderInputs :: [Text]
                     } deriving (Show)


-- {-# NOINLINE ugenShaderFns #-}
{-# INLINE ugenShaderFns #-}
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
          contents <- readFileUtf8 f
          return $ loadShader (f, contents)
      else do
        putStrLn "*** Debug: reading UGen shader functions from embedded files"
        return []


{- Load the shader functions at compile time using TemplateHaskell and
   the embedDir function. The number of shaders will always be
   sufficiently small that they can easily be held in memory for the
   duration of the program's execution.
-}
embeddedShaders :: [Shader]
embeddedShaders = map (loadShader.(\(f, bs) -> (f, Text.pack $ C.unpack bs))) $(embedDir "lib/fragment_shaders")

vertexShader :: ByteString
vertexShader = $(embedFile "lib/vertex_shader.vert")

screenVertShader :: ByteString
screenVertShader = $(embedFile "lib/screen_shader.vert")

screenFragShader :: ByteString
screenFragShader = $(embedFile "lib/screen_shader.frag")

loadShader :: (FilePath, Text) -> Shader
loadShader (path, fileContents) =
  let glslFn = fileContents
  in  Shader { shaderName = Text.takeWhile (/= '.') $ Text.pack path
             , shaderGlslFn = glslFn
             , shaderInputs = parseFnSig glslFn
             }

parseFnSig :: Text -> [Text]
parseFnSig glslFn =
  glslFn
  |> Text.takeWhile (/= ')')
  |> Text.lines
  |> Text.concat
  |> Text.dropWhile (/= '(')
  |> Text.unpack -- TODO: avoid conversion to String and back
  |> splitOneOf "(), "
  |> map Text.pack
  |> filter (not . (flip elem) ["", "in", "out", "inout"])
  |> argTypes
  where
    argTypes [] = []
    argTypes [_] = []
    argTypes (x:_:xs) = x : argTypes xs


{- Generate the fragment shader (GLSL) code from the sub-graph.
-}
generateFragShader :: SubGraph -> FragShader
generateFragShader (SubGraph units inputs output) =
  let units' = units -- filter (\u -> unitName u /= "DelBufWr") units
      fsCode = Text.concat [ fsHeader
                           , fsUniforms inputs units'
                           , fsFunctions units'
                           , "void main() {\n"
                           , "    FragColor = vec4(0.0, 0.0, 0.0, 1.0);\n"
                           , fsMain inputs output units'
                           , "}\n"
                           ]
  in  FragShader fsCode inputs output
  where
    fsHeader = Text.concat [ "#version 330 core\n\n"
                           , "in  vec2 TexCoord;\n"
                           , "out vec4 FragColor;\n\n"
                           -- , "uniform sampler2D sc_PrevFrame;\n\n"
                           ]


fsUniforms :: [Link] -> Graph -> Text
fsUniforms inputs graph = Text.append (inputBusUniforms inputs) (graphUniforms graph)
  where
    inputBusUniforms :: [Link] -> Text
    inputBusUniforms ins = Text.pack $ flip concatMap ins $ \i ->
      case i of LBus wireID _ _ -> "uniform sampler2D u_Bus_Local_"+|wireID|+";\n"
                Wire _      _ _ -> ""

    graphUniforms :: Graph -> Text
    graphUniforms units =
      let outWires = map unitOutput units
      in  Text.concat $ map (unitUniforms outWires) units

    unitUniforms :: [WireID] -> Unit -> Text
    unitUniforms outWires unit =
      let inWires = unitInputs unit
          args = getShaderInputs $ unitName unit
          signalInputs = map (\(i, glslType, _w) -> (i, glslType))
                       $ filter (\(_i, _type, w) -> notElem w outWires && (notElem w $ lBusIDs inputs))
                       $ zip3 [0::Int ..] args inWires
      in  Text.concat $ flip map signalInputs $ \(i, glslType) ->
            "uniform "+|glslType|+" in_Graph_"+|unitNodeID unit|+"_Unit_"+|unitID unit|+"_"+|i|+";\n"


fsFunctions :: Graph -> Text
fsFunctions = Text.concat.(map getShaderFn).nub.(map unitName)


fsMain :: [Link] -> Maybe Link -> Graph -> Text
fsMain inputs output graph = graphCode graph
  where
    graphCode :: Graph -> Text
    graphCode units =
      let outWires = map unitOutput units
      in  Text.concat $ map (unitCode outWires) units

    unitCode :: [WireID] -> Unit -> Text
    unitCode outWires unit = functionCall unit outWires $ unitOutput unit

    functionCall :: Unit -> [WireID] -> WireID -> Text
    functionCall unit outWires wireID =
      let name = unitName unit
          isGLOut = name == "GLOut"
          -- if output isNothing then the SubGraph should contain a GLOut
          --   but if output isJust then the src unit of the SubGraph's LBus should
          --   output to FragColor
          assignment = case output of
            Just (LBus _ src _) -> if (unitID unit) == src
              then       "    FragColor = " :: Text
              else       "    "+|fnType name|+" Graph_"+|unitNodeID unit|+"_Wire_"+|wireID|+" = "
            _ -> case name of
              "GLOut" -> "    FragColor = " :: Text
              _       -> "    "+|fnType name|+" Graph_"+|unitNodeID unit|+"_Wire_"+|wireID|+" = "

      in  Text.concat [ assignment
                 , unitName unit
                 , if isGLOut then "(FragColor, " else "("
                 , inputList (unitName unit) (zip [0..] $ unitInputs unit)
                 , ");\n"
                 ]
      where
        inputList :: Text -> [(Int, WireID)] -> Text
        inputList uName inWires =
          case inWires of
            [] -> ""
            [(i, w)] -> if elem w outWires && (notElem w $ lBusIDs inputs) then
                          "Graph_"+|unitNodeID unit|+"_Wire_"+|w|+""
                        else if (elem w $ lBusIDs inputs) && isJust (partitionOn uName) then
                          "u_Bus_Local_"+|w|+""
                        else if elem w $ lBusIDs inputs then
                          "texture(u_Bus_Local_"+|w|+", TexCoord)"
                        else
                          "in_Graph_"+|unitNodeID unit|+"_Unit_"+|unitID unit|+"_"+|i|+""
            hd:tl -> ""+|inputList uName [hd]|+", "+|inputList uName tl|+""


{- Utility functions -}

uniformName :: Int -> Int -> Int -> Text
uniformName gID uID index =
  -- It turns out that Fmt's String/Text building doesn't have great performance
  -- so use Text.concat here since this function gets called a LOT.
  Text.concat [ "in_Graph_", tshow gID, "_Unit_", tshow uID, "_", tshow index ]


getShaderInputs :: Text -> [Text]
getShaderInputs name =
  shaderInputs . head $ filter (\s -> shaderName s == name) ugenShaderFns


getShaderFn :: Text -> Text
getShaderFn name =
  shaderGlslFn . head $ filter (\s -> shaderName s == name) ugenShaderFns


fnType :: Text -> Text
fnType name =
  ugenShaderFns
  |> filter (\s -> shaderName s == name)
  |> head
  |> shaderGlslFn
  |> Text.dropWhile (== ' ') -- strip leading whitespace
  |> Text.takeWhile (/= ' ') -- strip space after type identifier


lBusIDs :: [Link] -> [WireID]
lBusIDs = (map linkID).(filter isLBus)
