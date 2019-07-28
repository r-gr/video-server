{-# LANGUAGE DeriveGeneric #-}

module Types ( Bus(..)
             , FragShader(..)
             , Graph
             , InBus(..)
             , Link(..)
             , NodeID
             , Node(..)
             , OutBus(..)
             , SCGraph
             , SCUnit(..)
             , ShaderProgram(..)
             , SubGraph(..)
             , Unit(..)
             , UnitID
             , Window(..)
             , WindowID
             , WindowState(..)
             , WireID
             , containsVideoUGen
             , partitionOn
             , linkID
             , isWire
             , isLBus
             ) where


import Data.ByteString (ByteString)
import Data.IntMap (IntMap)
import Data.Map (Map)
import Data.Aeson
import GHC.Generics
import qualified Graphics.Rendering.OpenGL as GL


type NodeID = Int
-- TODO: temporarily storing the graph to let the check for GLPrevFrame
--       in the Render module to stay in place
data Node = Node [ShaderProgram] SCGraph | Group (IntMap Node)

type WireID  = Int
type UnitID  = Int
type SCGraph = [SCUnit]
data SCUnit  = SCUnit { scUnitName    :: String
                      , scUnitID      :: UnitID
                      , scNodeID      :: NodeID
                      , scUnitInputs  :: [WireID]
                      , scUnitOutputs :: [WireID]
                      } deriving (Generic, Show)

instance FromJSON SCUnit

type WindowID = Int
data Window = Window { wNodes  :: IntMap Node
                     , wID     :: WindowID
                     , wShader :: ByteString
                     }

-- data Input  = InGlobal  Int | InLocal  Int
-- data Output = OutGlobal Int | OutLocal Int

data InBus  = InBus WireID Bus deriving (Show)
data OutBus = OutBus WireID Bus deriving (Show)

data Bus = Bus GL.FramebufferObject GL.TextureObject deriving (Show)

type Graph = [Unit]
data Unit = Unit { unitName        :: String
                 , unitID          :: UnitID
                 , nodeID          :: NodeID
                 , unitInputs      :: [WireID]
                 , unitOutput      :: WireID
                 , unitPartitionOn :: Maybe [Int]
                 } deriving (Eq, Show)

data Link = Wire WireID UnitID [UnitID]
          | LBus WireID UnitID [UnitID]
          deriving (Eq, Show)

data SubGraph   = SubGraph   Graph  [Link] (Maybe Link) deriving (Show)
data FragShader = FragShader String [Link] (Maybe Link) deriving (Show)
data ShaderProgram = ShaderProgram GL.Program [InBus] OutBus

data WindowState =
  WindowState { wsWidth  :: Int
              , wsHeight :: Int
              , wsBuses :: Map (NodeID, WireID) Bus
              , wsDefaultOutBus :: OutBus
              } deriving (Show)


containsVideoUGen :: [String] -> [SCUnit] -> Bool
containsVideoUGen glUGenNames units =
  elem True $ map (\unit -> elem (scUnitName unit) glUGenNames) units


partitionOn :: String -> Maybe [Int]
partitionOn uName =
  case uName of
    "FlipX"     -> Just [0]
    "FlipY"     -> Just [0]
    "MirrorX"   -> Just [0]
    "MirrorY"   -> Just [0]
    "Ripple"    -> Just [0]
    "Rotate"    -> Just [0]
    "Scale2"    -> Just [0]
    "ScaleXY"   -> Just [0]
    "Translate" -> Just [0]
    -- "Tex2Thing" -> Just [0, 1]
    _ -> Nothing


linkID :: Link -> WireID
linkID (Wire wireID _ _) = wireID
linkID (LBus wireID _ _) = wireID

isWire :: Link -> Bool
isWire (Wire _ _ _) = True
isWire _ = False

isLBus :: Link -> Bool
isLBus (LBus _ _ _) = True
isLBus _ = False
