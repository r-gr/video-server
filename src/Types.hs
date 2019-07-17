{-# LANGUAGE DeriveGeneric #-}

module Types ( Bus(..)
             , FragShader(..)
             , Graph
             -- , GraphID
             , InBus
             , Input(..)
             , NodeID
             , Node(..)
             , OutBus
             , Output(..)
             , SCGraph
             , SCUnit(..)
             , ShaderProgram(..)
             , SubGraph(..)
             -- , Synth(..)
             -- , SynthID
             , UInput(..)
             , UOutput(..)
             , Unit(..)
             , UnitID
             , Window(..)
             , WindowID
             , WireID
             , containsVideoUGen
             , requiresPartition
             , requiresPartition'
             , partitionOn
             , scUnitToUnit
             , scUnitsToUnits
             -- , scUnitToOut
             -- , scUnitToIn
             ) where


import Data.ByteString (ByteString)
import Data.IntMap (IntMap)
import Data.Aeson
import GHC.Generics
import qualified Graphics.Rendering.OpenGL as GL


type Graph = [Unit]
type SCGraph = [SCUnit]

type NodeID = Int
-- TODO: temporarily storing the graph to let the check for GLPrevFrame
--       in the Render module to stay in place
data Node = Node [ShaderProgram] SCGraph | Group (IntMap Node)

type WireID  = Int
type UnitID  = Int
data SCUnit = SCUnit { scUnitName    :: String
                     , scUnitID      :: UnitID
                     , scNodeID      :: NodeID
                     , scUnitInputs  :: [WireID]
                     , scUnitOutputs :: [WireID]
                     } deriving (Generic, Show)

instance FromJSON SCUnit

data UInput  = UIBus Input  | UIWire WireID
data UOutput = UOBus Output | UOWire WireID
data Unit = Unit { unitName    :: String
                 , unitID      :: UnitID
                 , nodeID      :: NodeID
                 , unitInputs  :: [WireID]
                 , unitOutputs :: [WireID]
                 , unitBusIns  :: Maybe [Input]
                 , unitBusOut  :: Maybe Output
                 }

type WindowID = Int
data Window = Window { wNodes  :: IntMap Node
                     , wID     :: WindowID
                     , wShader :: ByteString
                     }

-- data Synth = Synth SynthID Graph
-- type SynthID = Int

data SubGraph      = SubGraph      Graph      [Input] Output
data FragShader    = FragShader    String     [Input] Output
data ShaderProgram = ShaderProgram GL.Program [InBus] OutBus

data Input  = InGlobal  Int | InLocal  Int
data Output = OutGlobal Int | OutLocal Int

type InBus  = Bus
type OutBus = Bus

data Bus = Bus GL.FramebufferObject GL.TextureObject


containsVideoUGen :: [String] -> [SCUnit] -> Bool
containsVideoUGen glUGenNames units =
  elem True $ map (\unit -> elem (scUnitName unit) glUGenNames) units


requiresPartition :: SCUnit -> Bool
requiresPartition unit = elem (scUnitName unit) ["Rotate"]

requiresPartition' :: String -> Bool
requiresPartition' uName = elem uName ["Rotate"]

partitionOn :: String -> Maybe [Int]
partitionOn uName =
  case uName of
    "Rotate" -> Just [0]
    _ -> Nothing


scUnitsToUnits :: [SCUnit] -> [Input] -> Output -> [Unit]
scUnitsToUnits [] _ _ = []
scUnitsToUnits scUnits [] (OutGlobal _) = map scUnitToUnit scUnits
scUnitsToUnits (x:xs) ins (OutGlobal _) = (toInUnit ins x) : (map scUnitToUnit xs)
scUnitsToUnits scUnits [] out =
  (map scUnitToUnit $ init scUnits) ++ [toOutUnit out $ last scUnits]
scUnitsToUnits (x:xs) ins out =
  (toInUnit ins x) : (map scUnitToUnit $ init xs) ++ [toOutUnit out $ last xs]

scUnitToUnit :: SCUnit -> Unit
scUnitToUnit u = Unit { unitName    = scUnitName u
                      , unitID      = scUnitID u
                      , nodeID      = scNodeID u
                      , unitInputs  = scUnitInputs u
                      , unitOutputs = scUnitOutputs u
                      , unitBusIns  = Nothing
                      , unitBusOut  = Nothing
                      }

toOutUnit :: Output -> SCUnit -> Unit
toOutUnit out u = Unit { unitName    = scUnitName u
                       , unitID      = scUnitID u
                       , nodeID      = scNodeID u
                       , unitInputs  = scUnitInputs u
                       , unitOutputs = scUnitOutputs u
                       , unitBusIns  = Nothing
                       , unitBusOut  = Just out
                       }

toInUnit :: [Input] -> SCUnit -> Unit
toInUnit ins u = Unit { unitName    = scUnitName u
                      , unitID      = scUnitID u
                      , nodeID      = scNodeID u
                      , unitInputs  = scUnitInputs u
                      , unitOutputs = scUnitOutputs u
                      , unitBusIns  = Just ins
                      , unitBusOut  = Nothing
                      }

