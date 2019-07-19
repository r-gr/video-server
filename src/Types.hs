{-# LANGUAGE DeriveGeneric #-}

module Types ( Bus(..)
             , FragShader(..)
             , Graph
             -- , GraphID
             , InBus(..)
             -- , Input(..)
             , NodeID
             , Node(..)
             , OutBus(..)
             -- , Output(..)
             , SCGraph
             , SCUnit(..)
             , ShaderProgram(..)
             , SubGraph(..)
             -- , Synth(..)
             -- , SynthID
             -- , UInput(..)
             -- , UOutput(..)
             , Unit(..)
             , UnitID
             , Window(..)
             , WindowID
             , WireID
             , containsVideoUGen
             -- , requiresPartition
             -- , requiresPartition'
             , partitionOn
             , linkID
             , isWire
             , isLBus
             -- , scUnitToUnit
             -- , scUnitsToUnits
             -- , scUnitToOut
             -- , scUnitToIn
             -- , NewGraph
             -- , NewUnit(..)
             , Link(..)
             -- , NewSubGraph(..)
             -- , NewFragShader(..)
             ) where


import Data.ByteString (ByteString)
import Data.IntMap (IntMap)
import Data.Aeson
import GHC.Generics
import qualified Graphics.Rendering.OpenGL as GL


-- type OldGraph = [OldUnit]
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

-- data OldUInput  = UIBus Input  | UIWire WireID
-- data OldUOutput = UOBus Output | UOWire WireID
-- data OldUnit = OldUnit { oldUnitName    :: String
--                  , oldUnitID      :: UnitID
--                  , oldNodeID      :: NodeID
--                  , oldUnitInputs  :: [WireID]
--                  , oldUnitOutputs :: [WireID]
--                  , oldUnitBusIns  :: Maybe [Input]
--                  , oldUnitBusOut  :: Maybe Output
--                  }

type WindowID = Int
data Window = Window { wNodes  :: IntMap Node
                     , wID     :: WindowID
                     , wShader :: ByteString
                     }

-- data Synth = Synth SynthID Graph
-- type SynthID = Int

-- data OldSubGraph      = OldSubGraph      Graph      [Input] Output
-- data OldFragShader    = OldFragShader    String     [Input] Output
data ShaderProgram = ShaderProgram GL.Program [InBus] OutBus

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


containsVideoUGen :: [String] -> [SCUnit] -> Bool
containsVideoUGen glUGenNames units =
  elem True $ map (\unit -> elem (scUnitName unit) glUGenNames) units


-- requiresPartition :: SCUnit -> Bool
-- requiresPartition unit = elem (scUnitName unit) ["Rotate", "Tex1Thing", "Tex2Thing"]

-- requiresPartition' :: String -> Bool
-- requiresPartition' uName = elem uName ["Rotate", "Tex1Thing", "Tex2Thing"]

partitionOn :: String -> Maybe [Int]
partitionOn uName =
  case uName of
    "Rotate"    -> Just [0]
    "Tex1Thing" -> Just [0]
    "Tex2Thing" -> Just [0, 1]
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

-- scUnitsToUnits :: [SCUnit] -> [Input] -> Output -> [Unit]
-- scUnitsToUnits [] _ _ = []
-- scUnitsToUnits scUnits [] (OutGlobal _) = map scUnitToUnit scUnits
-- scUnitsToUnits (x:xs) ins (OutGlobal _) = (toInUnit ins x) : (map scUnitToUnit xs)
-- scUnitsToUnits scUnits [] out =
--   (map scUnitToUnit $ init scUnits) ++ [toOutUnit out $ last scUnits]
-- scUnitsToUnits (x:xs) ins out =
--   (toInUnit ins x) : (map scUnitToUnit $ init xs) ++ [toOutUnit out $ last xs]

-- scUnitToUnit :: SCUnit -> Unit
-- scUnitToUnit u = Unit { unitName    = scUnitName u
--                       , unitID      = scUnitID u
--                       , nodeID      = scNodeID u
--                       , unitInputs  = scUnitInputs u
--                       , unitOutputs = scUnitOutputs u
--                       , unitBusIns  = Nothing
--                       , unitBusOut  = Nothing
--                       }

-- toOutUnit :: Output -> SCUnit -> Unit
-- toOutUnit out u = Unit { unitName    = scUnitName u
--                        , unitID      = scUnitID u
--                        , nodeID      = scNodeID u
--                        , unitInputs  = scUnitInputs u
--                        , unitOutputs = scUnitOutputs u
--                        , unitBusIns  = Nothing
--                        , unitBusOut  = Just out
--                        }

-- toInUnit :: [Input] -> SCUnit -> Unit
-- toInUnit ins u = Unit { unitName    = scUnitName u
--                       , unitID      = scUnitID u
--                       , nodeID      = scNodeID u
--                       , unitInputs  = scUnitInputs u
--                       , unitOutputs = scUnitOutputs u
--                       , unitBusIns  = Just ins
--                       , unitBusOut  = Nothing
--                       }

