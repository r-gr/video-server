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
             , ShaderProgram(..)
             , SubGraph(..)
             -- , Synth(..)
             -- , SynthID
             , Unit(..)
             , UnitID
             , Window(..)
             , WindowID
             , WireID
             , containsVideoUGen
             , requiresPartition
             ) where


import Data.ByteString (ByteString)
import Data.IntMap (IntMap)
import Data.Aeson (FromJSON)
import GHC.Generics
import qualified Graphics.Rendering.OpenGL as GL


type Graph = [Unit]

type NodeID = Int
data Node = Node Graph | Group (IntMap Node)

type WireID  = Int
type UnitID  = Int
data Unit    = Unit { unitName    :: String
                    , unitID      :: UnitID
                    , nodeID      :: NodeID
                    , unitInputs  :: [WireID]
                    , unitOutputs :: [WireID]
                    } deriving (Generic, Show)

instance FromJSON Unit

-- data UGen = Alpha
--           | Mix
--           | Rotate

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


containsVideoUGen :: [String] -> [Unit] -> Bool
containsVideoUGen glUGenNames units =
  elem True $ map (\unit -> elem (unitName unit) glUGenNames) units

requiresPartition :: Unit -> Bool
requiresPartition unit = elem (unitName unit) ["Rotate"]
