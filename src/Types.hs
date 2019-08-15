module Types ( Bus(..)
             , CmdLineOpts(..)
             , FragShader(..)
             , Graph
             , InBus(..)
             , Link(..)
             , NodeID
             , Node(..)
             , OutBus(..)
             , RenderState(..)
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

             -- MSG
             , UnitData(..)
             , Msg(..)
             , InternalMsg(..)
             , ExternalMsg(..)
             , Response(..)
             , TextureUpdate(..)

             -- TEXTURE
             , Texture(..)
             , TextureClass(..)
             , ImageTexture(..)
             , VideoTexture(..)
             , LoadedVideo(..)
             , FrameGrabber

             , containsVideoUGen
             , partitionOn
             , linkID
             , isWire
             , isLBus
             ) where


import RIO

import Codec.Picture
import Data.ByteString (ByteString)
import Data.IntMap (IntMap)
import Data.Map (Map)
import Data.Aeson
import qualified Data.Vector as V
import qualified Graphics.Rendering.OpenGL as GL
import qualified Graphics.UI.GLFW as GLFW


data CmdLineOpts = CmdLineOpts
  { version :: Bool
  , cmdMsgSock :: String
  , dataMsgSock :: String
  , responseSock :: String
  }

type NodeID = Int
-- TODO: temporarily storing the graph to let the check for GLPrevFrame
--       in the Render module to stay in place
data Node = Node [ShaderProgram] SCGraph | Group (IntMap Node)

type WireID  = Int
type UnitID  = Int
type SCGraph = [SCUnit]
data SCUnit  = SCUnit { scUnitName    :: Text
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
data Unit = Unit { unitName        :: Text
                 , unitID          :: UnitID
                 , unitNodeID      :: NodeID
                 , unitInputs      :: [WireID]
                 , unitOutput      :: WireID
                 , unitPartitionOn :: Maybe [Int]
                 } deriving (Eq, Show)

data Link = Wire WireID UnitID [UnitID]
          | LBus WireID UnitID [UnitID]
          deriving (Eq, Show)

data SubGraph   = SubGraph   Graph [Link] (Maybe Link) deriving (Show)
data FragShader = FragShader Text  [Link] (Maybe Link) deriving (Show)
data ShaderProgram = ShaderProgram GL.Program [InBus] OutBus




{- WINDOW -}


data WindowState =
  WindowState { wsWindow :: GLFW.Window
              , wsWidth  :: Int -- TODO?: make the width and height IORefs?
              , wsHeight :: Int
              , wsShouldExit :: IORef Bool
              , wsMsgQIn :: TBQueue ExternalMsg
              , wsMsgQOut :: TBQueue Msg
              , wsTextures :: TVar [Texture] -- TODO: better data structure
              , wsNodeTree :: IORef (IntMap Node)
              , wsUniformVals :: TBQueue UnitData
              , wsTextureUpdates :: TBQueue TextureUpdate
              }




{- RENDER -}


data RenderState = RenderState
  { rsWindowState :: IORef WindowState
  , rsScreenShader :: GL.Program
  , rsVAO :: GL.VertexArrayObject
  , rsBuses :: IORef (Map (NodeID, WireID) Bus)
  , rsDefaultOutBus :: Bus
  -- TODO: sort out the feedback implementation so this isn't required
  , rsIter1 :: IORef Bool
  , rsTB1 :: GL.TextureObject
  , rsFB1 :: GL.FramebufferObject
  , rsTB2 :: GL.TextureObject
  , rsFB2 :: GL.FramebufferObject
  }




{- MSG -}


data Msg = Internal InternalMsg | External ExternalMsg
data ExternalMsg
  = GraphNew  { nodeID :: NodeID
              , units  :: [SCUnit]
              }
  | GraphFree { nodeID :: NodeID }
  | GLWindowNew  { windowID     :: WindowID
                 , windowWidth  :: Int
                 , windowHeight :: Int
                 }
  | GLWindowFree { windowID :: WindowID }
  | GLVideoNew  { videoID   :: Int
                , videoPath :: FilePath
                , videoRate :: Float
                , videoLoop :: Bool
                , windowID  :: WindowID
                }
  | GLVideoRead { videoID   :: Int
                , videoPath :: FilePath
                , videoRate :: Float
                , videoLoop :: Bool
                , windowID  :: WindowID
                }
  | GLVideoFree { videoID  :: Int
                , windowID :: WindowID
                }
  | GLImageNew  { imageID   :: Int
                , imagePath :: FilePath
                , windowID  :: WindowID
                }
  | GLImageFree { imageID  :: Int
                , windowID :: WindowID
                }
  | InVidDur       { inReqID  :: Int32
                   , videoID  :: Int
                   , windowID :: WindowID
                   }
  | InVidFrames    { inReqID  :: Int32
                   , videoID  :: Int
                   , windowID :: WindowID
                   }
  | InVidFrameRate { inReqID  :: Int32
                   , videoID  :: Int
                   , windowID :: WindowID
                   }
  deriving (Generic, Show)
data InternalMsg
  = SendResponse { responseMsg :: Response }
  | WindowFree { iWindowID :: WindowID }
  | ServerQuit
  deriving (Generic, Show)

instance FromJSON ExternalMsg

data Response
  = OutVidDur       { outReqID     :: Int32
                    , vidDur       :: Float
                    }
  | OutVidFrames    { outReqID     :: Int32
                    , vidFrames    :: Int32
                    }
  | OutVidFrameRate { outReqID     :: Int32
                    , vidFrameRate :: Float
                    }
  deriving (Generic, Show)

instance ToJSON Response

data UnitData = UnitData
  { uDataNodeID :: Int
  , uDataUnitID  :: Int
  , uDataInput   :: Int
  , uDataValue   :: Float
  } deriving (Show)

data TextureUpdate
  = AssignVideo (NodeID, UnitID, Int) Int
  | AssignImage (NodeID, UnitID, Int) Int
  deriving (Show)




{- TEXTURES -}


type FrameGrabber p = IO (Maybe (Image p, Double))
class TextureClass t where
  texObj      :: t -> GL.TextureObject
  texUnit     :: t -> GL.TextureUnit
  assignments :: t -> [(NodeID, UnitID, Int)]
  texID       :: t -> Int
data Texture = Vid VideoTexture | Img ImageTexture | LVd LoadedVideo
data VideoTexture
  = VideoTexture { vTexObj        :: GL.TextureObject
                 , vTexUnit       :: GL.TextureUnit
                 , vAssignments   :: [(NodeID, UnitID, Int)]
                 , vID            :: Int
                 , vLoop          :: Bool
                 , vFilePath      :: FilePath
                 , vNextFrame     :: (FrameGrabber PixelRGB8)
                 , vCleanupFFmpeg :: (IO ())
                 , vStartTime     :: Maybe Double
                 , vCurrentFrame  :: Int
                 , vFps           :: Maybe Double
                 , vRate          :: Double
                 }
data LoadedVideo
  = LoadedVideo { lvTexUnit       :: GL.TextureUnit
                , lvAssignments   :: [(NodeID, UnitID, Int)]
                , lvID            :: Int
                , lvLoop          :: Bool
                , lvFrames        :: V.Vector (GL.TextureObject, Double)
                , lvNumFrames     :: Int
                , lvStartTime     :: Maybe Double
                , lvCurrentFrame  :: Int
                , lvFps           :: Maybe Double
                , lvRate          :: Double
                }
data ImageTexture
  = ImageTexture { iTexObj      :: GL.TextureObject
                 , iTexUnit     :: GL.TextureUnit
                 , iAssignments :: [(NodeID, UnitID, Int)]
                 , iID          :: Int
                 , iImageRGB8   :: (Image PixelRGB8)
                 , iIsBound     :: Bool
                 }

instance Eq Texture where
  Vid _ == Img _ = False
  Img _ == Vid _ = False
  LVd _ == Vid _ = False
  Vid _ == LVd _ = False
  LVd _ == Img _ = False
  Img _ == LVd _ = False
  Vid v1 == Vid v2 = v1 == v2
  Img i1 == Img i2 = i1 == i2
  LVd v1 == LVd v2 = v1 == v2

instance Eq VideoTexture where
  VideoTexture tObj1 tUnit1 _ _ _ _ _ _ _ _ _ _ == VideoTexture tObj2 tUnit2 _ _ _ _ _ _ _ _ _ _ =
    tObj1 == tObj2 && tUnit1 == tUnit2

instance Eq ImageTexture where
  ImageTexture tObj1 tUnit1 _ _ _ _ == ImageTexture tObj2 tUnit2 _ _ _ _ =
    tObj1 == tObj2 && tUnit1 == tUnit2

instance Eq LoadedVideo where
  LoadedVideo tUnit1 _ vID1 _ _ _ _ _ _ _ == LoadedVideo tUnit2 _ vID2 _ _ _ _ _ _ _ =
    tUnit1 == tUnit2 && vID1 == vID2

instance TextureClass VideoTexture where
  texObj t = vTexObj t
  texUnit t = vTexUnit t
  assignments t = vAssignments t
  texID t = vID t

instance TextureClass ImageTexture where
  texObj t = iTexObj t
  texUnit t = iTexUnit t
  assignments t = iAssignments t
  texID t = iID t

instance TextureClass LoadedVideo where
  texObj t = fst $ (lvFrames t) V.! (lvCurrentFrame t)
  texUnit t = lvTexUnit t
  assignments t = lvAssignments t
  texID t = lvID t

instance TextureClass Texture where
  texObj (Vid t) = texObj t
  texObj (Img t) = texObj t
  texObj (LVd t) = texObj t
  texUnit (Vid t) = texUnit t
  texUnit (Img t) = texUnit t
  texUnit (LVd t) = texUnit t
  assignments (Vid t) = assignments t
  assignments (Img t) = assignments t
  assignments (LVd t) = assignments t
  texID (Vid t) = texID t
  texID (Img t) = texID t
  texID (LVd t) = texID t






containsVideoUGen :: [Text] -> [SCUnit] -> Bool
containsVideoUGen glUGenNames units =
  elem True $ map (\unit -> elem (scUnitName unit) glUGenNames) units


partitionOn :: Text -> Maybe [Int]
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
