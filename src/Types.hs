module Types where


import RIO
import RIO.Seq (Seq)

import Codec.Picture
import Data.Aeson
import qualified Graphics.Rendering.OpenGL as GL
import qualified Graphics.UI.GLFW as GLFW
import System.IO.Unsafe (unsafePerformIO)
import System.ZMQ4 (Socket, Sub)


data CmdLineOpts = CmdLineOpts
  { version :: Bool
  , cmdMsgSock :: String
  , dataMsgSock :: String
  , responseSock :: String
  }

type NodeID = Int
data Node = Node [ShaderProgram] | Group (IntMap Node)

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

data InBus  = InBus WireID Bus deriving (Eq, Show)
data OutBus = OutBus WireID Bus deriving (Eq, Show)

data Bus = Bus GL.FramebufferObject GL.TextureObject deriving (Eq, Show)

data DelBuf = DelBuf
  { dbID          :: Int
  , dbAssignments :: IORef [Assignment]
  , dbBuses       :: Seq Bus
  }

instance Show DelBuf where
  show (DelBuf bID bAs bBs) =
    "DelBuf {delBufID = "           ++ (show bID)
        ++ ", delBufAssignments = " ++ (show $ unsafePerformIO (readIORef bAs))
        ++ ", delBufBuses = "       ++ (show bBs)
        ++ "}"

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
data ShaderProgram = ShaderProgram GL.Program [WireID] WireID deriving (Eq)




{- WINDOW -}


data WindowState = WindowState
  { wsWindow :: GLFW.Window
  , wsWindowID :: WindowID
  , wsWidth  :: Int -- TODO?: make the width and height IORefs?
  , wsHeight :: Int
  , wsShouldExit :: IORef Bool
  , wsMsgQIn :: TBQueue ExternalMsg
  , wsMsgQOut :: TBQueue Msg
  , wsSubSocket :: Socket Sub
  , wsImages :: IORef (IntMap ImageTexture)
  , wsVideos :: IORef (IntMap Video)
  , wsPlayers :: IORef (Map Assignment Player)
  , wsDelBufs :: IORef (IntMap DelBuf)
  , wsBuses :: IORef (Map (NodeID, WireID) Bus)
  , wsNodeTree :: IORef (IntMap Node)
  , wsUniformVals :: TBQueue UnitData
  , wsUpdateQueue :: TBQueue WindowUpdate
  }




{- RENDER -}


data RenderState = RenderState
  { rsWindowState :: IORef WindowState
  , rsScreenShader :: GL.Program
  , rsVAO :: GL.VertexArrayObject
  -- , rsBuses :: IORef (Map (NodeID, WireID) Bus)
  , rsDefaultOutBus :: Bus
  , rsMaxTexUnits :: GL.GLsizei
  }

data ShaderState = ShaderState
  { ssTextureUnits :: IORef [GL.TextureUnit]
  , ssShaderProgram :: GL.Program
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
                , windowID  :: WindowID
                }
  | GLVideoRead { videoID   :: Int
                , videoPath :: FilePath
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
  | DelBufNew  { bufferID  :: Int
               , bufferLen :: Int
               , windowID  :: WindowID
               }
  | DelBufFree { bufferID :: Int
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

data WindowUpdate
  = WUDelBufRd Int Assignment
  | WUDelBufWr NodeID Int WireID
  | WUAssignImage Int Assignment
  | WUPlayVid Int Assignment Double Bool
  | WUVidRd Int Assignment Float
  deriving (Eq, Show)




{- VIDEOS + PLAYBACK -}


data Video = OnDiskVid VideoFile
           | InMemVid  VideoFile VideoData
           -- | VideoBuffer

isOnDiskVid :: Video -> Bool
isOnDiskVid (OnDiskVid _) = True
isOnDiskVid _ = False

isInMemVid :: Video -> Bool
isInMemVid (InMemVid _ _) = True
isInMemVid _ = False

class HasVideoID t where
  getVideoID :: t -> Int

instance HasVideoID Video where
  getVideoID (OnDiskVid v)  = vID v
  getVideoID (InMemVid v _) = vID v

instance HasVideoID Player where
  getVideoID (PlayVid bp) = getVideoID.bpVideo $ bp
  getVideoID (VidRd ph)   = getVideoID.phVideo $ ph

instance HasVideoID BasicPlayer where
  getVideoID bp = getVideoID.bpVideo $ bp

instance HasVideoID PlaybackHead where
  getVideoID ph = getVideoID.phVideo $ ph

data VideoFile = VideoFile
  { vID :: Int
  , vFilePath :: FilePath
  }
data VideoData = VideoData
  { vFrames    :: IORef (Vector (GL.TextureObject, Double))
  , vNumFrames :: Int
  }

data Player = PlayVid BasicPlayer
            | VidRd   PlaybackHead

isPlayVid :: Player -> Bool
isPlayVid (PlayVid _) = True
isPlayVid _ = False

isVidRd :: Player -> Bool
isVidRd (VidRd _) = True
isVidRd _ = False


data BasicPlayer = BasicPlayer
  { bpVideo               :: Video
  , bpAssignment          :: Assignment
  , bpRate                :: TVar Double
  , bpLoop                :: TVar Bool
  , bpStartTime           :: Maybe Double
  , bpOnDiskPlaybackTools :: Maybe OnDiskPlaybackTools
  , bpInMemPlaybackTools  :: Maybe InMemPlaybackTools
  }

-- TODO: This is pretty gross at the moment. Restructure more nicely, use
--       shorter field and type names etc.

data OnDiskPlaybackTools = OnDiskPlaybackTools
  { odptNextFrame     :: FrameGrabber PixelRGBA8
  , odptSkipFrame     :: IO Bool
  , odptCleanupFFmpeg :: IO ()
  , odptFps           :: Double
  , odptTextureObject :: GL.TextureObject
  , odptCurrentFrame  :: Int
  }

data InMemPlaybackTools = InMemPlaybackTools
  { imptFps           :: Double
  , imptCurrentFrame  :: Int
  }

data PlaybackHead = PlaybackHead
  { phVideo :: Video
  , phAssignment :: Assignment
  , phHeadPos :: TVar Float
  }

playerAssignment :: Player -> Assignment
playerAssignment = \case PlayVid bp -> bpAssignment bp
                         VidRd   ph -> phAssignment ph


{- TEXTURES -}


type FrameGrabber p = IO (Maybe (Image p, Double))
type Assignment = (NodeID, UnitID, Int)

data ImageTexture
  = ImageTexture { iTexObj      :: GL.TextureObject
                 , iAssignments :: [(NodeID, UnitID, Int)]
                 , iID          :: Int
                 }



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
    "DelBufWr"  -> Just [0]
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
