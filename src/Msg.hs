{-# LANGUAGE DeriveGeneric #-}

module Msg ( UnitData(..)
           , Msg(..)
           , TextureUpdate(..)
           , getUnitData
           , removeNullChar
           , receiveDataMsg
           ) where


import Control.Concurrent.STM
import Data.Aeson
import Data.Binary (Get)
import Data.Binary.Get (runGet, getInt32host, getFloathost, getWord16host)
import Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Char8 as C
import Data.ByteString.Lazy (fromStrict)
import Data.Int (Int32)
import Data.Word
import GHC.Generics
import System.ZMQ4 (Socket, Sub, receive)

import Texture
-- import Transform
import Types


data Msg
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
  | InternalWindowFree { windowID :: WindowID }
  | InternalServerQuit
  deriving (Generic, Show)

instance FromJSON Msg

data DataMsg = UnitDataMsg    RawUnitData
             | AssignVideoMsg RawAssignVideo
             | AssignImageMsg RawAssignImage
             | InvalidMsg Word16

data RawUnitData = RawUnitData
  { rawUDataInput   :: !Word16
  , rawUDataNodeID :: !Int32
  , rawUDataUnitID  :: !Int32
  , rawUDataValue   :: !Float
  } deriving (Show)

data RawAssignVideo = RawAssignVideo
  { rawAVInput   :: !Word16
  , rawAVNodeID :: !Int32
  , rawAVUnitID  :: !Int32
  , rawAVVideoID :: !Int32
  } deriving (Show)

data RawAssignImage = RawAssignImage
  { rawAIInput   :: !Word16
  , rawAINodeID :: !Int32
  , rawAIUnitID  :: !Int32
  , rawAIImageID :: !Int32
  } deriving (Show)

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


receiveDataMsg :: Socket Sub -> WindowID -> TBQueue UnitData -> TVar [Texture] -> TBQueue TextureUpdate -> IO ()
receiveDataMsg s _wID uniformVals textures textureQueue = do
  msg <- receive s

  case parseBinaryMsg msg of
    UnitDataMsg (RawUnitData uIn16 gID32 uID32 uVal) -> do
      let gID     = fromIntegral gID32 :: Int
          uID     = fromIntegral uID32 :: Int
          uIn     = fromIntegral uIn16 :: Int
          dataMsg = UnitData gID uID uIn uVal

      atomically $ writeTBQueue uniformVals dataMsg


    AssignVideoMsg (RawAssignVideo uIn16 gID32 uID32 vidID32) -> do
      let gID   = fromIntegral gID32   :: Int
          uID   = fromIntegral uID32   :: Int
          uIn   = fromIntegral uIn16   :: Int
          vidID = fromIntegral vidID32 :: Int

      -- putStrLn "*** Debug: received AssignVideoMsg"

      textures' <- readTVarIO textures
      let assignmentExists = elem True $ flip map textures' $ \tex ->
            case tex of Vid texture ->
                          texID texture == vidID && elem (gID, uID, uIn) (assignments texture)
                        LVd texture ->
                          texID texture == vidID && elem (gID, uID, uIn) (assignments texture)
                        _ -> False

      if not assignmentExists then do
        -- putStrLn "*** Debug: writing AssignVideoMsg to queue"
        atomically $ writeTBQueue textureQueue (AssignVideo (gID, uID, uIn) vidID)
        -- putStrLn "*** Debug:     finished writing AssignVideoMsg to queue"
      else
        return ()


    AssignImageMsg (RawAssignImage uIn16 gID32 uID32 imgID32) -> do
      let gID   = fromIntegral gID32   :: Int
          uID   = fromIntegral uID32   :: Int
          uIn   = fromIntegral uIn16   :: Int
          imgID = fromIntegral imgID32 :: Int

      textures' <- readTVarIO textures
      let assignmentExists = elem True $ flip map textures' $ \tex ->
            case tex of Img texture ->
                          texID texture == imgID && elem (gID, uID, uIn) (assignments texture)
                        _ -> False

      if not assignmentExists then
        atomically $ writeTBQueue textureQueue (AssignImage (gID, uID, uIn) imgID)
      else
        return ()


    InvalidMsg msgTypeVal-> do
      putStrLn $ "invalid message received. leading word16 has value: " ++ (show msgTypeVal)
      return ()


parseBinaryMsg :: ByteString -> DataMsg
parseBinaryMsg bs =
  let msgType = runGet getWord16host (fromStrict $ ByteString.take 2 bs)
  in  case msgType of
        0 -> UnitDataMsg    $ getUnitData    $ ByteString.drop 2 bs
        1 -> AssignVideoMsg $ getAssignVideo $ ByteString.drop 2 bs
        2 -> AssignImageMsg $ getAssignImage $ ByteString.drop 2 bs
        val -> InvalidMsg val


getUnitData :: ByteString -> RawUnitData
getUnitData bs =
  runGet unitDataGetter $ fromStrict bs
  where
    unitDataGetter :: Get RawUnitData
    unitDataGetter = RawUnitData <$> getWord16host -- input index
                                 <*> getInt32host  -- nodeID
                                 <*> getInt32host  -- unitID
                                 <*> getFloathost  -- value


getAssignVideo :: ByteString -> RawAssignVideo
getAssignVideo bs =
  runGet assignVideoGetter $ fromStrict bs
  where
    assignVideoGetter :: Get RawAssignVideo
    assignVideoGetter = RawAssignVideo <$> getWord16host -- input index
                                       <*> getInt32host  -- nodeID
                                       <*> getInt32host  -- unitID
                                       <*> getInt32host  -- videoID


getAssignImage :: ByteString -> RawAssignImage
getAssignImage bs =
  runGet assignImageGetter $ fromStrict bs
  where
    assignImageGetter :: Get RawAssignImage
    assignImageGetter = RawAssignImage <$> getWord16host -- input index
                                       <*> getInt32host  -- nodeID
                                       <*> getInt32host  -- unitID
                                       <*> getInt32host  -- imageID


removeNullChar :: ByteString -> ByteString
removeNullChar bs =
  if not (C.null bs) && C.last bs == '\NUL'
    then C.init bs
    else bs
