module Msg
  ( getUnitData
  , removeNullChar
  , receiveDataMsg
  ) where


import MyPrelude
import RIO
import RIO.Partial
import qualified RIO.Map as Map
import qualified RIO.Seq as Seq

import Data.Binary (Get)
import Data.Binary.Get
import Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Char8 as C
import Data.ByteString.Lazy (fromStrict)
import Data.Int (Int32)
import qualified Data.IntMap.Strict as IntMap
import System.ZMQ4 (receive)

import Types


data DataMsg = UnitDataMsg    RawUnitData
             | PlayVidMsg     RawPlayVid
             | AssignImageMsg RawAssignImage
             | VidRdMsg       RawVidRd
             | DelBufRdMsg    RawDelBufRd
             | DelBufWrMsg    RawDelBufWr
             | InvalidMsg     Word16

data RawUnitData = RawUnitData
  { rawUDataInput  :: !Word16
  , rawUDataNodeID :: !Int32
  , rawUDataUnitID :: !Int32
  , rawUDataValue  :: !Float
  } deriving (Show)

data RawPlayVid = RawPlayVid
  { rawPVInput   :: !Word16
  , rawPVNodeID  :: !Int32
  , rawPVUnitID  :: !Int32
  , rawPVVideoID :: !Int32
  , rawPVRate    :: !Float
  , rawPVLoop    :: !Int32
  } deriving (Show)

data RawAssignImage = RawAssignImage
  { rawAIInput   :: !Word16
  , rawAINodeID  :: !Int32
  , rawAIUnitID  :: !Int32
  , rawAIImageID :: !Int32
  } deriving (Show)

data RawVidRd = RawVidRd
  { rawVRInput   :: !Word16
  , rawVRNodeID  :: !Int32
  , rawVRUnitID  :: !Int32
  , rawVRVideoID :: !Int32
  , rawVRPhase   :: !Float
  } deriving (Show)

data RawDelBufRd = RawDelBufRd
  { rawDBRdInput  :: !Word16
  , rawDBRdNodeID :: !Int32
  , rawDBRdUnitID :: !Int32
  , rawDBRdBufID  :: !Int32
  } deriving (Show)

data RawDelBufWr = RawDelBufWr
  { rawDBWrInput  :: !Word16
  , rawDBWrNodeID :: !Int32
  , rawDBWrUnitID :: !Int32
  , rawDBWrBufID  :: !Int32
  , rawDBWrWireID :: !Word32
  } deriving (Show)


{- TODO: when queue is full, better to overwrite existing messages in the queue
         than drop new messages.
-}
receiveDataMsg :: RIO WindowState ()
receiveDataMsg = ask >>= \env -> liftIO $ do
  let socket      = wsSubSocket env
      uniformVals = wsUniformVals env
      images      = wsImages env
      videos      = wsVideos env
      players     = wsPlayers env
      delBufs     = wsDelBufs env
      buses       = wsBuses env
      updateQueue = wsUpdateQueue env

  msg <- receive socket

  case parseBinaryMsg msg of
    UnitDataMsg (RawUnitData uIn16 gID32 uID32 uVal) -> do
      let gID     = fromIntegral gID32 :: Int
          uID     = fromIntegral uID32 :: Int
          uIn     = fromIntegral uIn16 :: Int
          dataMsg = UnitData gID uID uIn uVal

      atomically $ writeTBQueue uniformVals dataMsg


    PlayVidMsg (RawPlayVid uIn16 gID32 uID32 vidID32 vRateF vLoop32) -> do
      let gID   = fromIntegral gID32   :: Int
          uID   = fromIntegral uID32   :: Int
          uIn   = fromIntegral uIn16   :: Int
          vidID = fromIntegral vidID32 :: Int
          vLoop = vLoop32 /= (0 :: Int32)
          vRate = if vRateF <= 0
                    -- set minimum rate value to prevent div by 0
                    then 0.000000000001
                    else realToFrac vRateF :: Double
          assignment = (gID, uID, uIn)

      players' <- readIORef players
      videos'  <- readIORef videos

      case IntMap.lookup vidID videos' of
        Nothing -> return ()
        Just _  ->
          case Map.lookup assignment players' of
            Just (PlayVid bp) -> if getVideoID bp == vidID
              then if | isOnDiskBasicPlayer bp -> updateOnDiskBP bp vRate vLoop
                      | otherwise ->              updateInMemBP  bp vRate vLoop
              else pushUpdate updateQueue $ WUPlayVid vidID assignment vRate vLoop
            _ ->   pushUpdate updateQueue $ WUPlayVid vidID assignment vRate vLoop
      where
        -- update video start time to account for the changed playback rate
        updateOnDiskBP bp r' loop = do
          mbt0 <- readTVarIO $ bpStartTime bp
          r    <- readTVarIO $ bpRate bp
          let pts = fromJust $ bpOnDiskPlaybackTools bp
              ti  = odptFrameTS pts
              t0' = (\t0 -> t0 - ti * (r' - r)) <$> mbt0
          atomically $ do writeTVar (bpStartTime bp) t0'
                          writeTVar (bpRate bp) r'
                          writeTVar (bpLoop bp) loop
        updateInMemBP bp r' loop = do
          mbt0 <- readTVarIO $ bpStartTime bp
          r    <- readTVarIO $ bpRate bp
          let pts = fromJust $ bpInMemPlaybackTools bp
              ti  = imptFrameTS pts
              t0' = (\t0 -> t0 - ti * (r' - r)) <$> mbt0
          atomically $ do writeTVar (bpStartTime bp) t0'
                          writeTVar (bpRate bp) r'
                          writeTVar (bpLoop bp) loop


    AssignImageMsg (RawAssignImage uIn16 gID32 uID32 imgID32) -> do
      let gID   = fromIntegral gID32   :: Int
          uID   = fromIntegral uID32   :: Int
          uIn   = fromIntegral uIn16   :: Int
          imgID = fromIntegral imgID32 :: Int
          assignment = (gID, uID, uIn)

      images' <- readIORef images
      case IntMap.lookup imgID images' of
        Nothing  -> return ()
        Just img -> do
          let assignments = (iAssignments img)
              assignmentExists = elem assignment assignments

          if not assignmentExists then
            pushUpdate updateQueue $ WUAssignImage imgID assignment
          else
            return ()


    VidRdMsg (RawVidRd uIn16 gID32 uID32 vidID32 vPhase) -> do
      let gID   = fromIntegral gID32   :: Int
          uID   = fromIntegral uID32   :: Int
          uIn   = fromIntegral uIn16   :: Int
          vidID = fromIntegral vidID32 :: Int
          assignment = (gID, uID, uIn)

      players' <- readIORef players
      videos'  <- readIORef videos

      case IntMap.lookup vidID videos' of
        Nothing -> return ()
        Just _  ->
          case Map.lookup assignment players' of
            Nothing ->
              pushUpdate updateQueue $ WUVidRd vidID assignment vPhase
            Just (VidRd ph) ->
              atomically $ writeTVar (phHeadPos ph) vPhase
            Just (PlayVid _) -> do
              pushUpdate updateQueue $ WUVidRd vidID assignment vPhase


    DelBufRdMsg (RawDelBufRd uIn16 gID32 uID32 bufID32) -> do
      let gID   = fromIntegral gID32   :: Int
          uID   = fromIntegral uID32   :: Int
          uIn   = fromIntegral uIn16   :: Int
          bufID = fromIntegral bufID32 :: Int
          assignment = (gID, uID, uIn)

      delBufs' <- readIORef delBufs

      case IntMap.lookup bufID delBufs' of
        Nothing -> return ()
        Just db -> do
          assignments <- readIORef $ dbAssignments db
          let assignmentExists = elem assignment assignments

          if not assignmentExists then
            pushUpdate updateQueue $ WUDelBufRd bufID assignment
          else
            return ()


    DelBufWrMsg (RawDelBufWr _uIn16 gID32 _uID32 bufID32 wireID32) -> do
      let gID    = fromIntegral gID32    :: Int
          bufID  = fromIntegral bufID32  :: Int
          wireID = fromIntegral wireID32 :: Int

      delBufs' <- readIORef delBufs
      buses'   <- readIORef buses

      case IntMap.lookup bufID delBufs' of
        Nothing -> return ()
        Just db -> do
          let hd Seq.:<| _ = dbBuses db
          case Map.lookup (gID, wireID) buses' of
            Nothing  -> return ()
            Just bus -> if hd == bus then return ()
              else pushUpdate updateQueue $ WUDelBufWr gID bufID wireID


    InvalidMsg msgTypeVal-> do
      putStrLn $ "invalid message received. leading word16 has value: " ++ (show msgTypeVal)
      return ()
  where
    pushUpdate updateQ msg = do
      isFull <- atomically $ isFullTBQueue updateQ
      if isFull then return ()
                else do atomically $ writeTBQueue updateQ msg



parseBinaryMsg :: ByteString -> DataMsg
parseBinaryMsg bs =
  let msgType = runGet getWord16host (fromStrict $ ByteString.take 2 bs)
  in  case msgType of
        0 -> UnitDataMsg    $ getUnitData    $ ByteString.drop 2 bs
        1 -> PlayVidMsg     $ getPlayVid     $ ByteString.drop 2 bs
        2 -> AssignImageMsg $ getAssignImage $ ByteString.drop 2 bs
        3 -> VidRdMsg       $ getVidRd       $ ByteString.drop 2 bs
        4 -> DelBufRdMsg    $ getDelBufRd    $ ByteString.drop 2 bs
        5 -> DelBufWrMsg    $ getDelBufWr    $ ByteString.drop 2 bs
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


getPlayVid :: ByteString -> RawPlayVid
getPlayVid bs =
  runGet assignVideoGetter $ fromStrict bs
  where
    assignVideoGetter :: Get RawPlayVid
    assignVideoGetter = RawPlayVid <$> getWord16host -- input index
                                   <*> getInt32host  -- nodeID
                                   <*> getInt32host  -- unitID
                                   <*> getInt32host  -- videoID
                                   <*> getFloathost  -- playback rate
                                   <*> getInt32host  -- loop (bool where non-zero value is true)


getAssignImage :: ByteString -> RawAssignImage
getAssignImage bs =
  runGet assignImageGetter $ fromStrict bs
  where
    assignImageGetter :: Get RawAssignImage
    assignImageGetter = RawAssignImage <$> getWord16host -- input index
                                       <*> getInt32host  -- nodeID
                                       <*> getInt32host  -- unitID
                                       <*> getInt32host  -- imageID


getVidRd :: ByteString -> RawVidRd
getVidRd bs =
  runGet playbackHeadGetter $ fromStrict bs
  where
    playbackHeadGetter :: Get RawVidRd
    playbackHeadGetter = RawVidRd <$> getWord16host -- input index
                                  <*> getInt32host  -- nodeID
                                  <*> getInt32host  -- unitID
                                  <*> getInt32host  -- videoID
                                  <*> getFloathost  -- phase


getDelBufRd :: ByteString -> RawDelBufRd
getDelBufRd bs =
  runGet delBufRdGetter $ fromStrict bs
  where
    delBufRdGetter :: Get RawDelBufRd
    delBufRdGetter = RawDelBufRd <$> getWord16host -- input index
                                 <*> getInt32host  -- nodeID
                                 <*> getInt32host  -- unitID
                                 <*> getInt32host  -- bufID


getDelBufWr :: ByteString -> RawDelBufWr
getDelBufWr bs =
  runGet delBufWrGetter $ fromStrict bs
  where
    delBufWrGetter :: Get RawDelBufWr
    delBufWrGetter = RawDelBufWr <$> getWord16host -- input index (unused)
                                 <*> getInt32host  -- nodeID
                                 <*> getInt32host  -- unitID (unused)
                                 <*> getInt32host  -- bufID
                                 <*> getWord32host -- wireID


removeNullChar :: ByteString -> ByteString
removeNullChar bs =
  if not (C.null bs) && C.last bs == '\NUL'
    then C.init bs
    else bs
