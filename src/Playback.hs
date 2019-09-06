module Playback (play, displayImage, displayDelBuf) where


import Prelude (putStrLn)
import RIO
import RIO.Partial
import qualified RIO.Seq as Seq
import qualified RIO.Vector.Partial as V'

import Codec.FFmpeg
import Codec.FFmpeg.Common
import Codec.FFmpeg.Decode
import Codec.FFmpeg.Juicy
import Codec.Picture
import Control.Monad (replicateM)
import Control.Monad.Except (runExceptT)
import Control.Monad.Trans.Maybe (MaybeT(..), runMaybeT)
-- import Fmt
import Foreign.Marshal.Alloc (free, mallocBytes)
import qualified Graphics.Rendering.OpenGL as GL
import qualified Graphics.UI.GLFW as GLFW

import GLUtils
import Types


displayImage :: ImageTexture -> RIO ShaderState ()
displayImage image = forM_ (iAssignments image) $ \assignment ->
  bindTexture (iTexObj image) assignment



displayDelBuf :: DelBuf -> RIO ShaderState ()
displayDelBuf delBuf = case dbBuses delBuf of
  Seq.Empty              -> return ()
  _ Seq.:|> (Bus _ tObj) -> do
    assignments <- readIORef $ dbAssignments delBuf
    -- liftIO $ putStrLn $ "\n*** Debug: displaying delay buffer "+||delBuf||+""
    forM_ assignments $ \assignment -> do
      _success <- bindTexture tObj assignment
      -- liftIO $ putStrLn $ "*** Debug: bindTexture "+||tObj||+" "+||assignment||+" - "+|success|+""
      return ()



play :: Player -> RIO ShaderState (Maybe Player)
play (PlayVid bp) = basicPlayback bp >>= return . (fmap PlayVid)
play (VidRd ph)   = playbackHead ph  >>= return . (fmap VidRd)



playbackHead :: PlaybackHead -> RIO ShaderState (Maybe PlaybackHead)
playbackHead player = ask >>= \env -> liftIO $
  case phVideo player of
    OnDiskVid _ -> return Nothing
    InMemVid _ (VideoData framesRef numFrames) -> do
      frames <- readIORef framesRef
      headPos <- readTVarIO $ phHeadPos player
      let rawIndex   = round $ headPos * (fromIntegral numFrames)
          frameIndex = rawIndex `mod` numFrames
          frameTObj  = fst $ frames V'.! frameIndex

      _success <- runRIO env $ bindTexture frameTObj (phAssignment player)

      return $ Just player
      -- if success
      --   then return $ Just player
      --   else return Nothing



basicPlayback :: BasicPlayer -> RIO ShaderState (Maybe BasicPlayer)
basicPlayback player = do
  if | isJust (bpOnDiskPlaybackTools player) -> continueOnDiskPlayback
     | isOnDiskVid (bpVideo player)    -> beginOnDiskPlayback (0 :: Int)
     | isJust (bpStartTime player)     -> continueInMemPlayback
     | otherwise                       -> beginInMemPlayback (0 :: Int)
  where
    beginInMemPlayback :: Int -> RIO ShaderState (Maybe BasicPlayer)
    beginInMemPlayback startingFrame = do
      let (InMemVid _ videoData) = bpVideo player
          VideoData framesRef numFrames = videoData
      frames <- readIORef framesRef
      let frameTObj = fst (frames V'.! startingFrame)
      success <- bindTexture frameTObj (bpAssignment player)
      if not success
        then return Nothing
        else do
          glCurrentTime <- liftIO GLFW.getTime
          -- NOTE: assumes constant frame rate. Incorrect behaviour for videos
          --       with variable frame rate, same for on-disk playback.
          let framerate = (fromIntegral numFrames) / (snd . V'.last) frames
              playbackTools = InMemPlaybackTools framerate startingFrame
          return $ Just $ player { bpStartTime = glCurrentTime
                                 , bpInMemPlaybackTools = Just playbackTools
                                 }

    beginOnDiskPlayback :: Int -> RIO ShaderState (Maybe BasicPlayer)
    beginOnDiskPlayback startingFrame = ask >>= \env -> liftIO $ do
      let (OnDiskVid video) = bpVideo player
      eitherPlaybackTools <- liftIO $ setupPlayback video
      case eitherPlaybackTools of
        Left e -> do
          putStrLn $ "*** Error: FFmpeg setup unsuccessful. " ++ e
          return Nothing
        Right playbackTools -> do
          _ <- liftIO $ replicateM startingFrame $ odptSkipFrame playbackTools
          frame <- liftIO $ odptNextFrame playbackTools
          case frame of
            Nothing -> return $ Just player
            Just (imageRGBA8, _timestamp) -> do
              writeImageToTexture (odptTextureObject playbackTools) imageRGBA8

              success <- runRIO env $ bindTexture (odptTextureObject playbackTools)
                                                    (bpAssignment player)

              if not success then return Nothing
              else do
                glCurrentTime <- GLFW.getTime
                rate <- readTVarIO $ bpRate player

                let framerate     = odptFps playbackTools
                    frameInterval = 1 / (framerate * rate)
                    skippedFrames = fromIntegral startingFrame
                    startTime     = (\t -> t - (skippedFrames * frameInterval))
                                 <$> glCurrentTime

                return $ Just $
                  player { bpStartTime     = startTime
                         , bpOnDiskPlaybackTools = Just playbackTools
                         }

    continueInMemPlayback :: RIO ShaderState (Maybe BasicPlayer)
    continueInMemPlayback = do
      glCurrentTime <- liftIO GLFW.getTime
      rate <- readTVarIO $ bpRate player
      loop <- readTVarIO $ bpLoop player
      let (InMemVid _ videoData) = bpVideo player
          playbackTools  = fromJust $ bpInMemPlaybackTools player
          startTime      = bpStartTime player
          currentTime    = (-) <$> glCurrentTime <*> startTime
          framerate      = imptFps playbackTools
          frameInterval  = 1 / (framerate * rate)
          scheduledFrame = fromMaybe 0 $ floor <$> (/ frameInterval) <$> currentTime
          currentFrame   = imptCurrentFrame playbackTools
          framesRef      = vFrames videoData
          numFrames      = vNumFrames videoData
      frames <- readIORef framesRef
      if | scheduledFrame <= currentFrame -> return $ Just player
         | (scheduledFrame > numFrames - 1) && (not loop) -> return Nothing
         | scheduledFrame > numFrames - 1 -> do -- loop back to start
             let currentFrame' = scheduledFrame `mod` numFrames
                 (frameTObj, _) = frames V'.! currentFrame'
             success <- bindTexture frameTObj (bpAssignment player)
             let startTime' = (\t -> t - ((fromIntegral currentFrame') * frameInterval))
                           <$> glCurrentTime
             if not success
               then return $ Just
                           $ player { bpStartTime = startTime'
                                    , bpInMemPlaybackTools = Just $
                                        playbackTools { imptCurrentFrame = 0 }
                                    }
               else return $ Just
                           $ player { bpStartTime = startTime'
                                    , bpInMemPlaybackTools = Just $
                                        playbackTools { imptCurrentFrame = currentFrame' }
                                    }
         | otherwise -> do
             let (frameTObj, _) = frames V'.! scheduledFrame
             success <- bindTexture frameTObj (bpAssignment player)
             if not success
               then return $ Just player
               else return $ Just
                           $ player { bpInMemPlaybackTools = Just $
                                        playbackTools { imptCurrentFrame = scheduledFrame }
                                    }

    continueOnDiskPlayback :: RIO ShaderState (Maybe BasicPlayer)
    continueOnDiskPlayback = ask >>= \env -> do
      glCurrentTime <- liftIO GLFW.getTime
      rate <- readTVarIO $ bpRate player
      loop <- readTVarIO $ bpLoop player
      let playbackTools  = fromJust $ bpOnDiskPlaybackTools player
          startTime      = bpStartTime player
          currentTime    = (-) <$> glCurrentTime <*> startTime
          framerate      = odptFps playbackTools
          frameInterval  = 1 / (framerate * rate)
          scheduledFrame = fromMaybe 0 $ floor <$> (/ frameInterval) <$> currentTime
          currentFrame   = odptCurrentFrame playbackTools

      if scheduledFrame <= currentFrame then return $ Just player
      else do
        let skippedFrames = scheduledFrame - currentFrame - 1

        -- liftIO $ putStrLn $ "\n\n\nstartTime = " ++ (show startTime)
        -- liftIO $ putStrLn $ "glCurrentTime = " ++ (show glCurrentTime)
        -- liftIO $ putStrLn $ "currentTime = " ++ (show currentTime)
        -- liftIO $ putStrLn $ "framerate = " ++ (show framerate)
        -- liftIO $ putStrLn $ "frameInterval = " ++ (show frameInterval)
        -- liftIO $ putStrLn $ "scheduledFrame = " ++ (show scheduledFrame)
        -- liftIO $ putStrLn $ "currentFrame = " ++ (show currentFrame)
        -- liftIO $ putStrLn $ "skippedFrames = " ++ (show skippedFrames)

        -- skip over any missed frames
        skipResults <- liftIO $ replicateM skippedFrames $ odptSkipFrame playbackTools

        frame <- liftIO $ odptNextFrame playbackTools
        case frame of
          Nothing -> maybeLoopVideo skipResults loop
          Just (imageRGBA8, _timestamp) -> do
            liftIO $ writeImageToTexture (odptTextureObject playbackTools) imageRGBA8

            success <- runRIO env $ bindTexture (odptTextureObject playbackTools)
                                                (bpAssignment player)
            if not success
            then return $ Just player -- Nothing
            else return $ Just $
              player { bpOnDiskPlaybackTools = Just $
                         playbackTools { odptCurrentFrame = scheduledFrame }
                     }

    maybeLoopVideo :: [Bool] -> Bool -> RIO ShaderState (Maybe BasicPlayer)
    maybeLoopVideo skipFrameResults shouldLoop
      | shouldLoop = beginOnDiskPlayback (length.(filter not) $ skipFrameResults)
      | otherwise  = let pts = fromJust $ bpOnDiskPlaybackTools player
                     in  do liftIO $ odptCleanupFFmpeg pts
                            liftIO $ GL.deleteObjectName . odptTextureObject $ pts
                            return Nothing



setupPlayback :: VideoFile -> IO (Either String OnDiskPlaybackTools)
setupPlayback video = runExceptT $ do
  {- to get FPS, need to `getTimeBase` from the `AVStream`.
     this returns a numerator and a denominator.
     then need to get the `pts` from the packet (timestamp in `time_base` units)
     the ffmpeg-light library then calculates the frame timestamp as:
         (pts * numerator) / denominator
     the FPS (assuming it is constant) is just:
         denominator / numerator

     Note however that libav states using time_base for decoding is deprecated and
     `framerate` should be used instead. `framerate` = 1 / `time_base`.

     Also note, calculating the FPS in this manner is only valid for fixed-fps
     content. Variable-fps content cannot have the FPS calculated for the sake of
     scheduling frames -- the timestamps of each frame would need to be read and
     displayed based on those.
     * For loaded videos, can ignore the FPS.
     * Calculating the FPS is easier when reading from disk because it allows a number
       of frames to be skipped more easily?


     To skip frames, want to run `read_frame_check`. This needs an
     `AvFormatContext` and an `AVPacket`. I think the `AVPacket` just needs
     to be allocated in memory, then freed after.
  -}
  liftIO $ initFFmpeg
  avFormatContext <- openInput $ File $ vFilePath video
  checkStreams avFormatContext
  (vidStreamIndex, ctx, cod, vidStream) <- findVideoStream avFormatContext
  _ <- openCodec ctx cod
  let dstFmt = juicyPixelFormat ([] :: [PixelRGBA8])
  (reader, cleanup) <- prepareReader avFormatContext vidStreamIndex dstFmt ctx
  AVRational num den <- liftIO $ getTimeBase vidStream
  let framerate = (fromIntegral den) / (1000 * fromIntegral num)
      (numl, dend) = (fromIntegral num, fromIntegral den)
      frameTime' frame = do n <- getPts frame
                            return $ fromIntegral (n * numl) / dend
      readTS = do frame <- reader
                  case frame of
                    Nothing -> return Nothing
                    Just f -> do t <- frameTime' f
                                 return $ Just (f, t)

  (frameGrabber, cleanup') <- liftIO $ getImageReader (readTS, cleanup)

  let skipFrame = do pkt <- AVPacket <$> mallocBytes packetSize
                     r <- av_read_frame avFormatContext pkt
                     free_packet pkt
                     free (getPtr pkt)
                     return (r >= 0)

  -- TODO: make texture repeat and filtering settings configurable
  textureObject <- liftIO $ setupTexture

  return $ OnDiskPlaybackTools { odptNextFrame = frameGrabber
                               , odptSkipFrame = skipFrame
                               , odptCleanupFFmpeg = cleanup'
                               , odptFps = framerate
                               , odptTextureObject = textureObject
                               , odptCurrentFrame = 0
                               }



{- Read time stamped frames from a video stream. Time is given in seconds from
   the start of the stream. Frames are read in the specified JuicyPixels format.
   Errors are thrown as 'IOException's.
-}
getImageReader :: forall p . JuicyPixelFormat p
                  => (IO (Maybe (AVFrame, Double)), IO ()) -> IO (FrameGrabber p, IO ())
getImageReader = (>>= either error return)
               . runExceptT
               . return
               . (first (runMaybeT . aux toJuicyImage))
  where aux g x = do (f,t) <- MaybeT x
                     f' <- MaybeT $ g f
                     return (f', t)
