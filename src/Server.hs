module Server
  ( runServer
  ) where


import Prelude (putStrLn)
import RIO

import Control.Concurrent (forkFinally, forkOS, killThread)
import Control.Monad (forever, forM_)
import Control.Monad.Loops (whileM_)
import Data.Aeson
import qualified Data.ByteString.Char8 as C
import Data.ByteString.Lazy.Char8 (fromStrict, toStrict)
import qualified Data.IntMap.Strict as IntMap
import System.IO.Unsafe (unsafePerformIO)
import System.Signal
import System.ZMQ4 hiding (message)

import Msg
import Shader (shaderName, ugenShaderFns)
import Types
import Window


children :: MVar [MVar ()]
children = unsafePerformIO (newMVar [])

waitForChildren :: IO ()
waitForChildren = do
  cs <- takeMVar children
  case cs of
    []   -> return ()
    m:ms -> do
      putMVar children ms
      takeMVar m
      waitForChildren

forkChildIO :: IO () -> IO ThreadId
forkChildIO io = do
  mvar <- newEmptyMVar
  childs <- takeMVar children
  putMVar children (mvar:childs)
  forkFinally io (\_ -> putMVar mvar ())

forkChildOS :: IO () -> IO ThreadId
forkChildOS io = do
  mvar <- newEmptyMVar
  childs <- takeMVar children
  putMVar children (mvar:childs)
  forkOSFinally io (\_ -> putMVar mvar ())
  where
    forkOSFinally :: IO a -> (Either SomeException a -> IO ()) -> IO ThreadId
    forkOSFinally action and_then =
      mask $ \restore ->
        forkOS $ try (restore action) >>= and_then


data Env = Env
  { envCtx         :: !Context
  , envOpts        :: !CmdLineOpts
  , envGlUgenNames :: ![Text]
  , envMsgQueues   :: !(IORef (IntMap.IntMap (TBQueue ExternalMsg)))
  , envMsgQIn      :: !(TBQueue Msg)
  , envMsgQOut     :: !(TBQueue Response)
  , envSendThread  :: !ThreadId
  , envRecvThread  :: !ThreadId
  , envServerExit  :: !(MVar ())
  }


runServer :: CmdLineOpts -> IO ()
runServer opts = withContext $ \ctx -> do
  msgQIn  <- newTBQueueIO 1000 :: IO (TBQueue Msg)
  msgQOut <- newTBQueueIO 1000 :: IO (TBQueue Response)
  messageQueues <- newIORef (IntMap.empty :: IntMap.IntMap (TBQueue ExternalMsg))

  -- putStrLn $ show ugenShaderFns

  {- Listen for messages sent over IPC and push them onto the server's message
     queue.
  -}
  recvThread <- forkChildIO $ withSocket ctx Pull $ \sock -> do
    connect sock $ cmdMsgSock opts -- "ipc://@scvid_cmd-msgs"
    forever $ do
      rawMsg <- receive sock
      let msg = removeNullChar rawMsg
          mbMsg = decode' (fromStrict msg) :: Maybe ExternalMsg

      case mbMsg of
        Nothing -> putStrLn "*** Error: Invalid JSON received."
        Just m  -> atomically $ writeTBQueue msgQIn (External m)

  {- Read messages from the out queue and send them over IPC to scsynth.
  -}
  sendThread <- forkChildIO $ withSocket ctx Push $ \sock -> do
    bind sock $ responseSock opts -- "ipc://@scvid_responses"
    forever $ do
      msg <- atomically $ readTBQueue msgQOut >>= return.toStrict.encode
      send sock [] msg

  -- use the BangPatterns language extensions to force shader files to be read
  -- from disk at this point if that is the result of ugenShaderFns, otherwise
  -- there may be an undesirable pause when the first GraphNew is received.
  let !glUGenNames = map shaderName ugenShaderFns

  serverExitMVar <- newEmptyMVar

  let env = Env { envCtx = ctx
                , envOpts = opts
                , envGlUgenNames = glUGenNames
                , envMsgQueues = messageQueues
                , envMsgQIn = msgQIn
                , envMsgQOut = msgQOut
                , envSendThread = sendThread
                , envRecvThread = recvThread
                , envServerExit = serverExitMVar
                }

  installHandler sigABRT $ \_signal -> runRIO env $ handleInternalMsg ServerQuit
  installHandler sigINT  $ \_signal -> runRIO env $ handleInternalMsg ServerQuit
  installHandler sigTERM $ \_signal -> runRIO env $ handleInternalMsg ServerQuit

  serverExit <- newIORef False

  {- Handle any messages added to the message queue.
  -}
  whileM_ (fmap not $ readIORef serverExit) $ do
    result <- race (readMVar serverExitMVar) (atomically $ readTBQueue msgQIn)

    case result of
      Left ()   -> writeIORef serverExit True
      Right msg ->
        case msg of External m -> runRIO env $ handleExternalMsg m
                    Internal m -> runRIO env $ handleInternalMsg m



handleExternalMsg :: ExternalMsg -> RIO Env ()
handleExternalMsg m = ask >>= \env -> case m of
  {- GLWindowNew: spawn a new GLFW window running a 'blank' shader program and
     check for messages on its dedicated queue. Performing these messages
     updates the running shader program.

     If a window is already running, ignore the message. Multiple windows
     running in parallel is not to be implemented unless as an extension goal of
     the project.
  -}
  GLWindowNew winID wWidth wHeight -> do
    let ctx           = envCtx env
        opts          = envOpts env
        msgQIn        = envMsgQIn env
        messageQueues = envMsgQueues env
    messageQueues' <- readIORef messageQueues
    -- If the map isn't empty (a window exists) then do nothing. Only a
    -- single output window is currently supported.
    if not $ IntMap.null messageQueues' then return () else do
      msgQ <- newTBQueueIO 1000
      writeIORef messageQueues $ IntMap.insert winID msgQ messageQueues'

      _threadID <- liftIO $ forkChildOS $ withSocket ctx Sub $ \s -> do
        subscribe s $ C.pack ""
        connect s $ dataMsgSock opts -- "ipc://@scvid_data-msgs"
        runRIO () $ newWindow s msgQ msgQIn winID wWidth wHeight

      return ()


  {- GLWindowFree: close the GLFW window with the specified window ID, clean up
     and free any associated resources.
  -}
  wFreeMsg@(GLWindowFree winID) -> do
    let messageQueues = envMsgQueues env
    forwardMsg winID wFreeMsg
    modifyIORef' messageQueues $ \mq -> IntMap.delete winID mq


  {- GraphNew: compile the GLSL fragment shader from the graph structure, spawn
     a new GLFW window and run the shader program in that window.
  -}
  graphNewMsg@(GraphNew _gID gUnits) -> do
    let glUGenNames = envGlUgenNames env
    if not (containsVideoUGen glUGenNames gUnits) then return () else do
      msgQs <- readIORef $ envMsgQueues env
      forM_ (IntMap.keys msgQs) $ \winID ->
        forwardMsg winID graphNewMsg


  {- GraphFree: close the window associated with the specified graph ID, clean
     up and free any resources used.
  -}
  GraphFree gphID -> do
    msgQs <- readIORef $ envMsgQueues env
    forM_ (IntMap.keys msgQs) $ \winID ->
      forwardMsg winID (GraphFree gphID)


  {- GLVideoNew: prepare a video for playback in the window specified by the
     window ID. This just forwards the message to the window by putting the
     message on the window's queue.
  -}
  vNewMsg@(GLVideoNew _vID _vPath winID) ->
    forwardMsg winID vNewMsg


  {- GLVideoRead: load a video into memory in the window specified by the window
     ID and prepare it for playback. This just forwards the message to the
     window by putting the message on the window's queue.
  -}
  vReadMsg@(GLVideoRead _vID _vPath winID) ->
    forwardMsg winID vReadMsg


  {- GLVideoFree: free up the resources associated with the video in the window
     specified by the window ID.
  -}
  vFreeMsg@(GLVideoFree _vID winID) ->
    forwardMsg winID vFreeMsg


  {- GLImageNew: load an image from the supplied file path in the window
     specified by the window ID. This just forwards the message to the window by
     putting the message on the window's queue.
  -}
  iNewMsg@(GLImageNew _vID _vPath winID) ->
    forwardMsg winID iNewMsg


  {- GLImageFree: delete the image of specified ID in that window. Do nothing if
     an image with that ID doesn't exist.
  -}
  iFreeMsg@(GLImageFree _vID winID) ->
    forwardMsg winID iFreeMsg


  {- DelBufNew: allocate a new delay buffer (a buffer of frames) of the
     specified length, replacing and freeing any existing buffer with that
     buffer ID.
  -}
  delBufNewMsg@(DelBufNew _bufID _bufLen winID) ->
    forwardMsg winID delBufNewMsg


  {- DelBufFree: delete any existing delay buffer with the specified buffer ID,
     freeing any associated resources and doing nothing if it doesn't exist.
  -}
  delBufFreeMsg@(DelBufFree _bufID winID) ->
    forwardMsg winID delBufFreeMsg


  {- TODO
  -}
  vidDurMsg@(InVidDur _reqID _vID winID) ->
    forwardMsg winID vidDurMsg


  {- TODO
  -}
  vidFramesMsg@(InVidFrames _reqID _vID winID) ->
    forwardMsg winID vidFramesMsg


  {- TODO
  -}
  vidFrameRateMsg@(InVidFrameRate _reqID _vID winID) ->
    forwardMsg winID vidFrameRateMsg


handleInternalMsg :: InternalMsg -> RIO Env ()
handleInternalMsg m = do
  env <- ask
  case m of
    {- InternalWindowFree: a message which can only be passed internally from
       an open window to the main server via a message queue. This handles the
       case where the output window is closed via the window manager and that
       window's message queue needs to be deleted from the server.
    -}
    WindowFree winID -> do
      let messageQueues = envMsgQueues env
      modifyIORef' messageQueues $ \mq -> IntMap.delete winID mq

    SendResponse response -> do
      let msgQOut = envMsgQOut env
      atomically $ writeTBQueue msgQOut response


    {- InternalServerQuit: perform graceful exit, close and free resorces.
    -}
    ServerQuit -> liftIO $ do
      killThread $ envRecvThread env
      killThread $ envSendThread env

      -- Request that each window closes and frees any resources; wait
      -- for them to finish doing so and exit.
      flip finally waitForChildren $ do
        msgQs <- readIORef $ envMsgQueues env
        forM_ (IntMap.keys msgQs) $ \winID ->
          runRIO env $ forwardMsg winID (GLWindowFree winID)

      shutdown $ envCtx env -- explicitly close ZeroMQ context
      putMVar (envServerExit env) ()


forwardMsg :: WindowID -> ExternalMsg -> RIO Env ()
forwardMsg winID message = do
  env <- ask
  messageQueues <- readIORef $ envMsgQueues env
  case IntMap.lookup winID messageQueues of
    Just msgQ -> atomically $ writeTBQueue msgQ message
    Nothing -> liftIO $
      putStrLn $ concat [ "*** Error: can't pass message to window "
                        , show winID
                        , " because window "
                        , show winID
                        , " doesn't exist."
                        ]
