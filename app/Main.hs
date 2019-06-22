{-# LANGUAGE OverloadedStrings #-}

module Main (main) where


import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception (finally, mask, try, SomeException)
import Control.Monad (forever, forM_)
import Control.Monad.STM (atomically)
import Data.Aeson
import qualified Data.ByteString.Char8 as C
import Data.ByteString.Lazy.Char8 (fromStrict)
import qualified Data.IntMap.Strict as IntMap
import Data.IORef
import System.Exit
import System.IO.Unsafe (unsafePerformIO)
import System.Signal
import System.ZMQ4 hiding (message)

import Graph
import Msg
import Render
import Shader (shaderName, ugenShaderFns)
import Window (WindowID)


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


main :: IO ()
main = withContext $ \ctx -> do
  msgQIn <- newTQueueIO :: IO (TQueue Msg)
  messageQueues <- newIORef (IntMap.empty :: IntMap.IntMap (TQueue Msg))

  -- putStrLn $ show ugenShaderFns

  {- Listen for messages sent over IPC and push them onto the server's message
     queue.
  -}
  recvThread <- forkChildIO $ withSocket ctx Pull $ \sock -> do
    connect sock "ipc://@sc-video_cmd-msgs"
    forever $ do
      rawMsg <- receive sock
      let msg = removeNullChar rawMsg
          mbMsg = decode' (fromStrict msg) :: Maybe Msg

      case mbMsg of
        Nothing -> putStrLn "*** Error: Invalid JSON received."

        -- Do nothing if passed an internal message over IPC
        Just (InternalWindowFree _) ->
          putStrLn "*** Error: Invalid JSON received."
        Just InternalServerQuit ->
          putStrLn "*** Error: Invalid JSON received."

        Just m -> atomically $ writeTQueue msgQIn m

  -- use $! to force shader files to be read from disk at this point if that is
  -- the result of ugenShaderFns, otherwise there may be an undesirable pause
  -- when the first GraphNew is received.
  let glUGenNames = map shaderName $! ugenShaderFns

  installHandler sigABRT $ interruptHandler msgQIn
  installHandler sigINT  $ interruptHandler msgQIn
  installHandler sigTERM $ interruptHandler msgQIn

  {- Handle any messages added to the message queue.
  -}
  forever $ do
    msg <- atomically $ readTQueue msgQIn

    case msg of
      {- GLWindowNew: spawn a new GLFW window running a 'blank' shader program
         and check for messages on its dedicated queue. Performing these
         messages updates the running shader program.

         If a window is already running, ignore the message. Multiple windows
         running in parallel is not to be implemented unless as an extension
         goal of the project.
      -}
      GLWindowNew wID wWidth wHeight -> do
        messageQueues' <- readIORef messageQueues
        -- If the map isn't empty (a window exists) then do nothing. Only a
        -- single output window is currently supported.
        if not $ IntMap.null messageQueues' then return () else do
          msgQ <- newTQueueIO
          writeIORef messageQueues $ IntMap.insert wID msgQ messageQueues'

          _threadID <- forkChildOS $ withSocket ctx Sub $ \s -> do
            subscribe s $ C.pack ""
            connect s "ipc://@sc-video_data-msgs"
            newWindow s msgQ msgQIn wID wWidth wHeight

          return ()


      {- GLWindowFree: close the GLFW window with the specified window ID,
         clean up and free any associated resources.
      -}
      wFreeMsg@(GLWindowFree wID) -> do
        forwardMsg messageQueues wID wFreeMsg
        modifyIORef' messageQueues $ \mq -> IntMap.delete wID mq


     {- GraphNew: compile the GLSL fragment shader from the graph structure,
         spawn a new GLFW window and run the shader program in that window.
      -}
      graphNewMsg@(GraphNew _gID gUnits) -> do
        if not (containsVideoUGen glUGenNames gUnits) then return () else do
          msgQs <- readIORef messageQueues
          forM_ (IntMap.keys msgQs) $ \wID ->
            forwardMsg messageQueues wID graphNewMsg


      {- GraphFree: close the window associated with the specified graph ID,
         clean up and free any resources used.
      -}
      GraphFree gphID -> do
        msgQs <- readIORef messageQueues
        forM_ (IntMap.keys msgQs) $ \wID ->
          forwardMsg messageQueues wID (GraphFree gphID)


      {- GLVideoNew: prepare a video for playback in the window specified by the
         window ID. This just forwards the message to the window by putting the
         message on the window's queue.
      -}
      vNewMsg@(GLVideoNew _vID _vPath _vLoop _vRate wID) -> do
        forwardMsg messageQueues wID vNewMsg


      {- GLVideoNew: load a video into memory in the window specified by the
         window ID and prepare it for playback. This just forwards the message
         to the window by putting the message on the window's queue.
      -}
      vReadMsg@(GLVideoRead _vID _vPath _vLoop _vRate wID) ->
        forwardMsg messageQueues wID vReadMsg


      {- GLVideoFree: free up the resources associated with the video in the
         window specified by the window ID. This will stop video playback but
         doesn't clear the video texture(?) or modify the shader program.
      -}
      vFreeMsg@(GLVideoFree _vID wID) -> do
        forwardMsg messageQueues wID vFreeMsg


      {- GLImageNew: load an image from the supplied file path in the window
         specified by the window ID. This just forwards the message to the
         window by putting the message on the window's queue.
      -}
      iNewMsg@(GLImageNew _vID _vPath wID) -> do
        forwardMsg messageQueues wID iNewMsg


      {- GLImageFree: delete the image of specified ID in that window. Do
         nothing if an image with that ID doesn't exist.
      -}
      iFreeMsg@(GLImageFree _vID wID) -> do
        forwardMsg messageQueues wID iFreeMsg


      {- InternalWindowFree: a message which can only be passed internally from
         an open window to the main server via a message queue. This handles the
         case where the output window is closed via the window manager and that
         window's message queue needs to be deleted from the server.
      -}
      InternalWindowFree wID -> do
        modifyIORef' messageQueues $ \mq -> IntMap.delete wID mq


      {- InternalServerQuit: perform graceful exit, close and free resorces.
      -}
      InternalServerQuit -> do
        killThread recvThread

        -- Request that each window closes and frees any resources; wait
        -- for them to finish doing so and exit.
        flip finally waitForChildren $ do
          msgQs <- readIORef messageQueues
          forM_ (IntMap.keys msgQs) $ \wID ->
            forwardMsg messageQueues wID (GLWindowFree wID)

          shutdown ctx -- explicitly close ZeroMQ context
          exitSuccess
  where
    interruptHandler msgQIn _signal = atomically $ writeTQueue msgQIn $ InternalServerQuit


forwardMsg :: IORef (IntMap.IntMap (TQueue Msg)) -> WindowID -> Msg -> IO ()
forwardMsg msgQsRef wID message = do
  messageQueues <- readIORef msgQsRef
  case IntMap.lookup wID messageQueues of
    Just msgQ -> atomically $ writeTQueue msgQ message
    Nothing ->
      putStrLn $ concat [ "*** Error: can't pass message to window "
                        , show wID
                        , " because window "
                        , show wID
                        , " doesn't exist."
                        ]
