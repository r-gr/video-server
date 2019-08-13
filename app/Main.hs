{-# LANGUAGE OverloadedStrings, BangPatterns #-}

module Main (main) where


import Options.Applicative
import Data.Semigroup ((<>))

import Server (runServer)


data CmdLineOpts = CmdLineOpts
  { version :: Bool
  , cmdMsgSock :: String
  , dataMsgSock :: String
  }

cmdLineOpts :: Parser CmdLineOpts
cmdLineOpts = CmdLineOpts
           <$> switch
              ( long "version"
             <> help "Print the version and quit"
              )
           <*> strOption
              ( long "cmd-msg-sock"
             <> value "ipc://@sc-video_cmd-msgs"
             <> showDefault
             <> help "The (ZeroMQ) endpoint where command messages are received"
             <> metavar "ENDPOINT"
              )
           <*> strOption
              ( long "data-msg-sock"
             <> value "ipc://@sc-video_data-msgs"
             <> showDefault
             <> help "The (ZeroMQ) endpoint where data messages are received"
             <> metavar "ENDPOINT"
              )


main :: IO ()
main = do
  opts <- execParser getOpts
  if version opts then
    putStrLn "scvid alpha"
  else
    runServer
  where
    getOpts = info (cmdLineOpts <**> helper)
      ( fullDesc
     <> header "scvid - Video server for SuperCollider-AV"
      )
