module Main (main) where


import Prelude (putStrLn)
import RIO

import Options.Applicative
import Data.Semigroup ((<>))

import Server (runServer)
import Types (CmdLineOpts(..))


cmdLineOpts :: Parser CmdLineOpts
cmdLineOpts = CmdLineOpts
           <$> switch
              ( long "version"
             <> help "Print the version and quit"
              )
           <*> strOption
              ( long "cmd-msg-sock"
             <> value "ipc://@scvid_cmd-msgs"
             <> showDefault
             <> help "The (ZeroMQ) endpoint where command messages are received"
             <> metavar "ENDPOINT"
              )
           <*> strOption
              ( long "data-msg-sock"
             <> value "ipc://@scvid_data-msgs"
             <> showDefault
             <> help "The (ZeroMQ) endpoint where data messages are received"
             <> metavar "ENDPOINT"
              )
           <*> strOption
              ( long "response-sock"
             <> value "ipc://@scvid_responses"
             <> showDefault
             <> help "The (ZeroMQ) endpoint where responses are sent to scsynth"
             <> metavar "ENDPOINT"
              )


main :: IO ()
main = do
  opts <- execParser getOpts
  if version opts then
    putStrLn "scvid alpha"
  else
    runServer opts
  where
    getOpts = info (cmdLineOpts <**> helper)
      ( fullDesc
     <> header "scvid - Video server for SuperCollider-AV"
      )
