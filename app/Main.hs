{-# LANGUAGE OverloadedStrings, BangPatterns #-}

module Main (main) where


import Options.Applicative
import Data.Semigroup ((<>))

import Server (runServer)


data CmdLineOpts = CmdLineOpts
  { version :: Bool
  }

cmdLineOpts :: Parser CmdLineOpts
cmdLineOpts = CmdLineOpts
           <$> switch
              ( long "version"
             <> help "Print the version and quit"
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
