{-# LANGUAGE DeriveDataTypeable #-}

import Data.Maybe (fromJust)
import Network
import System.Environment
import System.IO
import System.Timeout
import Text.Printf

import System.Console.CmdArgs hiding (name)
import qualified System.Console.CmdArgs as CA

import GameInfo
import Protocol



data Args = Args { address :: String,
                   port    :: Int }
          deriving (Show, Data, Typeable)

main = do
  args <- cmdArgs $ Args { address = def &= argPos 0,
                           port    = def &= argPos 1 }
  
  doRequest (address args) (fromIntegral $ port args)


doRequest :: HostName -> PortNumber -> IO ()
doRequest address port = do
  mh <- timeout (2500 * 1000) $ connectTo address $ PortNumber port
  
  case mh of
    Nothing -> printf "Connection timed out\n"
    Just h -> do
      game <- getGame h
      hClose h
      output game


output :: Game -> IO ()
output game = do
  printf $ name game
  printf (if state game == Waiting 
          then unlines [" Waiting for players",
                        printf " %s Age" $ show $ fromJust $ era game]
          else unlines [printf " Turn %d" $ turn game,
                        printf " Time to host %s" $ formatTime $ timeToHost game])
  printf " Nation                  Status Submitted Connected\n"
  printf (unlines $ map showNation $ nations game)

  where
    formatTime :: Int -> String
    formatTime ms =
      let (hours, ms') = ms `divMod` (60 * 60 * 1000)
          (mins, ms'') = ms' `divMod` (60 * 1000)
          secs         = ms'' `div` 1000
      in printf "%02d:%02d:%02d" hours mins secs

    showNation nation =
      printf " %-21s %8s %9s %9s"
      (nationName $ nationId nation)
      (case player nation of
          Empty  -> "open"
          Human  -> "player"
          AI     -> "AI"
          Closed -> "closed"
          DefeatedThisTurn -> "defeated"
          DefeatedEarlier  -> "defeated")
      (showColumn submitted nation)
      (showColumn connected nation)
    showColumn pred nation
      | player nation == AI               = "-"
      | player nation == DefeatedThisTurn = "-"
      | player nation == DefeatedEarlier  = "-"
      | pred nation = "yes"
      | otherwise   = ""
