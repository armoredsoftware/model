{-# LANGUAGE TemplateHaskell, DeriveDataTypeable, DeriveGeneric #-}
{-# OPTIONS_GHC -Wall #-}
import Control.Distributed.Process
import Control.Distributed.Process.Closure

import Control.Monad
import Text.Printf
import GHC.Generics (Generic)
import Data.Binary
import Data.Typeable

import DistribUtils

-- <<Message
data Message = Ping ProcessId
             | Pong ProcessId
             | Fib (ProcessId, Int)
  deriving (Typeable, Generic)          -- <1>

instance Binary Message                 -- <2>
-- >>

-- <<pingServer
pingServer :: Process ()
pingServer = do
  Ping from <- expect                              -- <1>
  say $ printf "ping received from %s" (show from) -- <2>
  mypid <- getSelfPid                              -- <3>
  send from (Pong mypid)                           -- <4>
-- >>
fib:: Int->Int
fib n  
   |n < 2 = 1
   | otherwise = (fib (n-2)) + (fib (n-1)) + 1

-- <<fibServer
fibServer:: Process()
fibServer = do
  mypid <- getSelfPid
  m <- expect
  case m of
    Fib (from, param)-> do say $ printf "Fib received from %s" (show from) 
                           send from (Fib (mypid,(fib param)))
    Ping from -> do say $ printf "ping received from %s" (show from) -- <2>
                    send from (Pong mypid)                           -- <4>
    Pong from-> do say $ "Error Pong Message received"
                   send from (Pong mypid) 

-- >>

-- <<remotable
--remotable ['pingServer]
remotable ['fibServer]
-- >>

-- <<master
master :: [NodeId] -> Process ()                     -- <1>
master peers = do

  ps <- forM peers $ \nid -> do                      -- <2>
          say $ printf "spawning on %s" (show nid)
          spawn nid $(mkStaticClosure 'fibServer)

  mypid <- getSelfPid

  forM_ ps $ \pid -> do                              -- <3>
    say $ printf "pinging %s" (show pid)
    send pid (Ping mypid)
    send pid (Ping mypid)

  waitForPongs ps                                    -- <4>

  forM_ ps $ \pid -> do                              -- <3>
    say $ printf "pinging2 %s" (show pid)
    send pid (Ping mypid)
  waitForPongs ps
  forM_ ps $ \pid -> do                              -- <3>
    say $ printf "sending fib 10 to  %s" (show pid)
    send pid (Fib (mypid,10))

  waitForPongs ps
 
  say "All pongs successfully received"
  terminate

waitForPongs :: [ProcessId] -> Process ()            -- <5>
waitForPongs [] = return ()
waitForPongs ps = do
  m <- expect
  case m of
    Pong p -> waitForPongs (filter (/= p) ps)
    Fib (proc, result)-> do say ("Fib Result is: " ++ (show result) )
                            waitForPongs (filter (/= proc) ps)
    _  -> say "MASTER received ping" >> terminate
-- >>

-- <<main
main :: IO ()
main = distribMain master Main.__remoteTable
-- >>
