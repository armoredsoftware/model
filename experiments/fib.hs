-- ghc fib.hs
import Control.Concurrent (threadDelay)
import Control.Monad (forever)
import Control.Distributed.Process
import Control.Distributed.Process.Node
import Network.Transport.TCP (createTransport, defaultTCPParameters)

------Spawned process methods : blocks until it receives a message that is of the following type

replyBack :: (ProcessId, String) -> Process ()
replyBack (sender, msg) = send sender msg

runFib :: (ProcessId, Int) -> Process()
runFib (sender, param) = send sender ("Fib Result: " ++(show (fib param))) 

logMessage :: String -> Process ()
logMessage msg = say $ "handling " ++ msg

logUnknown :: Process()
logUnknown = say "Error: Unknown Message"

fib:: Int -> Int
fib n | n < 2 = 1
fib n = (fib (n-2)) + (fib (n-1)) + 1


-------

main :: IO ()
main = do
  Right t <- createTransport "127.0.0.1" "10501" defaultTCPParameters
  node <- newLocalNode t initRemoteTable

  -- Spawn a new process on a local node 
  forkProcess node $ do
    -- Spawn worker inside one more process on the local node 
    echoPid <- spawnLocal $ forever $ do
      -- Test the matches in order against each message in the queue
      receiveWait [match logMessage, match replyBack, match runFib, matchUnknown logUnknown]


    say "send some messages!"

    send echoPid "hello" -- receiver will "handle message"

    self <- getSelfPid
    send echoPid (self, 15::Int) -- receiver will compute fib and return result

    m <- expectTimeout 1000000   -- obtain result of fib 
    case m of
      -- Die immediately - throws a ProcessExitException with the given reason.
      Nothing  -> die "nothing came back!"
      (Just s) -> say s

    send echoPid (15::Int) -- send something that wasn't expected

    send echoPid (self, "hello")-- send a string 
    m2 <- expectTimeout 1000000 -- expect a response
    case m2 of
      -- Die immediately - throws a ProcessExitException with the given reason.
      Nothing  -> die "nothing came back!"
      (Just s) -> say $ "Received "++ s
    return ()

  -- A 1 second wait. Otherwise the main thread can terminate before
  -- our messages reach the logging process or get flushed to stdio
  liftIO $ threadDelay (1*1000000)
  return ()
