import qualified Network.WebSockets as WS
import qualified Data.ByteString.Lazy.UTF8 as U
import Control.Monad
main :: IO ()
main = do
  WS.runServer "0.0.0.0" 9160 $ application 

application :: WS.ServerApp
application pending = do
  conn <- WS.acceptRequest pending
  WS.forkPingThread conn 30
  msg <- WS.receiveData conn
  putStrLn . U.toString . WS.fromLazyByteString $ msg
  talk conn

talk :: WS.Connection -> IO ()
talk conn = forever $ do
  msg <- WS.receiveData conn
  putStrLn . U.toString . WS.fromLazyByteString $ msg
