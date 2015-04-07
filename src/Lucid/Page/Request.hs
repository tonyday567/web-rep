import Control.Monad.Cont
import Control.Concurrent
import Network.HTTP

-- ajax :: String -> (String -> IO ()) -> IO ()
ajax :: String -> ContT () IO String
ajax url = ContT $ \f ->
  void $ forkIO (simpleHTTP (getRequest url) >>= getResponseBody >>= f)

f :: ContT () IO String
f = do
  a <- ajax "http://bla.com"
  b <- ajax "http://example.com"
  return $ a ++ b

main :: IO ()
main = do
  runContT f putStrLn
  putStrLn "asd"
  threadDelay 5000000
