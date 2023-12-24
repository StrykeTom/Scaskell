import Network.Socket
import Control.Exception
import Control.Concurrent
import System.IO
import System.Environment (getArgs)

checkPort :: HostName -> ServiceName -> IO ()
checkPort host port = do
    addrinfos <- getAddrInfo (Just defaultHints { addrFlags = [AI_NUMERICSERV] }) (Just host) (Just port)
    case addrinfos of
      [] -> putStrLn $ "Could not resolve the address of " ++ host
      (addr:_) -> do
        result <- try $ bracket (socket (addrFamily addr) Stream defaultProtocol) close $ \sock -> do
          connect sock $ addrAddress addr
          return ()
        case result of
          Left (SomeException _) -> putStrLn $ "Port " ++ port ++ " is closed."
          Right _ -> putStrLn $ "Port " ++ port ++ " is open."

main :: IO ()
main = do
  putStrLn "Enter host to scan:"
  host <- getLine 
  let ports = [80, 443]
  mapM_ (checkPort host . show) ports
