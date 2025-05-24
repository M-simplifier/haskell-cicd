{-# LANGUAGE OverloadedStrings #-}

import Network.HTTP.Types
import Network.Wai
import Network.Wai.Handler.Warp (run)
import System.Environment (lookupEnv)

app :: Application
app _ respond = do
  putStrLn "I've done some IO here"
  respond $
    responseLBS
      status200
      [("Content-Type", "text/plain")]
      "Hello, Web!"

main :: IO ()
main = do
  port <- maybe 8080 read <$> lookupEnv "PORT"
  putStrLn $ "http://localhost:" ++ show port
  run port app