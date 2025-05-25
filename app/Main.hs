{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

import Control.Monad (when)
import Data.Aeson (ToJSON, encode)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS
import Data.Maybe (fromMaybe)
import Database.PostgreSQL.Simple
  ( Connection,
    FromRow,
    Only (Only),
    connectPostgreSQL,
    execute,
    execute_,
    query_,
  )
import Database.PostgreSQL.Simple.FromRow (FromRow (fromRow), field)
import GHC.Generics (Generic)
import Network.HTTP.Types (status200)
import Network.Wai (Application, responseLBS)
import Network.Wai.Handler.Warp (run)
import System.Environment (lookupEnv)

-- DBのbooksテーブルの情報を表現する型
data Book = Book
  { bookId :: Int,
    title :: String,
    price :: Int
  }
  deriving (Show, Generic)

instance FromRow Book where
  fromRow = Book <$> field <*> field <*> field

instance ToJSON Book

-- テーブルの初期化と、初期状態ならデータ投入する処理
initDBAndData :: Connection -> IO ()
initDBAndData conn = do
  _ <- execute_ conn "CREATE TABLE IF NOT EXISTS books (id SERIAL PRIMARY KEY, title TEXT, price INT)"
  [Only count] <- query_ conn "SELECT COUNT(*) FROM books" :: IO [Only Int]
  when (count == 0) $ do
    _ <- execute conn "INSERT INTO books (title, price) VALUES (?,?)" ("The Catcher in the Rye" :: ByteString, 10 :: Int)
    _ <- execute conn "INSERT INTO books (title, price) VALUES (?,?)" ("To Kill a Mockingbird" :: ByteString, 15 :: Int)
    _ <- execute conn "INSERT INTO books (title, price) VALUES (?,?)" ("1984" :: ByteString, 20 :: Int)
    return ()

-- booksテーブルの全件取得
queryBooks :: Connection -> IO [Book]
queryBooks conn = query_ conn "SELECT id, title, price FROM books"

-- Waiアプリケーション定義： GET "/" にアクセスすると、booksのリストをJSONで返す
app :: Connection -> Application
app conn _req respond = do
  books <- queryBooks conn
  respond $ responseLBS status200 [("Content-Type", "application/json")] (encode books)

main :: IO ()
main = do
  mPortStr <- lookupEnv "PORT"
  mDbPath <- lookupEnv "DB_PATH"
  case (mPortStr, mDbPath) of
    (Nothing, _) -> putStrLn "PORT environment variable is not set. Exiting."
    (_, Nothing) -> putStrLn "DB_PATH environment variable is not set. Exiting."
    (Just portStr, Just dbPath) -> do
      let port = read portStr
      conn <- connectPostgreSQL (BS.pack dbPath)
      initDBAndData conn
      putStrLn $ "Running on port " ++ show port
      run port (app conn)