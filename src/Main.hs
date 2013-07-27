{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Data.Aeson as A
import           Data.ByteString.Char8 as BS
import           Database.KyotoCabinet.Db
import           Control.Applicative
import           Control.Monad.IO.Class (liftIO)
import           Snap.Core
--import           Snap.Util.FileServe
import           Snap.Http.Server
import           System.Environment
import           Data.Text

-- Datums
data InsertResult = InsertResult
    { what_happened :: Text
    }

instance ToJSON InsertResult where
    toJSON (InsertResult result) = object ["What happened?" .= result]


-- Snap stuff
main :: IO ()
main = do
    args <- getArgs
    runCommand $ fmap BS.pack args

site :: BS.ByteString -> Snap ()
site db_location =
    ifTop (method POST $ submitHandler db_location) <|>
    writeBS ("Nothing to see here. " `BS.append` db_location)

submitHandler :: BS.ByteString -> Snap ()
submitHandler db_location = do
    result <- liftIO $ kcwithdbopen (BS.unpack db_location) [] [KCOWRITER, KCOCREATE] (insertUrl "ASDF")
    modifyResponse $ setContentType "application/json"
    writeLBS $ A.encode $ result

-- General Utils
runCommand :: [BS.ByteString] -> IO ()
runCommand ["-db", db_location] =
    httpServe defaultConfig $ site db_location
runCommand _ = BS.putStrLn "Must specify a database location."

-- DB Utils
urlInDb :: BS.ByteString -> KcDb -> Bool
urlInDb url database = True

insertUrl :: BS.ByteString -> KcDb -> IO InsertResult
insertUrl url db = do
    case urlInDb url db of
        True -> return $ InsertResult "Someone tried to submit a duplicate URL."
        False -> do
            --kcwithdbcursor db $ \cur -> do
            --    kccurjump
            return $ InsertResult "MUDADA"
