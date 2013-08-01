{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Gather.DB
import           Gather.Types

import           Data.Aeson as A
import qualified Data.ByteString.Char8 as BS
import qualified Data.Text.Encoding as TE
import           Database.KyotoCabinet.Db
import           Control.Applicative
import           Control.Monad.IO.Class (liftIO)
import           Snap.Core
import           Snap.Http.Server
import           Snap.Extras.JSON
import           System.Environment


runCommand :: [BS.ByteString] -> IO ()
runCommand ["-db", db_location] =
    httpServe defaultConfig $ site db_location
runCommand _ = BS.putStrLn "Must specify a database location."

main :: IO ()
main = do
    args <- getArgs
    runCommand $ fmap BS.pack args

site :: BS.ByteString -> Snap ()
site db_location =
    ifTop (method POST $ submitHandler db_location) <|>
    writeBS "Nothing to see here. "

submitHandler :: BS.ByteString -> Snap ()
submitHandler db_location = do
    posted_link_data <- getJSON :: Snap (Either String PostedLinkData)
    case posted_link_data of
        Right pld -> do
            -- #DEBUG
            -- liftIO $ BS.putStrLn $ BS.concat [normalUrl pld, ", ", normalPerson pld]
            result <- liftIO $ db_func $ insertUrl (normalUrl pld) (normalPerson pld)
            modifyResponse $ setContentType "application/json"
            writeLBS $ A.encode $ result
        Left _ -> do
            modifyResponse $ setContentType "application/json"
            writeLBS $ A.encode $ InsertResult "YOU'RE DUMB"
  where
    normalUrl a = TE.encodeUtf8 $ pldUrl a
    normalPerson a = TE.encodeUtf8 $ pldPerson a
    db_func insert_func = kcwithdbopen (BS.unpack db_location) [] [KCOWRITER, KCOCREATE] insert_func
