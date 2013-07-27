{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Data.ByteString.Char8 as BS
import           Database.KyotoCabinet.Db
import           Control.Applicative
import           Snap.Core
--import           Snap.Util.FileServe
import           Snap.Http.Server
import           System.Environment


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
submitHandler db_location =
    writeBS "Woo!"

-- General Utils
runCommand :: [BS.ByteString] -> IO ()
runCommand ["-db", db_location] =
    httpServe defaultConfig $ site db_location
runCommand _ = BS.putStrLn "Must specify a database location."

-- DB Utils
urlInDb :: BS.ByteString -> KcDb -> Bool
urlInDb _ _ = False
