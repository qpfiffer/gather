{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import           Control.Exception
import           Data.Aeson as A
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as LBS
import           Data.Maybe
import           Data.Text as T
import qualified Data.Text.Encoding as TE
import           Database.KyotoCabinet.Db
import           Control.Applicative
import           Control.Monad
import           Control.Monad.IO.Class (liftIO)
import           Snap.Core
--import           Snap.Util.FileServe
import           Snap.Http.Server
import           Snap.Extras.JSON
import           System.Environment

import Prelude hiding (catch)

person_colors :: [BS.ByteString]
person_colors = ["#FFD923", "#AA2BEF", "#366EEF", "#A68B0B"]
filter_max :: Integer
filter_max = 50

-- Datums
data InsertResult = InsertResult
    { what_happened :: Text
    }

instance ToJSON InsertResult where
    toJSON (InsertResult result) = object ["What happened?" .= result]

data LinkData = LinkData
    { created_at :: Integer
    , title :: Text
    , url :: Text
    , person :: Text
    , summary :: Text
    , person_color :: Text
    }

instance ToJSON LinkData where
    -- Should I feel nausea at one-letter variable names?
    toJSON (LinkData c t u p s pc) =
        object [ "created_at" .= c
               , "title" .= t
               , "url" .= u
               , "person" .= p
               , "summary" .= s
               , "person_color" .= pc
               ]

instance FromJSON LinkData where
    -- Should I feel nausea at one-letter variable names?
    parseJSON (Object v) = LinkData <$>
                           v .: "created_at" <*>
                           v .: "title" <*>
                           v .: "url" <*>
                           v .: "person" <*>
                           v .: "summary" <*>
                           v .: "person_color"
    parseJSON _ = mzero

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
    json_param <- getJSON
    poster <- getParam "person"
    posted_url <- getParam "url"
    -- TODO: Don't use fromJust here.
    result <- liftIO $ kcwithdbopen (BS.unpack db_location) [] [KCOWRITER, KCOCREATE] (insertUrl (fromJust posted_url) (fromJust poster))
    modifyResponse $ setContentType "application/json"
    writeLBS $ A.encode $ result

-- General Utils
runCommand :: [BS.ByteString] -> IO ()
runCommand ["-db", db_location] =
    httpServe defaultConfig $ site db_location
runCommand _ = BS.putStrLn "Must specify a database location."

-- DB Utils
urlInDb :: Text -> KcDb -> IO (Either String Bool)
urlInDb test_url database = do
    kcwithdbcursor database $ \cur -> do
        kccurjumpback cur
        let loop = do
            (_, val) <- kccurget cur False
            case (A.decode (LBS.fromStrict val) :: Maybe LinkData) of
                Nothing -> undefined -- TODO: What happens when Aeson cannot decode?
                Just (LinkData _ _ u _ _ _) ->
                    if u == test_url
                    then return $ Right True
                    else do
                        -- #DEBUG
                        -- BS.putStrLn $ BS.concat [TE.encodeUtf8 u, ", ", TE.encodeUtf8 test_url]
                        kccurstepback cur
                        loop
                        return $ Right False
        loop `catch` \(exn::KcException) ->
            kcdbecode database >>= \error_code ->
            case error_code of
                KCENOREC -> return $ Right False
                err -> return $ Left (show err) -- Something bad happened.

insertUrl :: BS.ByteString -> BS.ByteString -> KcDb -> IO InsertResult
insertUrl posted_url poster db = do
    in_db_already <- urlInDb (TE.decodeUtf8 posted_url) db
    case in_db_already of
        (Right True) -> return $ InsertResult "Someone tried to submit a duplicate URL."
        (Right False) -> return $ InsertResult "TODO"
        (Left msg) -> return $ InsertResult $ T.pack msg
