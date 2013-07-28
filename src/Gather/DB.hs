{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Gather.DB where

import           Gather.Types
import           Gather.Utils

import           Control.Exception
import           Data.Aeson as A
import           Data.ByteString.Char8 as BS
import           Data.ByteString.Lazy as LBS
import           Data.Text as T
import qualified Data.Text.Encoding as TE
import           Database.KyotoCabinet.Db
import           Text.HTML.TagSoup

import Prelude hiding (catch)

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
                        -- return $ Right False
        loop `catch` \(_::KcException) ->
            kcdbecode database >>= \error_code ->
            case error_code of
                KCENOREC -> return $ Right False
                err -> return $ Left (show err) -- Something bad happened.

insertUrl :: BS.ByteString -> BS.ByteString -> KcDb -> IO InsertResult
insertUrl posted_url poster db = do
    in_db_already <- urlInDb (TE.decodeUtf8 posted_url) db
    case in_db_already of
        (Right True) -> return $ InsertResult "Someone tried to submit a duplicate URL."
        (Right False) -> do
            time_submitted <- getTimeStamp
            tags <- fmap parseTags $ openURL $ BS.unpack posted_url
            return $ InsertResult $ getTitle tags
            --return $ InsertResult "MUDADA"
        (Left msg) -> return $ InsertResult $ T.pack msg
