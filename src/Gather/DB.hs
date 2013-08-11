{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Gather.DB where

import           Gather.Summarize
import           Gather.Types
import           Gather.Utils

import           Control.Exception
import           Data.Aeson as A
import           Data.ByteString.Char8 as BS
import           Data.ByteString.Lazy as LBS
import           Data.Text as T
import qualified Data.Text.Encoding as TE
import           Database.KyotoCabinet.Db
import           System.Random
import           Text.HTML.TagSoup

import Prelude hiding (catch)

person_colors :: [Text]
person_colors = ["#FFD923", "#AA2BEF", "#366EEF", "#A68B0B"]

filter_max :: Integer
filter_max = 50

urlInDb :: Text -> KcDb -> IO (Either String Bool)
urlInDb test_url database = do
    kcwithdbcursor database $ \cur -> do
        kccurjumpback cur
        let loop x = do
            (_, val) <- kccurget cur False
            case (A.decode (LBS.fromStrict val) :: Maybe LinkData) of
                Nothing -> return $ Right False
                Just (LinkData _ _ u _ _ _) ->
                    if u == test_url
                    then return $ Right True
                    else
                        -- #DEBUG
                        -- BS.putStrLn $ BS.concat [TE.encodeUtf8 u, ", ", TE.encodeUtf8 test_url]
                        kccurstepback cur >>= \_ ->
                            if x > 0
                            then loop (x-1)
                            else return $ Right False
        loop filter_max `catch` \(_::KcException) ->
            kcdbecode database >>= \error_code ->
            case error_code of
                KCENOREC -> return $ Right False
                err -> return $ Left (show err) -- Something bad happened.

getRandomPersonColor :: IO Text
getRandomPersonColor = random_index >>= \rabbit ->
    return $ person_colors !! rabbit
  where
    random_index = getStdRandom $ randomR (0, (Prelude.length person_colors) - 1)

insertUrl :: BS.ByteString -> BS.ByteString -> KcDb -> IO InsertResult
insertUrl posted_url poster db = do
    let text_url = TE.decodeUtf8 posted_url
    in_db_already <- urlInDb text_url db
    case in_db_already of
        (Right True) -> return $ InsertResult "Someone tried to submit a duplicate URL."
        (Right False) -> do
            time_submitted <- getTimeStamp
            rand_color <- getRandomPersonColor
            if (BS.take 5 posted_url) /= "https"
                then do
                    tags <- fmap parseTags $ openURL $ BS.unpack posted_url
                    let page_title = getTitle tags
                        summation = summarize text_url 300 tags
                    kcdbset db (BS.pack $ show time_submitted) $ LBS.toStrict $ A.encode $
                        LinkData time_submitted page_title text_url (TE.decodeUtf8 poster) summation rand_color
                    return $ InsertResult "MUDADA"
                else do
                    -- HTTP-4000 can't do https. :(
                    kcdbset db (BS.pack $ show time_submitted) $ LBS.toStrict $ A.encode $
                        LinkData time_submitted (TE.decodeUtf8 posted_url) text_url (TE.decodeUtf8 poster) "..." rand_color
                    return $ InsertResult "MUDADA"
        (Left msg) -> return $ InsertResult $ T.pack msg
