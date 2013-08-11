{-# LANGUAGE OverloadedStrings #-}
module Gather.Utils where

--import           Data.ByteString as BS
import           Data.Char as C
import           Data.Text as T
import qualified Network.HTTP as NH
import           System.Time
import           Text.HTML.TagSoup

openURL :: String -> IO String
openURL x = do
    response <- NH.simpleHTTP (NH.getRequest x)
    response_body <- NH.getResponseBody response
    return $ Prelude.map C.toLower response_body

getTitle :: [Tag String] -> T.Text
getTitle tags =
    T.pack title_text
  where
    the_sections = sections (~== ("<title>" :: String)) tags
    TagText title_text =
        case Prelude.length the_sections of
            0 -> TagText ("" :: String)
            _ -> the_sections !! 0 !! 1

getTimeStamp :: IO Integer
getTimeStamp = getClockTime >>= \(TOD unix _) -> return unix

