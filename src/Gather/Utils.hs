{-# LANGUAGE OverloadedStrings #-}
module Gather.Utils where

--import           Data.ByteString as BS
import           Data.Text as T
import qualified Network.HTTP as NH
import           System.Time
import           Text.HTML.TagSoup

openURL :: String -> IO String
openURL x = NH.getResponseBody =<< NH.simpleHTTP (NH.getRequest x)

getTitle :: [Tag String] -> T.Text
getTitle tags = T.pack title_of_page
  where
    -- (!! 0) get the first tag that matches.
    -- ((!! 0) !! 1) get the actual text of the tag.
    TagText title_of_page = (sections (~== ("<title>" :: String)) tags) !! 0 !! 1

getTimeStamp :: IO Integer
getTimeStamp = getClockTime >>= \(TOD unix _) -> return unix

