{-# LANGUAGE OverloadedStrings #-}
module Gather.Summarize where

import           Data.Text as T
import           Text.HTML.TagSoup

summarize :: Integer -> [Tag String] -> T.Text
summarize summary_size tags = "SUMMARY"
