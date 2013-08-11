{-# LANGUAGE OverloadedStrings #-}
module Gather.Summarize where

import           Data.Text as T hiding (filter, head)
import           Text.HTML.TagSoup as TS

-- Takes a url, splits it into things separated by /
-- Takes the domain chunk (after http) and splits that on .
-- filters out the stuff I don't care about (www, com)
-- Gets the first part of that. Probably fails for weird domains like
-- "data.google.com"
getDomain :: Text -> Text
getDomain url =
    head $ filter (\x -> x /= "www" && x /= "com") $
        splitOn "." $ splitOn "/"  url !! 2

combineTags :: String -> String -> String
combineTags accum val = accum ++ " " ++ val

-- This is the fall through summarizer in case we don't
-- have a specific one to handle that domain.
general_summarize :: Int -> [Tag String] -> T.Text
general_summarize summary_size tags =
    case Prelude.length tags of
        0 -> "..."
        _ -> T.pack $ Prelude.take summary_size $
                Prelude.foldl combineTags " " $
                    Prelude.map fromTagText $ Prelude.filter TS.isTagText relevant_stuff
  where
    relevant_stuff = (sections (~== ("<body>" :: String)) tags) !! 0

-- URL, Number of characters to return, parsed tags
summarize :: Text -> Int -> [Tag String] -> T.Text
summarize url summary_size tags =
    case getDomain url of
        "youtube" -> "youtube"
        "twitter" -> "twitter"
        _ -> general_summarize summary_size tags
