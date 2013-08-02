{-# LANGUAGE OverloadedStrings #-}
module Gather.Summarize where

import           Data.Text as T
import           Text.HTML.TagSoup as TS

combineTags :: String -> String -> String
combineTags accum val = accum ++ " " ++ val

-- Number of characters to return, parsed tags
summarize :: Int -> [Tag String] -> T.Text
summarize summary_size tags =
    case Prelude.length tags of
        0 -> "..."
        _ -> T.pack $ Prelude.take summary_size $
                Prelude.foldl combineTags " " $
                    Prelude.map fromTagText $ Prelude.filter TS.isTagText relevant_stuff
  where
    relevant_stuff = (sections (~== ("<body>" :: String)) tags) !! 0
