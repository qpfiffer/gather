{-# LANGUAGE OverloadedStrings #-}
module Gather.Types where

import           Control.Applicative
import           Control.Monad
import           Data.Aeson as A
import           Data.Text as T

data InsertResult = InsertResult
    { what_happened :: Text
    }

instance ToJSON InsertResult where
    toJSON (InsertResult result) = object ["What happened?" .= result]


data PostedLinkData = PostedLinkData
    { pldUrl :: Text
    , pldPerson :: Text
    }

instance FromJSON PostedLinkData where
    parseJSON (Object v) = PostedLinkData <$>
                           v .: "url" <*>
                           v .: "person"
    parseJSON _ = mzero


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
    parseJSON (Object v) = LinkData <$>
                           v .: "created_at" <*>
                           v .: "title" <*>
                           v .: "url" <*>
                           v .: "person" <*>
                           v .: "summary" <*>
                           v .: "person_color"
    parseJSON _ = mzero

