{-# LANGUAGE OverloadedStrings #-}

module Model.Definition (
              Definition(..)
            , allDefinitions 
            ) where

import Control.Applicative ((<$>), (<*>))
import Data.Aeson (ToJSON, toJSON, object, (.=))
import Data.Text.Lazy (Text)
import Database.PostgreSQL.Simple (Connection, query_)
import Database.PostgreSQL.Simple.FromRow (FromRow, fromRow, field)

data Definition = Definition { word :: Text
                             , meaning :: Text
                             } deriving (Eq, Show)

instance FromRow Definition where
    fromRow = Definition <$> field <*> field 

instance ToJSON Definition where
    toJSON (Definition w m) = object ["word" .= w
                                    , "meaning" .= m]

allDefinitions :: Connection -> IO [Definition]
allDefinitions conn = query_ conn "SELECT word, meaning FROM definition"

