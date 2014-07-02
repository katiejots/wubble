{-# LANGUAGE OverloadedStrings #-}

module Model.Wordset (
              Wordset(..)
            , wordsetNames 
            , wordsetByName
            ) where

import Control.Monad.IO.Class (liftIO, MonadIO)
import Control.Monad.Trans.Maybe (MaybeT(..))
import qualified Data.Text.Lazy as T (Text)
import Database.PostgreSQL.Simple (Connection, Only(..), query_, query)
import Database.PostgreSQL.Simple.FromField ()
import Model.Definition (Definition(..))

data Wordset = Wordset { name :: T.Text
                       , topic :: T.Text
                       , size :: Int 
                       , definitions :: [Definition] 
                       } deriving (Eq, Show)

wordsetNames :: Connection -> IO [T.Text] 
wordsetNames conn = do wordsets <- query_ conn "SELECT name FROM wordset ORDER BY name" :: IO [Only T.Text] 
                       return $ map (\(Only ws) -> ws) wordsets 

wordsetByName :: Connection -> T.Text -> MaybeT IO Wordset
wordsetByName conn wsName = do
    wordsets <- liftIO (query conn "SELECT id, topic FROM wordset WHERE name = ?" [wsName] :: IO [(Integer, T.Text)])
    case wordsets of (wsId, wsTopic):_ -> constructWordset conn wsName wsTopic wsId
                     _ -> MaybeT $ return Nothing 

constructWordset :: MonadIO m => Connection -> T.Text -> T.Text -> Integer -> m Wordset
constructWordset conn wsName wsTopic wsId = do
    defs <- liftIO (query conn "SELECT word, meaning FROM definition \
                               \ JOIN wordset_definition ON wordset_definition.definition_id = definition.id \
                               \ WHERE wordset_definition.wordset_id = ?" [wsId] :: IO [Definition])
    return $ Wordset wsName wsTopic (length defs) defs 
 
