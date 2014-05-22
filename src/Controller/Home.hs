{-# LANGUAGE OverloadedStrings #-}

module Controller.Home
    (home) where

import Control.Monad.IO.Class (liftIO)
import Database.PostgreSQL.Simple (Connection)
import Model.Wordset (wordsetNames)
import View.Home (renderHome)
import Web.Scotty (ScottyM, get)

home :: Connection -> ScottyM ()
home conn = do 
    wordsets <- liftIO $ wordsetNames conn
    get "/" $ renderHome wordsets
