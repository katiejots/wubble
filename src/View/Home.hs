{-# LANGUAGE OverloadedStrings #-}

module View.Home (renderHome) where

import Control.Monad (forM_)
import qualified Data.Text.Lazy as T (Text)
import Text.Blaze.Html5.Attributes (action, method, name, value, type_, class_)
import Text.Blaze.Html5 (body, h1, form, p, input, select, option, toHtml, toValue, (!))
import View.Header (headerHtml)
import View.Util (blaze)
import Web.Scotty (ActionM)

renderHome :: [T.Text] -> ActionM () 
renderHome wordsets = blaze $ do
    headerHtml
    body ! class_ "homeview" $ do
        h1 "Wubble"
        form ! action "/game" ! method "get" $ do
            p "Wordset: "
            select ! name "wordset" $ do
                forM_ wordsets (\wordset -> option ! value (toValue wordset) $ toHtml wordset)
            p "Number of bubbles: "
            input ! name "bubbles" ! type_ "text"
            p (input ! type_ "submit")
