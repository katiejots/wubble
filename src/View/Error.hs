{-# LANGUAGE OverloadedStrings #-}

module View.Error (renderError) where

import Text.Blaze.Html5.Attributes (class_)
import Text.Blaze.Html5 (body, h1, h2, toHtml, (!))
import View.Header (headerHtml)
import View.Util (blaze)
import Web.Scotty (ActionM)

renderError :: String -> ActionM ()  
renderError err = blaze $ do
    headerHtml
    body ! class_ "errorview" $ do
        h1 "Things Have Gone aWubbley!"
        h2 ("Error: " >> toHtml err)
