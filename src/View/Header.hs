{-# LANGUAGE OverloadedStrings #-}

module View.Header (headerHtml) where

import Text.Blaze.Html5.Attributes (href, rel)
import Text.Blaze.Html5 (Html, head, title, link, (!)) 
import qualified Text.Blaze.Html5 as H (head) 

headerHtml :: Html 
headerHtml = 
    H.head $ do
        title "Wubble"
        link ! rel "stylesheet" ! href "css/style.css"
