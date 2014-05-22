{-# LANGUAGE OverloadedStrings #-}

module View.Game (renderGame) where

import Control.Monad (forM_)
import Data.Aeson (encode)
import Data.Text.Lazy as T
import Model.Definition (Definition(..))
import Prelude hiding (id, div)
import Text.Blaze.Html5.Attributes (class_, src, id, type_)
import Text.Blaze.Html5 (body, div, p, script, toHtml, (!), unsafeLazyByteString)
import View.Header (headerHtml)
import View.Util (blaze)
import Web.Scotty (ActionM)

renderGame :: T.Text -> [Definition] -> [Definition] -> ActionM () 
renderGame _ defs shuffled = blaze $ do
    headerHtml
    body ! class_ "gameview" $ do 
        div ! class_ "maintext" $ "Wubble Time!"
        p ! class_ "instructions" $ "Click the word bubble that matches the definition."
        div ! id "gametext" $ ""
        div ! id "bubbles" $ 
            forM_ shuffled (div ! class_ "bubble" . toHtml $ word) 
        script ! id "gamedata" ! type_ "application/json" $ 
            unsafeLazyByteString $ encode defs
        script ! src "js/wubble.js" $ ""
        
