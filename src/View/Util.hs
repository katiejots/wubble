module View.Util
    (blaze) where

import Text.Blaze.Html.Renderer.Text (renderHtml)
import Text.Blaze.Html5 (Html)
import Web.Scotty (ActionM, html)

blaze :: Html -> ActionM ()
blaze = html . renderHtml
