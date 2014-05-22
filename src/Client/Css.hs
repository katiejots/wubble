{-# LANGUAGE OverloadedStrings #-}

module Client.Css 
    (generateCss) where

import Clay
import Clay.Background()
import qualified Clay.Display as Display
import Clay.FontFace()
import Data.Text.Lazy as T

generateCss :: T.Text 
generateCss = render $ do
    bubbleFont
    bubble
    bubbles
    gamehead
    gametext
    instruct    

bubbleFont :: Css
bubbleFont = fontFace $ do
    fontFamily ["BubblePixel"] []
    fontFaceSrc [FontFaceSrcUrl "../fonts/bubble_pixel-7.ttf" (Just TrueType)]

bubble :: Css
bubble = ".bubble" ? do
    background (url "../images/bubble.png")
    backgroundRepeat noRepeat
    backgroundSize cover
    width (px 100) 
    height (px 100)
    textAlign (alignSide sideCenter)
    display tableCell
    "vertical-align" -: "middle"

bubbles :: Css
bubbles = "#bubbles" ? do
    display Display.table
    "border-collapse" -: "separate"
    "border-spacing" -: "10px" 
    marginLeft auto
    marginRight auto
    paddingTop (px 20)

gamehead :: Css
gamehead = ".maintext" ? do
    fontSize (px 70)
    fontFamily ["BubblePixel"] [sansSerif]
    color $ rgb 0 76 211 
    paddingTop (px 50) 
    textAlign (alignSide sideCenter)

gametext :: Css
gametext = "#gametext" ? do
    fontSize (px 26)
    textAlign (alignSide sideCenter)
    fontWeight bold
    paddingTop (px 40)
    paddingBottom (px 40)
    background $ rgb 139 187 205 

instruct :: Css
instruct = ".instructions" ? do
    textAlign (alignSide sideCenter)
    fontStyle italic 
    fontSize (px 16)
