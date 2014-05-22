module Main where

import Control.Monad (forM_)
import Control.Monad.IO.Class (MonadIO)
import Haste
import qualified Haste.Concurrent as C
import Haste.JSON

main :: IO ()
main = C.concurrent $ do
    mvar <- withElems ["gametext", "gamedata"] loadGame
    withElem "bubbles" (addHandlers mvar)

loadGame :: [Elem] -> C.CIO (C.MVar String)
loadGame [textElem, dataElem] = do
    (Right (Arr defList)) <- getInitData dataElem
    let (_, meaning) = getDefAtIndex defList 0 
    updateContent textElem meaning
    C.server (defList, 0, length defList) (handlePop textElem) 

handlePop :: MonadIO m => Elem -> ([JSON], Int, Int) -> String -> m (Maybe ([JSON], Int, Int))
handlePop textElem (defList, wordIndex, numWords) wordHit  
    | currentWord /= wordHit = do
        updateContent textElem "Game over! Refresh to try again." 
        return Nothing
    | numWords <= (wordIndex + 1) = do
        updateContent textElem "You win! Refresh to play again." 
        return Nothing
    | otherwise = do
        updateContent textElem nextMeaning
        return $ Just (defList, wordIndex + 1, numWords)
    where
        (currentWord, _) = getDefAtIndex defList wordIndex
        (_, nextMeaning) = getDefAtIndex defList (wordIndex + 1) 

getDefAtIndex :: [JSON] -> Int -> (String, String)
getDefAtIndex defList index = 
    (getValStr "word", getValStr "meaning") 
    where
        def = defList !! index
        getVal defn key = defn ! toJSString key
        extractStr (Str str) = fromJSStr str 
        getValStr key = extractStr $ getVal def key
   
updateContent :: MonadIO m => Elem -> String -> m ()
updateContent element = setProp element innerHtml 

getInitData :: MonadIO m => Elem -> m (Either String JSON)
getInitData dataElem = do  
    gdata <- getProp dataElem innerHtml 
    updateContent dataElem ""
    return $ decodeJSON (toJSString gdata) 

addHandlers :: MonadIO m => C.MVar String -> Elem -> m ()
addHandlers mvar bubblesElem = do
    bublist <- getChildren bubblesElem
    forM_ bublist (\bubble -> bubble `onEvent` OnClick $ 
        \_ _ -> C.concurrent $ handleClick bubblesElem bubble mvar)

handleClick :: Elem -> Elem -> C.MVar String -> C.CIO () 
handleClick bubbles bubble mvar = do
    word <- getProp bubble innerHtml 
    doPop bubble bubbles
    C.putMVar mvar word

doPop :: MonadIO m => Elem -> Elem -> m ()
doPop = removeChild 

innerHtml :: PropID 
innerHtml = "innerHTML"
