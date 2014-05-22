{-# LANGUAGE OverloadedStrings #-}

module Controller.Game
    (game) where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.State.Lazy (evalState)
import Data.Char (isDigit)
import Data.Random (StdRandom(..), runRVar, runRVar)
import Data.Random.Extras (sample, shuffle)
import Data.Text.Lazy as T
import Database.PostgreSQL.Simple (Connection)
import Model.Definition (Definition(..))
import Model.Wordset (Wordset(..), wordsetNames, wordsetByName)
import Network.HTTP.Types.Status (Status, badRequest400, internalServerError500)
import System.Random (StdGen, newStdGen)
import View.Error (renderError)
import View.Game (renderGame)
import Web.Scotty (ScottyM, ActionM, get, params, status)

game :: Connection -> ScottyM ()
game conn = get "/game" $ do
    parameters <- params
    validWordsets <- liftIO $ wordsetNames conn
    maybe missingParamError (validate conn validWordsets) $ mapM (`lookup` parameters) ["wordset", "bubbles"]
    where missingParamError = badRequestErr "Missing parameter"

validate :: Connection -> [T.Text] -> [T.Text] -> ActionM ()    
validate conn validWordsets (wordset:bubbles:_) 
    | T.null wordset = badRequestErr "Invalid wordset parameter"
    | T.null bubbles = badRequestErr "Invalid bubbles parameter"
    | wordset `notElem` validWordsets = badRequestErr "Wordset does not exist" 
    | isNotPositiveInt bubbles = badRequestErr "Bubbles must be a positive number"
    | otherwise = loadWordset conn wordset (read (T.unpack bubbles))
validate _ _ _ = internalErr "Internal error"

isNotPositiveInt :: T.Text -> Bool
isNotPositiveInt txt = not (Prelude.all isDigit t) || read t < (1 :: Int)
                       where t = T.unpack txt 

loadWordset :: Connection -> T.Text -> Int -> ActionM ()
loadWordset conn wsName bubbles = do
    wordset <- liftIO $ wordsetByName conn wsName 
    case wordset of Nothing -> internalErr "Unable to load requested wordset"
                    Just ws@(Wordset {size = s}) -> checkBubbleNum s ws
    where checkBubbleNum s ws = if s < bubbles then badRequestErr "Not enough words in set for bubble choice" 
                                               else liftIO $ chooseDefs ws bubbles >>= \defs -> 
                                                            shuffled defs >>= \sdefs -> 
                                                            renderGame (name ws) defs sdefs

chooseDefs :: Wordset -> Int -> IO [Definition] 
chooseDefs ws num = newStdGen >>= return . generateDefList (definitions ws) num

shuffled :: [Definition] -> IO [Definition]
shuffled ls = runRVar (shuffle ls) StdRandom

generateDefList :: [Definition] -> Int -> StdGen -> [Definition]
generateDefList input num = evalState (runRVar (sample num input) StdRandom) 

returnError :: Status -> String -> ActionM ()
returnError stat err = renderError err >> status stat 

badRequestErr :: String -> ActionM ()
badRequestErr = returnError badRequest400 

internalErr :: String -> ActionM ()
internalErr = returnError internalServerError500 
