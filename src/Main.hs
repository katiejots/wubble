import Client.Generate (generateResources)

import Control.Applicative ((<$>))

import Controller.Home (home)
import Controller.Game (game)

import Data.Default (def)
import Data.Maybe (fromMaybe)
import Data.String (fromString)

import Database.PostgreSQL.Simple (ConnectInfo(..), Connection, connect)

import Network.Wai.Handler.Warp (setHost, setPort, defaultSettings)
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import Network.Wai.Middleware.Static (staticPolicy, noDots, addBase, (>->))

import System.Environment (getArgs, lookupEnv)

import Web.Scotty (Options(..), scottyOpts, middleware)

main :: IO ()
main = do
    opts <- commandLineOptions
    static <- staticPath "resources"
    generateResources static
    conn <- dbConn

    scottyOpts opts $ do
        middleware logStdoutDev
        middleware $ staticPolicy (noDots >-> addBase static)
        home conn >> game conn

commandLineOptions :: IO Options
commandLineOptions = do
    (ip:port:_) <- getArgs
    let sets = setPort (read port) . setHost (fromString ip) $ defaultSettings
    return $ def { verbose = 0, settings = sets }

staticPath :: FilePath -> IO FilePath 
staticPath dir = maybe dir (++ dir) <$> lookupEnv "OPENSHIFT_REPO_DIR"

dbConnInfo :: IO ConnectInfo 
dbConnInfo = do host <- getEnvDefault "OPENSHIFT_POSTGRESQL_DB_HOST" "127.0.0.1"
                port <- getEnvDefault "OPENSHIFT_POSTGRESQL_DB_PORT" "5432"
                user <- getEnvDefault "OPENSHIFT_POSTGRESQL_DB_USERNAME" "postgres"
                password <- getEnvDefault "OPENSHIFT_POSTGRESQL_DB_PASSWORD" ""
                dbName <- getEnvDefault "OPENSHIFT_APP_NAME" "postgres"
                return (ConnectInfo host (read port) user password dbName)

getEnvDefault :: String -> String -> IO String
getEnvDefault env defaultVal = fromMaybe defaultVal <$> lookupEnv env

dbConn :: IO Connection
dbConn = dbConnInfo >>= connect
