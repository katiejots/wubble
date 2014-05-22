{-# LANGUAGE OverloadedStrings #-}

module Client.Generate 
    (generateResources) where

import Client.Css (generateCss)
import qualified Data.Text.Lazy.IO as TextIO (writeFile)

generateResources :: FilePath -> IO ()
generateResources basedir = TextIO.writeFile (basedir ++ "/css/style.css") generateCss
