module Data.Configuration where

data Configuration 
    = Configuration { fileName :: FilePath, folder :: FilePath }