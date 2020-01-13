{-# LANGUAGE CPP               #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
module Settings.Chat where

import           ClassyPrelude.Yesod
import           Language.Haskell.TH.Syntax  (Exp, Name, Q)
import           Yesod.Default.Util (WidgetFileSettings,
                                     widgetFileNoReload,
                                     widgetFileReload)

-- This module enables the reading out of the environment, which we need because we are getting the
-- configuration for our web app entirely from the env variables...
import           System.Environment

-- DEFINING THE SETTINGS

data AppSettings = AppSettings
    { appDatabaseHost               :: String
    , appDatabasePort               :: String
    , appDatabaseName               :: String
    , appDatabaseUser               :: String
    , appDatabasePassword           :: String
    , appDatabasePoolSize           :: Int
    , appPort                       :: Int
    }

-- UTILITY METHODS

makeSettings :: IO AppSettings
makeSettings = do
    appDatabaseHost <- getEnv "POSTGRES_HOST"
    appDatabasePort <- getEnv "POSTGRES_PORT"
    appDatabaseName <- getEnv "POSTGRES_DB"
    appDatabaseUser <- getEnv "POSTGRES_USER"
    appDatabasePassword <- getEnv "POSTGRES_PASSWORD"
    poolsize <- getEnv "POSTGRES_POOLSIZE"
    port <- getEnv "APPLICATION_PORT"
    let appDatabasePoolSize = read poolsize
        appDatabasePort = read port
    return $ AppSettings {..}

appDatabaseString :: AppSettings -> String
appDatabaseString settings = "postgres://" ++
        (appDatabaseUser settings)      ++ ":" ++
        (appDatabasePassword settings)  ++ "@" ++
        (appDatabaseHost settings)      ++ ":" ++
        (appDatabasePort settings)      ++ "/" ++
        (appDatabaseName settings)

widgetFileSettings :: WidgetFileSettings
widgetFileSettings = def

widgetFile :: String -> Q Exp
widgetFile = widgetFileReload widgetFileSettings