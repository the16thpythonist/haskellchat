{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Application
    ( makeFoundation
    , chatMain
    ) where

import Control.Monad.Logger                 (liftLoc, runLoggingT)
import Database.Persist.Postgresql          (createPostgresqlPool, pgConnStr,
                                             pgPoolSize, runSqlPool)
import Import
import Language.Haskell.TH.Syntax           (qLocation)
import Network.HTTP.Client.TLS              (getGlobalManager)
import Network.Wai (Middleware)
import Network.Wai.Handler.Warp             (Settings, defaultSettings,
                                             defaultShouldDisplayException,
                                             runSettings, setHost,
                                             setOnException, setPort, getPort)
import Network.Wai.Middleware.RequestLogger (Destination (Logger),
                                             IPAddrSource (..),
                                             OutputFormat (..), destination,
                                             mkRequestLogger, outputFormat)
import System.Log.FastLogger                (defaultBufSize, newStdoutLoggerSet,
                                             toLogStr)
-- Importing all the relevant handlers
import Handler.Chat
import Handler.Post
import Handler.Update

-- This is the second part which is needed for Setting up the "Chat" foundation type.
-- The first part of this process is being done in "Foundation.hs" using the function
-- "mkYesodData"

-- It is important to note that the "resourcesChat" is not a function which is being
-- explicitly defined somewhere. It is dynamically created by the mkYesodData function.
-- The of this function is always in the scheme "resources[NAME OF FOUNDATION DATA TYPE]"

-- Calling this function is necessary for the warp function in main to work!
-- This method of using two functions for the foundation type setup is necessary when
-- we want to define the foundation data type in one file, the handlers and the application
-- main in other files.
-- The alternative would be to use the single command "mkYesod". But using this command would
-- require us to define everything in a single file: The foundation data type, the handlers
-- for the routes and the application main...
mkYesodDispatch "Chat" resourcesChat


makeFoundation :: IO Chat
makeFoundation = do
    -- Some basic initializations: HTTP connection manager, logger, and static
    -- subsite.
    chatHttpManager <- getGlobalManager
    chatLogger <- newStdoutLoggerSet defaultBufSize >>= makeYesodLogger

    -- We need a log function to create a connection pool. We need a connection
    -- pool to create our foundation. And we need our foundation to get a
    -- logging function. To get out of this loop, we initially create a
    -- temporary foundation without a real connection pool, get a log function
    -- from there, and then create the real foundation.
    let mkFoundation chatConnPool = Chat {..}
        -- The App {..} syntax is an example of record wild cards. For more
        -- information, see:
        -- https://ocharles.org.uk/blog/posts/2014-12-04-record-wildcards.html
        tempFoundation = mkFoundation $ error "connPool forced in tempFoundation"
        logFunc = messageLoggerSource tempFoundation chatLogger

    -- Create the database connection pool
    pool <- flip runLoggingT logFunc $ createPostgresqlPool
        ("postgres://debug:debug@postgres:5432/haskellchat")
        (10)

    -- Perform database migration using our application's logging settings.
    runLoggingT (runSqlPool (runMigration migrateAll) pool) logFunc

    -- Return the foundation
    return $ mkFoundation pool


chatMain :: IO ()
chatMain = do
    foundation <- makeFoundation
    warp 3000 foundation