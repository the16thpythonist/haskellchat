{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE InstanceSigs #-}

module Foundation where

import Import.NoFoundation

import Database.Persist.Sql     (ConnectionPool,
                                 runSqlPool)
import Yesod.Core.Types         (Logger)
import Text.Hamlet              (hamletFile)

import Settings.Chat

-- Here we define, what "fields" the basic foundation data type contains.
-- ConnectionPool: This will be the connection to the database later on.
data Chat = Chat
    { chatConnPool      :: ConnectionPool
    , chatHttpManager   :: Manager
    , chatLogger        :: Logger
    }

-- This function sets up the "Chat" data type as the yesod fundation data type. Using the
-- "mkYesodData" instead of "mkYesod" enables the definition of the foundation data type,
-- the handlers and the application main in different modules. BUT it also requires the
-- calling of another function "mkYesodDispatch" in the main module...

-- The first argument to this function the string name of the foundation data type, which
-- is defined above... The second argument is some sort of a definition of the url routes
-- which make up this application. These can either be defined by using the function
-- "parseRoutesFile" and then supply a separate file (by relative path system toward
-- the application main folder) or directly by using the "parseRoutes" QuasiQuoter for
-- an inline DSL section
mkYesodData "Chat" $(parseRoutesFile "config/routes")

-- This is the "foundation data type"
instance Yesod Chat where
    defaultLayout :: Widget -> Handler Html
    defaultLayout widget = do
        master <- getYesod

        pc <- widgetToPageContent $ do
            $(widgetFile "default-layout")
        withUrlRenderer $(hamletFile "templates/base.hamlet")

-- I think this defines the foundation type "Chat" as an instance of the typeclass
-- "RenderMessage".
-- I am not sure what this does in detail, but this seems to be required to be able to
-- use forms of any kind with the application!
instance RenderMessage Chat FormMessage where
    renderMessage _ _ = defaultFormMessage


instance YesodPersist Chat where
    type YesodPersistBackend Chat = SqlBackend
    runDB :: SqlPersistT Handler a -> Handler a
    runDB action = do
        master <- getYesod
        runSqlPool action $ chatConnPool master