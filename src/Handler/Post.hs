{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE RecordWildCards            #-}
module Handler.Post where

import Database.Persist
import Database.Persist.Postgresql
import Database.Persist.TH
import Data.Time
import Control.Applicative
import Data.Text (Text)
import Data.Int
import Data.Aeson.Types

import Import
import Chat

-- This is the data type, "into" which the URL variables for a GET request to this route are
-- parsed.
-- | This is the data type, that represents the input from a form submission to the "/post"
--   route of the application. It is used by a user to post a new chat message to the chat.
data PostRequest = PostRequest
    { postRequestContent    :: Text
    , postRequestUsername   :: Text
    }
    deriving Show

-- | This is the data type, which represents the reply the server sends to the client following
--   a "/post" request. This request simply contains the internal ID of the message, that has
--   been posted to the chat.
data PostReply = PostReply
    { postReplyID           :: Int
    } deriving Show

-- | Implementation of how the PostReply type is converted into JSON
instance ToJSON PostReply where
    toJSON (PostReply
            {..}) = object
                [ "ID" .= postReplyID ]


-- | Given the string name of a user, this function looks up, whether this user already
--   exists. If this user does exist it will return its ID, if it does not yet exist a new
--   user will be created in the DB and the new key will be returned
maybeInsertUser username = do
    -- The "getBy" database function will return a single database record by a unique field constraint
    -- In this case UniqueName is a unique constraint, which has been derived from the string name of
    -- the user.
    -- It is important to note, that getBy returns a "Maybe Record" so its value is either "Just record"
    -- or Nothing.
    maybeUser <- runDB (getBy $ UniqueName username)
    userId <- maybeInsert username maybeUser
    return userId
        where
            -- This is a helper function, which uses pattern matching to deduce if the value is a Just ...
            -- or nothing. In case of Nothing a new user with that username is being inserted and this
            -- new UserId is returned...
            maybeInsert _ (Just (Entity userId user)) = return userId
            maybeInsert username Nothing = do
                userId <- runDB (insert $ User username)
                return $ userId

-- Going by yesods naming convention, this is the handler function for a GET call to the route
-- "PostR"
getPostR :: Handler RepJson
getPostR = do
    -- Important note about forms: It seems to be necessary to define the foundation type of the
    -- application as an instance of the "RenderMessage" typeclass.
    -- For more information about the code snippet to do that see the "Foundation.hs" file

    -- PARSING URL VARIABLES
    -- We can use the function "runInputGet" to parse the GET URL parameters, which have been
    -- passed when the route was called.
    -- To do this we need to define a data type, "into" which we can stuff the values. Using "ireq"
    -- in the definition means, that the parsing absolutely expects this value. The first argument
    -- is the type of value that is expected from that field and the second one the name if the URL
    -- variable.
    -- Furthermore "iopt" can be used for optional values within the URL query.
    postRequest <- runInputGet $ PostRequest
            <$> ireq textField "content"
            <*> ireq textField "username"


    -- INSERTING THE NEW MESSAGE
    let username = postRequestUsername postRequest
        content  = postRequestContent postRequest

    -- This function "maybeInsertUser" works like "get_or_insert" in other frameworks: If the user
    -- does not exist by that name a new user will be added.
    -- It is important to note here, that "unpack username" is being used. username is of the type
    -- Text, but all the database functions need the String type. "unpack" converts Text -> String
    uid <- maybeInsertUser username

    -- NOTE on database interactions: It is super important, that runDB is absolutely necessary to
    -- run any kind of database functions!
    mi <- runDB (insert $ Message uid content)
    let index = fromIntegral $ fromSqlKey mi
    let postReply = PostReply index
    -- RETURNING THE JSON REPLY
    sendStatusJSON status200 (toJSON postReply)