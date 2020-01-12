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
module Handler.Update where

import Database.Persist
import Database.Persist.Postgresql
import Database.Persist.TH
import Database.Persist.Sql
import Data.Time
import Control.Applicative
import Data.Text (Text)
import Data.Int
import Data.Aeson.Types
import Data.List
--import Network.HTTP.Types.Status (status400, status200)

import Import
import Chat


-- | This is the data type, that represents the input from the URL variables passed to the
--   "/update" route of the application. This route is used by the client side program to
--   request new chat messages to be displayed.
data UpdateRequest = UpdateRequest
    { updateRequestIndex    :: Int
    , updateRequestUsername :: Text
    }

-- | This is the data type, that represents the reply, which the server is giving the client
--   following a "update" request. This reply contains a list of all the new messages.
data UpdateReply = UpdateReply
    { updateReplyMessages   :: [OutMessage]
    }

-- NOTE: The Json capabilities are coming from the third party package "aeson" and less so from
-- the Yesod framework itself!
-- So this is how you convert something to JSON: You have to define your data type as an instance
-- of the ToJSON type class and then define the toJSON function for it.
-- | Implementation of how a "UpdateReply" type is converted into a JSON string
instance ToJSON UpdateReply where
    -- NOTE: to use this weird "{..}" notation you need the {# LANGUAGE RecordWildCards #} on top of
    -- the file!
    -- See the line "[ "messages" .= updateReplyMessages ]":
    -- The string there defines the key, which is later the JSON key for this data
    -- ".=" is some weird syntax, which is just required.
    -- The "updateReplyMessages" has to match the name given to this field in the data type definition!
    -- in this case the field is a list. That is no problem, during conversion this is automatically
    -- converted into a JSON list, but the elements of the list must be instances of ToJSON type class
    -- as well of course.
    -- As far as I have understood it, it is the "object" function which actually does it.
    toJSON (UpdateReply
            {..}) = object
                [ "messages" .= updateReplyMessages ]

-- THE OutMessage DATA TYPE
-- Lets talk about this data type: To be honest I only introduced it, because I did not know
-- how to write the toJSON method for the "Entity Message" type (from the database) directly.
-- But I knew how to "unpack" it into a "normal" data type. So Entity Messages are now first
-- converted into OutMessages, which can then be parsed as JSON.

-- | This data type represents a message by just its content. The actual "Entity Message"
--   data type from the database is converted into this data type, because a simple data type
--   like this can be converted into a JSON string better
data OutMessage = OutMessage
    { outMessageContent     :: Text
    , outMessageUsername    :: Text
    } deriving Show

-- | Implementation of how a "OutMessage" type is converted into a JSON string
instance ToJSON OutMessage where
    toJSON (OutMessage
            {..}) = object
                [ "content" .= outMessageContent
                , "username" .= outMessageUsername
                ]

-- | Given a list of "Entity Message" from the database (returned by selectList for example),
--   this function will convert it into a list of "OutMessage", which can be parsed as JSON
--   easily.
outMessagesFromMessages :: [(Entity Message)] -> HandlerFor Chat [OutMessage]
outMessagesFromMessages ms = mapM toOutMessage ms

-- | This function converts a single "Entity Message" type into a "OutMessage" type.
toOutMessage :: (Entity Message) -> HandlerFor Chat OutMessage
-- IMO the best way to get to the *actual* "Message" data type within a "Entity Message" is
-- to create a separate function and then utilize the pattern matching utilities a function
-- definition provides.
toOutMessage (Entity messageId message) = do
    maybeUser <- runDB (get $ messageUserId message)
    let user     = fromMaybe maybeUser
        username = userName user
    return $ OutMessage (messageContent message) (username)
        where
            fromMaybe (Just user) = user
            fromMaybe (Nothing) = User "Anonymus"

-- The type signature here is really important! Why you ask?
-- so first up the type signature of the "toSqlKey" function is "Int64 -> Key record". Yes you
-- saw correctly it is only a key for the generic "record" type. So if we just used the
-- "toSqlKey" function directly in a database "get" for example, it wouldn't know in which
-- table to look up the key, because "record" does not specify any specific model!
-- But in the end, we can fix this by simply changing the type signature of this function,
-- which will *sort of* "implicitly cast it"
-- | Given an integer value, this function returns the corresponding "Key Message" data type
--   value, which represents the database key for the "Message" column at the given integers
--   as primary key.
keyMessage :: Int -> (Key Message)
-- The type signature of "toSqlKey" is "Int64 -> Key record". So the fromIntegral part there is
-- because of the "Int64" parameter requirement. "fromIntegral" will convert the Int to the needed
-- Int 64 type.
keyMessage i = toSqlKey $ fromIntegral i


-- Lets talk about this weird type signature here: The documentation says it returns a
-- list of all the "Entity Message", but the type signature says. "HandlerFor Chat [Entity Message]"
-- why is that?
-- This comes down to the fact, that this message needs to be "IO called" (using the <- operator)
-- from within the Handler function. Now the way a do Block works is, that it *sort of* concats all
-- the expressions within the do block and then returns the result of this "fusing". The handler
-- function has to return a "Handler" type at the end of the day (=the fused result). Apparently
-- This is only possible when all of the "<-" called function are *wrapped* in the "HandlerFor Chat"
-- monad!
-- | This function returns a list with all the "Entity Message" from the database, whose
--   primary key (MessageId) is real greater than the given integer.
getMessagesGreater :: Int -> HandlerFor Chat [Entity Message]
getMessagesGreater i = do
    messages <- runDB (selectList [MessageId >. (keyMessage i)] [])
    return messages


-- | This is the handler for a GET request to the "/update" url of the application.
--   The handler will parse the URL parameters given with the request, then look up the database
--   for all the new messages and then return a JSON response containing the list of just these
---  new messages.
getUpdateR :: Handler RepJson
getUpdateR = do
    -- PARSING THE URL VARIABLES
    updateRequest <- runInputGet $ UpdateRequest
            <$> ireq intField "index"
            <*> ireq textField "username"

    let index       = updateRequestIndex updateRequest
        username    = updateRequestUsername updateRequest

    -- GETTING THE NEW MESSAGES FROM THE DATABASE
    -- This function "getMessagesGreater" returns a list of "Entity Message" (data type directly
    -- coming from the database)
    messages <- getMessagesGreater index

    -- CONVERTING MESSAGES TO JSON REPLY
    -- "outMessagesFromMessages" converts [Entity Message] to [OutMessage], because OutMessage is
    -- the data type, which can be converted into JSON!
    outMessages <- outMessagesFromMessages messages
    -- NOTE: status200 is success, whereas status400 would be failure.
    -- The "toJSON" function has the type signature "ToJSON a => a -> Value", which means JSON
    -- conversions always return "Value" data type
    -- as such "sendStatusJSON" needs the two parameters with types "Status" ans "Value"
    let updateReply = UpdateReply outMessages
    sendStatusJSON status200 (toJSON updateReply)