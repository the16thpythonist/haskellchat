{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.Chat where

import Import
import Chat

getHomeR :: Handler Html
getHomeR = do
    defaultLayout $ do
        setTitle "HaskellChat"
        let chat = $(widgetFile "chat")
        $(widgetFile "home")