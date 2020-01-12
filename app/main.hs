{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
import            Prelude       (IO)
import            Application   (chatMain)

-- The "main" function is the standard entry point for the haskell compiler and it HAS TO
-- return an IO data type.
-- the "warp" function makes use of the "Warp" web server, which has been written specifically
-- for Yesod. The first argument is the port number to run on and the second one the
-- foundation data type, which describes the entire application, that is supposed to be run
-- in this web server...
main :: IO ()
main = chatMain
