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


widgetFileSettings :: WidgetFileSettings
widgetFileSettings = def

widgetFile :: String -> Q Exp
widgetFile = widgetFileReload widgetFileSettings