{-# LANGUAGE FlexibleContexts, ScopedTypeVariables #-}

module StatusBar (myPrettyPrinter) where

import XMonad
import XMonad.Util.Dzen
import XMonad.Util.Loggers

import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers

someStatusThing = 5

myPrettyPrinter :: PP
myPrettyPrinter = dzenPP
                { ppTitle =  dzenColor "green" "" . shorten 50
                --, ppOrder = myPPOrder
                , ppSep = " | "
                , ppExtras = [date dateFormat, loadAvg, logCmd volumeCommand, logCmd "echo hello"]
                }
    where
        myPPOrder (a:b:c:x) = [c]

dateFormat :: String
dateFormat = "%a %b %_d %Y %-I:%M:%S %p"

volumeCommand = "amixer sget Master | egrep --only-matching --max-count 1 '([0-9]){2,3}%'"


--myStatusBar = statusBar "dzen2" myPrettyPrinter $ const (noModMask, xK_VoidSymbol)

