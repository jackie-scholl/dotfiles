{-# LANGUAGE FlexibleContexts, ScopedTypeVariables #-}

module StatusBar (myPrettyPrinter) where

import XMonad
import XMonad.Util.Dzen
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers

someStatusThing = 5

myPrettyPrinter :: PP
myPrettyPrinter = dzenPP
                { ppTitle =  dzenColor "green" "" . shorten 50
                --, ppOrder = myPPOrder
                }
    where
        myPPOrder (a:b:c:x) = [c]


--myStatusBar = statusBar "dzen2" myPrettyPrinter $ const (noModMask, xK_VoidSymbol)
