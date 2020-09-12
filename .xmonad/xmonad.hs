{-# LANGUAGE FlexibleContexts, ScopedTypeVariables #-}

import XMonad

import XMonad.Config.Kde

import XMonad.Layout.Grid
import XMonad.Layout.MultiColumns
import XMonad.Layout.Spacing
import XMonad.Layout.NoBorders
import XMonad.Layout.LayoutModifier (ModifiedLayout)
    
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.ManageHook
import XMonad.Operations
--import XMonad.Util.Dzen
import XMonad.Util.EZConfig

import Shortcuts
import StatusBar

main = xmonad =<< (myStatusBar $ docks myConfig )

myDzenFlags :: String 
myDzenFlags = "-e onstart lower -w 800 -h 100"

myConfig = def
        { manageHook = manageHook kde4Config {-<+> manageDocks <+> (isFullscreen --> doFullFloat)-} <+> manageHook def
        , layoutHook = myLayouts
        , keys = \c ->  myShortcuts c <> keys defaultConfig c
        --, startupHook = return () >> checkKeymap myConfig audioKeys
        , handleEventHook = handleEventHook defaultConfig <+> docksEventHook
        , terminal = "konsole"
        , focusFollowsMouse = False
        , workspaces = ["left", "right"] -- our goal is to not use workspaces
        , modMask = mod4Mask     -- Rebind Mod to the Windows key
        }

mySpacing = spacingRaw True emptyBorder False smallBorder True
    where
        emptyBorder = Border 0 0 0 0
        smallBorder = Border 5 5 5 5
        largeBorder = Border 30 30 30 30
        
myLayouts = avoidStruts $ mySpacing $ 
            Tall 1 (3/100) (1/3) ||| Mirror (Tall 1 (3/100) (3/5))


myStatusBar = statusBar "dzen2" myPrettyPrinter $ const (noModMask, xK_VoidSymbol)



