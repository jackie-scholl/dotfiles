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

import XMonad.Hooks.EwmhDesktops

import Shortcuts
import StatusBar
import Layout

main = xmonad =<< (myStatusBar $ docks myConfig )

myConfig = ewmh $ def
        { manageHook = {-manageDocks <+> (isFullscreen --> doFullFloat) <+>-} manageHook def
        , layoutHook = myLayouts
        , keys = \c ->  myShortcuts c <> keys defaultConfig c
        --, startupHook = return () >> checkKeymap myConfig audioKeys
        , handleEventHook = handleEventHook defaultConfig <+> docksEventHook
        , terminal = "konsole"
        , focusFollowsMouse = False
        , workspaces = ["left", "right"] -- our goal is to not use workspaces
        , modMask = mod4Mask     -- Rebind Mod to the Windows key
        }

myStatusBar = statusBar "xmobar" myXmobarPP $ const (noModMask, xK_VoidSymbol)

myXmobarPP = xmobarPP {ppTitle = xmobarColor "green" "" . shorten 50, ppOrder = third}

third (a:b:c:x) = [c]

