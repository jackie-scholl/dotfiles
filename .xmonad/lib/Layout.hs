
{-# LANGUAGE FlexibleContexts #-}

module Layout (myLayouts) where

import XMonad

import GHC.Word

import XMonad.Layout.Spacing
import XMonad.Layout.Spiral
import XMonad.Layout.PerWorkspace --(onWorkspace)
import XMonad.Layout.ThreeColumns
import XMonad.Layout.Reflect

--import XMonad.Layout.NoBorders
import XMonad.Hooks.ManageDocks

import XMonad.Layout.LayoutModifier (ModifiedLayout)

mySpacing = spacingRaw True emptyBorder False smallBorder True
    where
        emptyBorder = Border 0 0 0 0
        smallBorder = Border 5 5 5 5
        largeBorder = Border 30 30 30 30
        
myLayouts = mySpacing $ myLayouts2

myLayouts2 :: PerWorkspace (ThreeCol) (PerWorkspace (Mirror Tall) XMonad.Layout.Spiral.SpiralWithDir) GHC.Word.Word64
--myLayouts2 = onWorkspace "left" (Tall 1 (3/100) (1/3)) $ onWorkspace "right" (Mirror $ Tall 1 (3/100) (3/5)) $ spiral (6/7)
myLayouts2 = onWorkspace "left" (ThreeCol 1 (3/100) (1/3)) $ onWorkspace "right" (Mirror $ Tall 1 (3/100) (3/5)) $ spiral (6/7)

