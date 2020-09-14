{-# LANGUAGE FlexibleContexts, ScopedTypeVariables #-}

module Shortcuts (myShortcuts) where

import XMonad

import qualified XMonad.StackSet as W
import qualified XMonad.Util.EZConfig as EZConfig
--import XMonad.Util.Dzen
import qualified XMonad.Util.Paste as Paste
import XMonad.Actions.CycleWS

import qualified Data.List as List 
import Data.Foldable (fold)
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.Maybe
import Control.Monad
import System.IO

type Keystroke = (KeyMask, KeySym)

myShortcuts :: XConfig l -> M.Map Keystroke (X ())
myShortcuts config = windowingKeys config <> audioKeys config <> myShortcutMaps

windowingKeys :: XConfig l -> M.Map Keystroke (X ())
windowingKeys config = includeModifier `M.mapKeys` (focusSpaceAndWindow <$> indexAllKeys myWorkspaceKeyMapping)
    where
        myWorkspaceKeyMapping :: [(String, [KeySym])]
        myWorkspaceKeyMapping = [("left", [xK_1..xK_5]), ("right", [xK_6..xK_9])]

        indexList :: (Ord a) => [a] -> M.Map a Int
        indexList xs = M.fromList $ zip xs [0..] 
        
        indexKeys :: (String, [KeySym]) -> M.Map KeySym (String, Int)
        indexKeys (x, y) = pair x <$> indexList y

        indexAllKeys :: [(String, [KeySym])] -> M.Map KeySym (String, Int)
        indexAllKeys workspaceKeyMapping = fold $ indexKeys <$> workspaceKeyMapping

        focusSpaceAndWindow :: (String, Int) -> X ()
        focusSpaceAndWindow (workspaceName, windowNumber) = windows $ focusSpaceAndWindowHelper workspaceName windowNumber

        includeModifier :: KeySym -> Keystroke
        includeModifier = pair (XMonad.modMask config)
        
        focusSpaceAndWindowHelper :: String -> Int -> WindowSet -> WindowSet
        focusSpaceAndWindowHelper myWorkspace windowNumber windowSet = focused
            where
                correctedWorkspace = W.view myWorkspace windowSet
                windowList = W.index correctedWorkspace
                targetWindow = windowList !! windowNumber
                focused = W.focusWindow targetWindow correctedWorkspace

audioKeys :: XConfig l -> M.Map Keystroke (X ())
audioKeys config = EZConfig.mkKeymap config [
    ("<XF86AudioMute>",        spawn "amixer -q  set Master mute"),
    ("<XF86AudioLowerVolume>", spawn "amixer -q sset Master 2%-"),
    ("<XF86AudioRaiseVolume>", spawn "amixer -q sset Master 2%+"),
    ("M-y", swapNextScreen)
    ]




sendKey2 :: Keystroke -> X ()
sendKey2 = uncurry Paste.sendKey

shortcutMapper :: M.Map String (M.Map Keystroke Keystroke) -> M.Map Keystroke (X ())
shortcutMapper shortcutMap = M.mapWithKey sendMappedKeystroke $ invertMapOfMaps shortcutMap

sendMappedKeystroke :: Keystroke -> M.Map String Keystroke -> X ()
sendMappedKeystroke ks destMap = getKeystroke >>= sendKey2 -- produceOriginalInput sendKey2 >>= showKey
    where
        getKeystroke :: X Keystroke
        getKeystroke = fromMaybe ks <$> possibleKeystroke

        possibleKeystroke :: X (Maybe Keystroke)
        possibleKeystroke = withFocused' $ runQuery mappedQuery

        mappedQuery :: Query (Maybe Keystroke)
        mappedQuery = (destMap M.!?) <$> className

withFocused' :: forall a. (Window -> X (Maybe a)) -> X (Maybe a)
withFocused' f = withWindowSet $ runOnFocusedWindow
    where
        runOnFocusedWindow :: WindowSet -> X (Maybe a)
        runOnFocusedWindow ws = fromMaybe (pure Nothing) $ f <$> W.peek ws

myShortcutMaps :: M.Map Keystroke (X ())
myShortcutMaps = shortcutMapper $ M.fromList
    [   ("fakeProgramDoesnExist", M.empty)
        , ("Firefox", firefoxShortcuts)
    ]


firefoxShortcuts :: M.Map Keystroke Keystroke
firefoxShortcuts = M.fromList $ [
    ((mod1Mask, xK_Right), (controlMask, xK_Page_Down)),
    ((mod1Mask, xK_Left ), (controlMask, xK_Page_Up  )),
    ((controlMask, xK_Q),  (noModMask, xK_VoidSymbol))
    ]

--withMask :: KeyMask -> Keystroke -> Keystroke
--withMask addMask (keyMask, keySym) = (keyMask .|. addMask, keySym)


-- UTILITIES

pair :: a -> b -> (a, b)
pair a b = (a, b)

produceOriginalInput :: Functor f => (a -> f ()) -> (a -> f a)
produceOriginalInput action input = fmap (const input) (action input)

both :: (a -> b) -> (a, a) -> (b, b)
both f (x, y) = (f x, f y) 

invertMapOfMaps :: forall a b c. (Ord a, Ord b) => M.Map a (M.Map b c) -> M.Map b (M.Map a c)
invertMapOfMaps myMap = M.fromSet bToACMap bset
    where
        bset :: S.Set b
        bset = fold $ fmap M.keysSet myMap

        bToACMap :: b -> M.Map a c
        bToACMap bravo = M.mapMaybe (M.lookup bravo) myMap







