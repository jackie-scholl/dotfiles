{-# LANGUAGE FlexibleContexts, ScopedTypeVariables #-}

import XMonad

import XMonad.Config.Kde

import XMonad.Layout.Grid
import XMonad.Layout.MultiColumns
import XMonad.Layout.ThreeColumns
import XMonad.Layout.Spacing
import XMonad.Layout.NoBorders
import XMonad.Layout.LayoutModifier (ModifiedLayout)
    
import qualified XMonad.StackSet as W

import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.ManageHook
import XMonad.Operations
import XMonad.Util.Dzen
import XMonad.Util.EZConfig
import XMonad.Util.NamedWindows
import qualified XMonad.Util.Paste as Paste
import XMonad.Util.Run (spawnPipe)

import Data.Foldable (fold)
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.Maybe
import Control.Monad
import System.IO

main = xmonad =<< (myStatusBar $ docks myConfig )--`additionalKeysP` myKeymap {-`additionalKeys` myShortcutMaps1-} )

myConfig = def
        { manageHook = manageHook kde4Config <+> manageDocks <+> (isFullscreen --> doFullFloat) <+> manageHook def
        , layoutHook = myLayouts
        , keys = \c -> myShortcutMaps1 <> mkKeymap c myKeymap <> keys defaultConfig c
        --, keys = \c -> mkKeymap c myKeymap
        , startupHook = return () >> checkKeymap myConfig myKeymap >> blah2
        , handleEventHook = handleEventHook defaultConfig <+> docksEventHook
        , terminal = "konsole"
        , focusFollowsMouse = False
        , workspaces = ["left", "right"] -- our goal is to not use workspaces
        , modMask = mod4Mask     -- Rebind Mod to the Windows key
        }

blah2 :: X ()
blah2 = myDzen "foobar" 

myDzen :: String -> X ()
myDzen = dzenConfig myDzenConfig
    where
        myDzenConfig :: DzenConfig
        myDzenConfig = center 200 200 0

mySpacing = spacingRaw True emptyBorder False smallBorder True
    where
        emptyBorder = Border 0 0 0 0
        smallBorder = Border 5 5 5 5
        largeBorder = Border 30 30 30 30
        
myLayouts = (noBorders $ Full) ||| 
            (avoidStruts $ mySpacing $ 
            Tall 1 (3/100) (1/2) ||| Mirror (Tall 1 (3/100) (3/5)))

myPrettyPrinter = xmobarPP
                { ppCurrent = xmobarColor "blue" "" . wrap "[" "]"
                -- , ppVisible = const ""
                -- , ppLayout = const ""
                , ppTitle =  xmobarColor "green" "" . shorten 50
                , ppOrder = myPPOrder
                }

myPPOrder (a:b:c:x) = [c]

myStatusBar = statusBar "xmobar" myPrettyPrinter $ const (noModMask, xK_VoidSymbol)

myKeymap :: [(String, X ())]
myKeymap = windowingKeys <> audioKeys -- <> firefoxKeys 

windowingKeys :: [(String, X ())]
windowingKeys = [("M-" ++ show (modifier windowNum), helper workspace windowNum)
          | windowNum <- [0..4],
          (workspace, modifier) <- [("left", \x -> x + 1), ("right", \x -> (x+6) `mod` 10) ]
          ]
    where
        helper myWorkspace windowNumber = windows $ \x -> helper2 x myWorkspace windowNumber 

        helper2 :: WindowSet -> String -> Int -> WindowSet
        helper2 windowSet myWorkspace windowNumber = focused
            where
                correctedWorkspace :: WindowSet
                correctedWorkspace = W.view myWorkspace windowSet
                windowList = W.index correctedWorkspace
                targetWindow = windowList !! windowNumber
                focused = W.focusWindow targetWindow correctedWorkspace 

audioKeys :: [(String, X ())]
audioKeys = [
    ("<XF86AudioMute>",        spawn "amixer -q  set Master mute"),
    ("<XF86AudioLowerVolume>", spawn "amixer -q sset Master 2%-"),
    ("<XF86AudioRaiseVolume>", spawn "amixer -q sset Master 2%+")
    ]

{-}
data WindowType = Firefox | NotFirefox
data MoveType = MLeft | MRight

changeTab :: MoveType -> WindowType -> X ()
changeTab moveType Firefox = sendKey controlMask $ 
    case moveType of
        MLeft  -> xK_Page_Up
        MRight -> xK_Page_Down
changeTab moveType NotFirefox = sendKey mod1Mask $
    case moveType of
        MLeft  -> xK_Left
        MRight -> xK_Right

queryAndChangeTab :: MoveType -> X ()
queryAndChangeTab moveType = withFocused $ (>>= changeTab moveType ) . runQuery firefoxQuery
    where
        firefoxQuery :: Query WindowType
        firefoxQuery = fmap (\x -> if x then Firefox else NotFirefox) $ className =? "Firefox" -- XMonad.ManageHook
-}


invertMapOfMaps :: forall a b c. (Ord a, Ord b) => M.Map a (M.Map b c) -> M.Map b (M.Map a c)
invertMapOfMaps myMap = M.fromSet bToACMap bset
    where
        bset :: S.Set b
        bset = fold $ fmap M.keysSet myMap

        bToACMap :: b -> M.Map a c
        bToACMap bravo = M.mapMaybe (M.lookup bravo) myMap

type Keystroke = (KeyMask, KeySym)
sendKey2 :: Keystroke -> X ()
sendKey2 = uncurry Paste.sendKey

showKey :: Keystroke -> X ()
showKey = myDzen . show

produceOriginalInput :: Functor f => (a -> f ()) -> (a -> f a)
produceOriginalInput action input = fmap (const input) (action input)

shortcutMapper :: M.Map String (M.Map Keystroke Keystroke) -> M.Map Keystroke (X ())
shortcutMapper shortcutMap = M.mapWithKey sendMappedKeystroke $ invertMapOfMaps shortcutMap

sendMappedKeystroke :: Keystroke -> M.Map String Keystroke -> X ()
sendMappedKeystroke ks destMap = getKeystroke >>= produceOriginalInput showKey >>= sendKey2
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

myShortcutMaps :: M.Map String (M.Map Keystroke Keystroke)
myShortcutMaps = M.fromList
    [
        ("Firefox", firefoxShortcuts),
        ("konsole", firefoxShortcuts)
    ]

myShortcutMaps1 :: M.Map Keystroke (X ())
myShortcutMaps1 = shortcutMapper myShortcutMaps


firefoxShortcuts :: M.Map Keystroke Keystroke
firefoxShortcuts = M.fromList $ [
    ((mod1Mask, xK_Right), (controlMask, xK_Page_Down)),
    ((mod1Mask, xK_Left), (controlMask, xK_Page_Up))
    ]

withMask :: KeyMask -> Keystroke -> Keystroke
withMask addMask (keyMask, keySym) = undefined 


{-}
firefoxKeys :: [(String, X ())]
firefoxKeys = [
    ("M1-<Right>", queryAndChangeTab MRight),
    ("M1-<Left>",  queryAndChangeTab MLeft )
    ]
-}

        {-, logHook = dynamicLogWithPP xmobarPP
                        { ppCurrent = xmobarColor "blue" "" . wrap "[" "]"
                        , ppTitle = xmobarColor "green" "" . shorten 50
                        , ppOutput = hPutStrLn xmproc
                        }
        -}
