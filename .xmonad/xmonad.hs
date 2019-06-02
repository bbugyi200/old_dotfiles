{-# LANGUAGE DeriveDataTypeable, TypeSynonymInstances, MultiParamTypeClasses #-}

-------------------------------------------------------------------------------
-- IMPORTED LIBRARIES                                                        --
-------------------------------------------------------------------------------
import Control.Monad
import Data.Ratio
import Graphics.X11.ExtraTypes.XF86
import XMonad
import XMonad.Core
import XMonad.Layout
import XMonad.Layout.Grid
import XMonad.Layout.MultiToggle
import XMonad.Layout.TwoPane
import XMonad.Layout.Tabbed

import XMonad.Actions.SpawnOn (spawnOn,spawnHere,manageSpawn)
import XMonad.Config.Desktop (desktopConfig)
import XMonad.Hooks.EwmhDesktops (ewmh,ewmhDesktopsLogHook,ewmhDesktopsStartup)
import XMonad.Hooks.ManageHelpers (doRectFloat,doFullFloat)
import XMonad.Hooks.SetWMName (setWMName)
import XMonad.Layout.NoBorders (smartBorders)
import XMonad.Layout.Spacing (smartSpacing)
import XMonad.Util.Run (spawnPipe,hPutStrLn)
import XMonad.Util.EZConfig (additionalKeys)
import XMonad.Util.WorkspaceCompare (getSortByIndex)

import qualified Data.Char as DataChar
import qualified Data.List as DataList
import qualified Network.HostName as HostName
import qualified System.Exit as Exit
import qualified XMonad.Actions.CycleWS as CW
import qualified XMonad.Actions.DynamicWorkspaces as DW
import qualified XMonad.Actions.DynamicWorkspaceOrder as DW
import qualified XMonad.Actions.FloatKeys as FK
import qualified XMonad.Actions.Navigation2D as N2D
import qualified XMonad.Actions.UpdatePointer as UP
import qualified XMonad.Hooks.DynamicLog as DL
import qualified XMonad.Hooks.ManageDocks as Docks
import qualified XMonad.Layout.ResizableTile as RT
import qualified XMonad.Prompt as P
import qualified XMonad.StackSet as W
import qualified XMonad.Util.NamedScratchpad as NSP

-- hlint ignore directives
{-# ANN module "HLint: ignore Evaluate" #-}

-------------------------------------------------------------------------------
-- XMobar Templates                                                          --
-------------------------------------------------------------------------------
getXmobarTemplate :: String -> String
getXmobarTemplate "1-top-athena" = "%UnsafeStdinReader%}%wstatus% %wpoll%{ %pia%  %volume%  |  %date%"
getXmobarTemplate "1-top-aphrodite" = "%UnsafeStdinReader%    (%window_count%)}{ %pia%  %battery%  |  %volume%  |  %date%"
getXmobarTemplate "1-bottom" = "%cpu%  |  %memory%}%calevent%{%counter%%dynnetwork%"
getXmobarTemplate "2-top" = "}%weather%%xweather%     (☀ %suntimes%%xsuntimes%){"
getXmobarTemplate "2-bottom" = "}{"
getXmobarTemplate "3-top" = "}{"
getXmobarTemplate "3-bottom" = "}{"

-------------------------------------------------------------------------------
-- MAIN                                                                      --
-------------------------------------------------------------------------------
main :: IO ()
main = do
    hostname <- HostName.getHostName
    xmproc <- spawnPipe (xmobarTempFmt (getXmobarTemplate $ "1-top-" ++ hostname) ++ " --screen=1")
    xmonad . Docks.docks . ewmh $ desktopConfig
        {
            terminal                = myTerminal
          , modMask                 = alpha
          , borderWidth             = myBorderWidth
          , focusedBorderColor      = myFocusedBorderColor
          , focusFollowsMouse       = myFocusFollowsMouse
          , clickJustFocuses        = myClickJustFocuses
          , workspaces              = myWorkspaces
          , manageHook              = myManageHook
          , layoutHook              = Docks.avoidStruts myLayout
          , startupHook             = myStartupHook
          , logHook                 = DL.dynamicLogWithPP DL.xmobarPP
            { DL.ppOutput                = hPutStrLn xmproc
            , DL.ppOrder                 = myPpOrder
            , DL.ppSep                   = DL.xmobarColor "white" "" "    "
            , DL.ppCurrent               = DL.xmobarColor "yellow" "" . strToUpper . DL.wrap "[" "]"
            , DL.ppVisible               = DL.xmobarColor "yellow" "" . strToUpper
            , DL.ppHidden                = DL.xmobarColor "white" "" . strToUpper
            , DL.ppHiddenNoWindows       = DL.xmobarColor "darkgrey" "" . strToUpper
            , DL.ppWsSep                 = "    "
            , DL.ppTitle                 = DL.xmobarColor "green"  "" . DL.shorten 40
            , DL.ppSort                  = (NSP.namedScratchpadFilterOutWorkspace .) `liftM` DW.getSortByOrder
            } >> ewmhDesktopsLogHook <+> DL.dynamicLogXinerama
      } `additionalKeys` myAdditionalKeys

-------------------------------------------------------------------------------
-- UTILITY FUNCTIONS                                                         --
-------------------------------------------------------------------------------
-- Function that prevents cycling to workspaces available on other screens
hiddenNotNSP :: X (WindowSpace -> Bool)
hiddenNotNSP = do
  sort <- DW.getSortByOrder
  hs <- gets (map W.tag . sort . NSP.namedScratchpadFilterOutWorkspace . W.hidden . windowset)
  return (\w -> W.tag w `elem` hs)

-- This is a re-implementation of DW.withNthworkspace with "skipTags"
-- added to filter out NSP.
withNthWorkspace' :: (String -> WindowSet -> WindowSet) -> Int -> X ()
withNthWorkspace' job wnum = do
    sort <- DW.getSortByOrder
    ws <- gets (map W.tag . sort . NSP.namedScratchpadFilterOutWorkspace . W.workspaces . windowset)
    case drop wnum ws of
        (w:_) -> windows $ job w
        []    -> return ()

xmobarTempFmt :: String -> String
xmobarTempFmt temp = "xmobar --template=\"" ++ temp ++ "\" /home/bryan/.xmobarrc"

removeEmptyWorkspaceAfter' :: X () -> X ()
removeEmptyWorkspaceAfter' f = do
    workspaceList <- gets (W.workspaces . windowset)
    let n = length workspaceList
    when (n > 3) $ DW.removeEmptyWorkspaceAfter f
    when (n <= 3) f

removeEmptyWorkspace' :: X ()
removeEmptyWorkspace' = do
    workspaceList <- gets (W.workspaces . windowset)
    let n = length workspaceList
    when (n > 4) DW.removeEmptyWorkspace

launchApp :: String -> String -> X ()
launchApp ws cmd = do
    DW.addWorkspace ws
    spawnHere $ "hide_nsp && WS_is_Empty && " ++ cmd

launchAppAndUP :: String -> String -> X ()
launchAppAndUP ws cmd = do
    UP.updatePointer (0.5, 0.5) (0, 0)
    launchApp ws cmd

launchFullApp :: String -> String -> X ()
launchFullApp ws cmd = launchApp ws ("xdotool key super+f && " ++ cmd)

-- Only shows layout when fullscreen mode is enabled
myPpOrder :: [String] -> [String]
myPpOrder (ws:l:t:_) = [ws]

strToUpper :: String -> String
strToUpper = map DataChar.toUpper

pushWindow :: X ()
pushWindow = do 
    CW.swapNextScreen
    CW.toggleWS' ["NSP"]

swapScreens :: String -> X ()
swapScreens dir = do
    removeEmptyWorkspace'
    if dir == "next"
        then CW.swapNextScreen
    else
        CW.swapPrevScreen
    removeEmptyWorkspace'

pushDesktop :: String -> X ()
pushDesktop key = spawn $ "xmonad-scratch-bind " ++ key ++ " 0.15"

delayedSpawn :: Int -> String -> X ()
delayedSpawn seconds cmd = spawn $ "sleep " ++ show seconds ++ " && " ++ cmd

-------------------------------------------------------------------------------
-- LAYOUT CONFIGS                                                            --
-------------------------------------------------------------------------------
myFull = smartBorders simpleTabbed

-- Transformers (W+f)
data TABBED = TABBED deriving (Read, Show, Eq, Typeable)
instance Transformer TABBED Window where
	transform TABBED x k = k myFull (const x)

myLayout = id
    $ mkToggle (single TABBED)
    $ TwoPane (3/100) (1/2) ||| Grid

-------------------------------------------------------------------------------
-- MISCELLANEOUS CONFIGS                                                     --
-------------------------------------------------------------------------------
myFont = "xft:Source Code Pro"
myTerminal = "tm-init"

myFocusFollowsMouse = False
myClickJustFocuses = False

myBorderWidth = 5
myFocusedBorderColor = "#0000FF"

myWorkspaces :: [String]
myWorkspaces = ["NSP", "term","web", "zath"]

myXPConfig :: P.XPConfig
myXPConfig = P.def {
  P.font = myFont,
  P.position = P.CenteredAt 0.2 0.4
}

scratchpads = [ NSP.NS "scratchpad" scratchpad (appName =? "scratchpad") 
                    NSP.nonFloating
              , NSP.NS "calculator" calculator (appName =? "calculator")
                    NSP.nonFloating
              , NSP.NS "spotify" spotify (appName =? "spotify")
                    NSP.nonFloating
              , NSP.NS "gtd" gtd (appName =? "GTD")
                    NSP.nonFloating ]
            where 
                calculator = "urxvt -name calculator -e zsh -c 'wtitle Calculator && bc -l'"
                scratchpad = "scratchpad-launcher"
                spotify = "spotify"
                gtd = "gtd-launcher"

myManageHook = composeAll
    [ manageSpawn
    , NSP.namedScratchpadManageHook scratchpads
    , appName=? "peek"        --> doFloat
    , className=? "Pinentry-gtk-2"  --> doFloat
    , className=? "Clipster"  --> doRectFloat (W.RationalRect bigl bigt bigw bigh)
    , appName=? "floater"     --> doRectFloat (W.RationalRect l t w h)
    , appName=? "big-floater" --> doRectFloat (W.RationalRect bigl bigt bigw bigh)
    , appName=? "qute-editor" --> doRectFloat (W.RationalRect l t w h)]

-- Measurements used by Floating Windows
l = 0.05; bigl = 0.015  -- Distance from left edge
t = 0.3; bigt = 0.03  -- Distance from top edge
w = 0.9; bigw = 0.97  -- Total Width of Window
h = 0.3; bigh = 0.94  -- Total Height of Window

myStartupHook = ewmhDesktopsStartup
                >> setWMName "LG3D"
                >> spawn "init-bg"
                >> spawn "xrandr --output DisplayPort-1 --right-of DisplayPort-0"
                >> delayedSpawn 2 "calalrms"
                >> delayedSpawn 2 "xmonad-suntimes"
                >> delayedSpawn 2 "xmonad-volume"
                >> delayedSpawn 2 "xmonad-weather"
                >> spawn (xmobarTempFmt (getXmobarTemplate "1-bottom") ++ " -b --screen=1")
                >> spawn ("[[ $(x11screens) -ge 0 ]] && " ++ xmobarTempFmt (getXmobarTemplate "2-top") ++ " --screen=0")
                >> spawn ("[[ $(x11screens) -ge 0 ]] && " ++ xmobarTempFmt (getXmobarTemplate "2-bottom") ++ " -b --screen=0")
                >> spawn ("[[ $(x11screens) -ge 2 ]] && " ++ xmobarTempFmt (getXmobarTemplate "3-top") ++ " --screen=2")
                >> spawn ("[[ $(x11screens) -ge 2 ]] && " ++ xmobarTempFmt (getXmobarTemplate "3-bottom") ++ " -b --screen=2")

-------------------------------------------------------------------------------
-- KEY BINDING CONFIGS                                                       --
-------------------------------------------------------------------------------
alt = mod1Mask
super = mod4Mask
ctrl = controlMask
shift = shiftMask

------- Modifier Masks (mod1Mask: alt, mod4Mask: super)
--
-- The `alpha` and `beta` keys should be set to either 'super' or 'alt', depending on which
-- key you want as your primary meta key.
--
-- NOTE: I have used Xmodmap to swap the 'super' and 'alt' keys on my keyboard.  This has no effect
-- on this configuration (i.e. the alt key still corresponds to `mod1Mask`), but most other
-- programs will recognize 'super' as 'alt' and vice-versa.

-- KeyMask Aliases
alpha = alt
beta = super

myAdditionalKeys = [
   ---------- ALPHANUMERIC CHARACTERS ----------
   -- (you can sort these bindings with `<range>sort r /, [A-z]),/`)
   ((alpha, xK_0), do
           swapScreens "next"
           CW.nextScreen
     )
   , ((alpha .|. beta, xK_0), swapScreens "next")
   , ((alpha, xK_9), do
           swapScreens "prev"
           CW.prevScreen
     )
   , ((alpha .|. beta, xK_9), swapScreens "prev")
   , ((alpha, b), spawn "clipster_rofi_menu") -- clipmenu
   , ((alpha .|. beta, b), spawn "clipster_gtk")
   , ((alpha, c), launchApp "chat" "hexchat")
   , ((alpha, d), windows W.focusDown)
   , ((alpha, f), sendMessage $ Toggle TABBED)
   , ((alpha .|. beta, g), spawn "qb_prompt")
   , ((alpha, h), N2D.windowGo N2D.L True)
   , ((alpha .|. beta, h), sendMessage Shrink) -- Next Layout
   , ((alpha, i), launchApp "web" "qutebrowser --enable-webengine-inspector")
   , ((alpha, j), N2D.windowGo N2D.D True)
   , ((alpha .|. beta, j), sendMessage RT.MirrorShrink) -- Shrink Master Area
   , ((alpha, k), N2D.windowGo N2D.U True)
   , ((alpha .|. beta, k), sendMessage RT.MirrorExpand) -- Expand Master Area
   , ((alpha, l), N2D.windowGo N2D.R True)
   , ((alpha .|. beta, l), sendMessage Expand)
   , ((alpha .|. shift, l), spawn "my-screenlock") -- screenlock
   , ((alpha .|. ctrl, l), sendMessage NextLayout)
   , ((alpha, m), launchApp "mail" "thunderbird")
   , ((alpha .|. beta, m), do
           DW.addHiddenWorkspace "misc"
           windows $ W.shift "misc"
           removeEmptyWorkspaceAfter' $ windows $ W.view "misc"
     ) -- Shift current window to MISC
   , ((alpha .|. beta, n), sequence_ [DW.addWorkspacePrompt myXPConfig, DW.setWorkspaceIndex 1,
                           CW.toggleWS' ["NSP"], DW.withWorkspaceIndex W.shift 1,
                           removeEmptyWorkspaceAfter' $ DW.withWorkspaceIndex W.view 1]) -- Shift current window to _______
   , ((alpha, n), launchApp "notes" "tusk")
   , ((alpha .|. beta .|. shift, n), do
           ws_name <- io $ readFile "/tmp/xmonad.workspace"
           DW.addWorkspace ws_name
     )
   , ((alpha, o), CW.toggleWS' ["NSP"])
   , ((alpha .|. ctrl, o), spawn "zopen")
   , ((alpha, p), spawn ":")
   , ((alpha, q), spawn "qb_prompt")
   , ((alpha .|. ctrl, q), io (Exit.exitWith Exit.ExitSuccess))
   , ((alpha, r), spawn "killall xmobar; generate_xmobar_config; xmonad --recompile && xmonad --restart")
   , ((alpha .|. ctrl, r), DW.removeWorkspace)  -- Remove Current Workspace
   , ((alpha .|. shift, r), removeEmptyWorkspace') -- Remove Current Workspace if Empty
   , ((alpha .|. beta, s), windows W.swapDown) -- Swap Windows
   , ((alpha, t), spawn "DISPLAY=:0 new_enote_task") -- evernote (inbox)
   , ((alpha, u), windows W.focusUp)
   , ((alpha, w), spawn "close-window") -- Close Focused Window
   , ((alpha, x), launchApp "term" myTerminal)
   , ((alpha .|. beta, x), launchApp "term'" "urxvt -name primes -e zsh -c 'tm-init-prime'")
   , ((alpha, v), launchApp "mpv" "umpv")
   , ((alpha, z), launchApp "zath" "zathura")
   , ((alpha .|. beta, z), launchApp "zath'" "zcopy")

   ---------- KEYPAD CHARACTERS ----------
   , ((alpha, xK_KP_Add), spawn "next_task")
   , ((alpha, xK_KP_Begin), withFocused $ windows . W.sink)
   , ((alpha, xK_KP_Delete), spawn "twd")
   , ((alpha, xK_KP_Divide), spawn "wait_task -N")
   , ((alpha, xK_KP_Down), withFocused $ FK.keysMoveWindow (0, 100))
   , ((alpha .|. ctrl, xK_KP_Down), withFocused $ FK.keysResizeWindow (0, -50) (0, 1))
   , ((alpha, xK_KP_Enter), spawn "task start.any: done && task_refresh")
   , ((alpha, xK_KP_Insert), spawn "task start.any: stop && task_refresh")
   , ((alpha, xK_KP_Left), withFocused $ FK.keysMoveWindow (-100, 0))
   , ((alpha .|. ctrl, xK_KP_Left), withFocused $ FK.keysResizeWindow (-50, 0) (0, 0))
   , ((alpha, xK_KP_Multiply), spawn "wait_task -D 1h -N --purge")
   , ((alpha, xK_KP_Right), withFocused $ FK.keysMoveWindow (100, 0))
   , ((alpha .|. ctrl, xK_KP_Right), withFocused $ FK.keysResizeWindow (50, 0) (0, 0))
   , ((alpha, xK_KP_Subtract), spawn "last_task")
   , ((alpha, xK_KP_Up), withFocused $ FK.keysMoveWindow (0, -100))
   , ((alpha .|. ctrl, xK_KP_Up), withFocused $ FK.keysResizeWindow (0, 50) (0, 1))

   ---------- MISCELLANEOUS CHARACTERS ----------
   -- (you can sort these bindings with `:<range>sort r /K_[A-z]/`)
   , ((0, xF86XK_Calculator), NSP.namedScratchpadAction scratchpads "calculator") -- Scratchpad Calculator
   , ((0, xF86XK_AudioPlay), spawn "dbus-send --print-reply --dest=org.mpris.MediaPlayer2.spotify /org/mpris/MediaPlayer2 org.mpris.MediaPlayer2.Player.PlayPause")
   , ((0, xF86XK_AudioPrev), spawn "dbus-send --print-reply --dest=org.mpris.MediaPlayer2.spotify /org/mpris/MediaPlayer2 org.mpris.MediaPlayer2.Player.Previous")
   , ((0, xF86XK_AudioNext), spawn "dbus-send --print-reply --dest=org.mpris.MediaPlayer2.spotify /org/mpris/MediaPlayer2 org.mpris.MediaPlayer2.Player.Next")
   , ((alpha, xK_apostrophe), NSP.namedScratchpadAction scratchpads "spotify") -- Scratchpad Add Task to Inbox
   , ((alpha .|. beta .|. ctrl, xK_backslash), pushWindow)
   , ((alpha .|. beta .|. ctrl .|. shift, xK_backslash), CW.shiftNextScreen)
   , ((alpha, xK_bracketleft), CW.prevScreen)
   , ((beta, xK_bracketleft), windows W.focusUp)
   , ((alpha .|. beta, xK_bracketleft), do
           CW.nextScreen
           DW.moveTo CW.Prev (CW.WSIs hiddenNotNSP)
           CW.prevScreen
     ) -- Prev Hidden NonEmpty Workspace (viewed on non-active screen)
   , ((alpha, xK_bracketright), CW.nextScreen)
   , ((beta, xK_bracketright), windows W.focusDown)
   , ((alpha .|. beta, xK_bracketright), do
           CW.nextScreen
           DW.moveTo CW.Next (CW.WSIs hiddenNotNSP)
           CW.prevScreen
     ) -- Next Hidden NonEmpty Workspace (viewed on non-active screen)
   , ((alpha, xK_comma), do
           spawn "tmux -L GTD select-window -t0"
           NSP.namedScratchpadAction scratchpads "gtd"
     )
   , ((alpha .|. ctrl, xK_comma), do
           spawn "tmux -L GTD select-window -t2"
           NSP.namedScratchpadAction scratchpads "gtd"
     )
   , ((alpha, xK_equal), DW.moveTo CW.Next (CW.WSIs hiddenNotNSP)) -- Next Hidden NonEmpty Workspace
   , ((alpha .|. ctrl, xK_equal), spawn "set_volume 2%+")
   , ((alpha, xK_minus), spawn "wtoggle")
   , ((alpha .|. ctrl, xK_minus), spawn "set_volume 2%-")
   , ((alpha, xK_period), NSP.namedScratchpadAction scratchpads "scratchpad")
   , ((alpha, xK_Print), spawn "sshot") -- Screenshot
   , ((beta, xK_Print), spawn "saved_sshot") -- Saved Screenshot
   , ((alpha, xK_semicolon), spawn "shellPrompt")
   , ((alpha .|. beta, xK_semicolon), spawn "shellPrompt -L")
   , ((alpha, xK_slash), NSP.namedScratchpadAction scratchpads "calculator") -- Calculator Scratchpad
   , ((alpha .|. beta .|. ctrl, xK_slash), do
           pushWindow
           CW.nextScreen
     )
   , ((alpha .|. beta .|. ctrl .|. shift, xK_slash), do
           CW.shiftNextScreen
           CW.nextScreen
     )
   , ((alpha, xK_space), spawn "rofi -modi drun -show drun")
   , ((alpha, xK_Tab), do
           DW.addWorkspacePrompt myXPConfig
           DW.setWorkspaceIndex 1
           CW.toggleWS' ["NSP"]
           DW.withWorkspaceIndex W.shift 1
           removeEmptyWorkspaceAfter' $ DW.withWorkspaceIndex W.view 1
     )
   ]

   -- Shift to WS; then Focus WS
   ++ [((alpha, k), do
           withNthWorkspace' W.shift i
           withNthWorkspace' W.view i
       )
       | (i, k) <- zip [0..] [xK_1 .. xK_8]
      ]

   where
       a = xK_a; b = xK_b; c = xK_c; d = xK_d; e = xK_e; f = xK_f
       g = xK_g; h = xK_h; i = xK_i; j = xK_j; k = xK_k; l = xK_l
       m = xK_m; n = xK_n; o = xK_o; p = xK_p; q = xK_q; r = xK_r
       s = xK_s; t = xK_t; u = xK_u; v = xK_v; w = xK_w; x = xK_x
       y = xK_y; z = xK_z
