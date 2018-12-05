{-# LANGUAGE DeriveDataTypeable, TypeSynonymInstances, MultiParamTypeClasses #-}

import Data.Ratio
import Graphics.X11.ExtraTypes.XF86
import XMonad
import XMonad.Core
import XMonad.Layout
import XMonad.Layout.Accordion
import XMonad.Layout.Grid
import XMonad.Layout.ToggleLayouts
import XMonad.Layout.TwoPane
import XMonad.Layout.Tabbed
import XMonad.Layout.MultiToggle
import XMonad.Layout.NoBorders (noBorders,smartBorders,withBorder)
import XMonad.Layout.Minimize
import XMonad.Layout.Named
import XMonad.Layout.Gaps

import XMonad.Actions.SpawnOn (spawnOn,spawnHere,manageSpawn)
import XMonad.Config.Desktop (desktopConfig)
import XMonad.Hooks.EwmhDesktops (ewmh,ewmhDesktopsLogHook,ewmhDesktopsStartup)
import XMonad.Hooks.ManageHelpers (doRectFloat,doFullFloat)
import XMonad.Hooks.SetWMName (setWMName)
import XMonad.Layout.Spacing (smartSpacing)
import XMonad.Util.Run (spawnPipe,hPutStrLn)
import XMonad.Util.EZConfig (additionalKeys)
import XMonad.Util.WorkspaceCompare (getSortByIndex)

import qualified Control.Monad as Monad
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

-- hlint directives
{-# ANN module "HLint: ignore Evaluate" #-}

-----------------
--  Functions  --
-----------------
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

getXmobarTemplate :: String -> String
getXmobarTemplate "1-top-athena" = "%UnsafeStdinReader%    (%window_count%)}%timew%%xtimew%{ %pia%  %volume%  |  %date%"
getXmobarTemplate "1-top-aphrodite" = "%UnsafeStdinReader%    (%window_count%)}%timew%%xtimew%{ %pia%  %battery%  |  %volume%  |  %date%"
getXmobarTemplate "1-bottom" = "%cpu%  |  %memory%}%calevent%{%counter%%dynnetwork%"
getXmobarTemplate "2-top" = "}%weather%%xweather%     (☀ %suntimes%%xsuntimes%){"
getXmobarTemplate "2-bottom" = "}{"

removeEmptyWorkspaceAfter' :: X () -> X ()
removeEmptyWorkspaceAfter' f = do
    workspaceList <- gets (W.workspaces . windowset)
    let n = length workspaceList
    Monad.when (n > 3) $ DW.removeEmptyWorkspaceAfter f
    Monad.when (n <= 3) f

removeEmptyWorkspace' :: X ()
removeEmptyWorkspace' = do
    workspaceList <- gets (W.workspaces . windowset)
    let n = length workspaceList
    Monad.when (n > 3) DW.removeEmptyWorkspace

launchSeq :: String -> String -> [X ()]
launchSeq ws cmd = [DW.addWorkspace ws, spawnHere $ "hide_nsp && WS_is_Empty && " ++ cmd]

launchApp :: String -> String -> X ()
launchApp ws cmd = sequence_ $ launchSeq ws cmd

launchAppAndUP :: String -> String -> X ()
launchAppAndUP ws cmd = sequence_ $ UP.updatePointer (0.5, 0.5) (0, 0) : launchSeq ws cmd

launchFullApp :: String -> String -> X ()
launchFullApp ws cmd = launchApp ws ("xdotool key super+f && " ++ cmd)

-- Only shows layout when fullscreen mode is enabled
myPpOrder :: [String] -> [String]
myPpOrder (ws:l:t:_) = [ws]

strToUpper :: String -> String
strToUpper = map DataChar.toUpper

seqPush :: [X ()]
seqPush = [CW.swapNextScreen, CW.toggleWS' ["NSP"]]

seqSwap :: [X ()]
seqSwap = [removeEmptyWorkspace', CW.swapNextScreen, removeEmptyWorkspace']

pushDesktop :: String -> X ()
pushDesktop key = spawn $ "xmonad-scratch-bind " ++ key ++ " 0.15"

delayedSpawn :: Int -> String -> X ()
delayedSpawn seconds cmd = spawn $ "sleep " ++ show seconds ++ " && " ++ cmd

--------------------
--  Key Bindings  --
--------------------
------- Modifier Masks (mod1Mask: alt, mod4Mask: super)
--
-- The `alpha` and `beta` keys should be set to either 'super' or 'alt', depending on which
-- key you want as your primary meta key.
--
-- NOTE: I have used Xmodmap to swap the 'super' and 'alt' keys on my keyboard.  This has no effect
-- on this configuration (i.e. the alt key still corresponds to `mod1Mask`), but most other
-- programs will recognize 'super' as 'alt' and vice-versa.

-- KeyMask Aliases
alpha = mod1Mask
beta = mod4Mask
ctrl = controlMask
shift = shiftMask

myAdditionalKeys = [
   ---------- ALPHABETIC CHARACTERS ----------
   -- (you can sort these bindings with `<range>sort r /, [A-z]),/`)
   ((alpha, a), launchAppAndUP "ANKI" "anki")
   , ((alpha, b), spawn "clipster_rofi_menu") -- clipmenu
   , ((alpha .|. beta, b), spawn "clipster_gtk")
   , ((alpha, c), launchApp "WEB" "qutebrowser --enable-webengine-inspector")
   , ((alpha, d), windows W.focusDown)
   , ((alpha .|. shift, d), spawn "tmux -L $(tm-socket) kill-window")
   , ((alpha, e), spawn "tm-send --action=clear") -- clear screen
   , ((alpha, f), sendMessage $ XMonad.Layout.MultiToggle.Toggle TABBED)
   , ((alpha, g), spawn "qb_prompt --next-screen")
   , ((alpha .|. beta, g), spawn "qb_prompt")
   , ((alpha, h), sequence_ [N2D.windowGo N2D.L False])
   , ((alpha .|. ctrl, h), spawn "tm-send --action 'cd $(tm-session-root --get $(tmux display-message -p \"#{session_name}\")) && ll'")  -- cd to Tmuxinator Project Root
   , ((alpha .|. beta, h), sendMessage Shrink) -- Next Layout
   , ((alpha .|. shift, h), spawn "tm-send --action 'cd \"$(tm-window-root $(tmux display-message -p \"#{session_name} #{window_index}\"))\" && ll'")  -- cd to Tmuxinator Window-Specific Root
   , ((alpha .|. beta .|. shift, h), spawn "tm-send --action 'tm-window-root $(tmux display-message -p \"#{session_name} #{window_index}\") -s \"$(pwd)\" && ll'")  -- set Tmuxinator Window-Specific Root
   , ((alpha, j), sequence_ [N2D.windowGo N2D.D False])
   , ((alpha .|. beta, j), sendMessage RT.MirrorShrink) -- Shrink Master Area
   , ((alpha, k), sequence_ [N2D.windowGo N2D.U False])
   , ((alpha .|. beta, k), sendMessage RT.MirrorExpand) -- Expand Master Area
   , ((alpha .|. shift, k), spawn "tm-kill")
   , ((alpha .|. ctrl, k), spawn "tmux copy-mode")
   , ((alpha, l), sequence_ [N2D.windowGo N2D.R False])
   , ((alpha .|. beta, l), sendMessage Expand)
   , ((alpha .|. shift, l), spawn "my-screenlock") -- screenlock
   , ((alpha .|. ctrl, l), sendMessage NextLayout)
   , ((alpha, m), sequence_ [DW.addHiddenWorkspace "MISC", windows $ W.shift "MISC", removeEmptyWorkspaceAfter' $ windows $ W.view "MISC"]) -- Shift current window to MISC
   , ((alpha .|. beta, m), spawn "toggle_monitor && sleep 1 && killall xmobar; xmonad --restart") -- Toggle External Monitor
   , ((alpha .|. shift, m), sequence_ $ [DW.addHiddenWorkspace "MPV", windows $ W.shift "MPV", removeEmptyWorkspaceAfter' $ windows $ W.view "MPV"] ++ seqPush)
   , ((alpha, n), spawn "tmux -L $(tm-socket) next-window") -- Tmux Next
   , ((alpha .|. beta, n), sequence_ [DW.addWorkspacePrompt myXPConfig, DW.setWorkspaceIndex 1,
                           CW.toggleWS' ["NSP"], DW.withWorkspaceIndex W.shift 1,
                           removeEmptyWorkspaceAfter' $ DW.withWorkspaceIndex W.view 1]) -- Shift current window to _______
   , ((alpha .|. shift, n), spawn "tm-new-window")
   , ((alpha, o), launchApp "OKULAR" "okular & new_okular_instance && zopen") -- Open New Book in Zathura
   , ((alpha .|. ctrl, o), spawn "zopen")
   , ((alpha, p), spawn "tmux -L $(tm-socket) previous-window") -- Tmux Previous
   , ((alpha .|. beta, p), spawn "PIA") -- Toggle PIA
   , ((alpha .|. shift, p), spawn "pause_task")
   , ((alpha, q), spawn "tm-send --action=quit") -- Quit Screen
   , ((alpha .|. ctrl, q), io (Exit.exitWith Exit.ExitSuccess))
   , ((alpha, r), spawn "killall xmobar; generate_xmobar_config; xmonad --recompile && xmonad --restart") -- Restarts XMonad
   , ((alpha .|. ctrl, r), DW.removeWorkspace)  -- Remove Current Workspace
   , ((alpha .|. shift, r), removeEmptyWorkspace') -- Remove Current Workspace if Empty
   , ((alpha .|. beta .|. ctrl, r), spawn "confirm --dmenu 'sudo reboot'") -- Restart
   , ((alpha, s), sequence_ seqSwap) -- Swap
   , ((alpha .|. beta, s), sequence_ $ seqSwap ++ [CW.nextScreen]) -- Swap and Follow
   , ((alpha .|. shift, s), windows W.swapDown)    -- Shift Local
   , ((alpha .|. beta .|. ctrl, s), spawn "confirm --dmenu 'smart_shutdown'") -- Shutdown
   , ((alpha, t), spawn "prompt 'Inbox' -format 'q' | xargs task add +inbox | tail -1 | xargs -I _ notify-send -u low _") -- taskwarrior (inbox)
   , ((alpha .|. beta, t), spawn "prompt 'Due Today' -format \"'q'\" | xargs task add +today | tail -1 | xargs -I _ notify-send -u low _ && task_refresh") -- taskwarrior (due today)
   , ((alpha .|. shift, t), spawn "task_hotstart")
   , ((alpha, u), windows W.focusUp)
   , ((alpha, v), launchApp "MPV" "mpvlc")
   , ((alpha .|. shift, v), launchApp "VIDEOSTREAM" "/opt/google/chrome/google-chrome --profile-directory=Default --app-id=cnciopoikihiagdjbjpnocolokfelagl")
   , ((alpha .|. shift, v), NSP.namedScratchpadAction scratchpads "videostream")
   , ((alpha, w), spawn "close-window") -- Close Focused Window
   , ((alpha, x), launchApp "TERM" myTerminal)
   , ((alpha .|. beta, x), launchApp "TERM'" "urxvt -name primes -e zsh -c 'tm-init-prime'")
   , ((alpha, z), launchFullApp "ZATH" "zathura")
   , ((alpha .|. beta, z), launchApp "ZATH'" "zathura")
   , ((alpha .|. shift, z), launchApp "ZEAL" "zeal")
   , ((alpha .|. beta .|. shift, z), sequence_ [CW.nextScreen, launchApp "ZATH'" "zsearch --new-instance --no-search"])

   ---------- NUMERIC CHARACTERS ----------
   , ((alpha, xK_0), spawn "tmux -L $(tm-socket) switchc -n") -- Tmux Next Session
   , ((alpha, xK_9), spawn "tmux -L $(tm-socket) switchc -p") -- Tmux Previous Session

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
   , ((alpha, xK_apostrophe), NSP.namedScratchpadAction scratchpads "weechat") -- Scratchpad Add Task to Inbox
   , ((alpha, xK_backslash), CW.nextScreen) -- Next Screen
   , ((alpha .|. beta, xK_backslash), pushDesktop "backslash")
   , ((alpha .|. beta .|. ctrl, xK_backslash), sequence_ seqPush)
   , ((alpha .|. beta .|. ctrl .|. shift, xK_backslash), CW.shiftNextScreen)
   , ((alpha, xK_BackSpace), spawn "tmux -L $(tm-socket) kill-window")
   , ((alpha, xK_bracketleft), sequence_ [DW.moveTo CW.Prev (CW.WSIs hiddenNotNSP)]) -- Prev Hidden NonEmpty Workspace
   , ((alpha .|. beta, xK_bracketleft), sequence_ [CW.nextScreen, DW.moveTo CW.Prev (CW.WSIs hiddenNotNSP), CW.prevScreen]) -- Prev Hidden NonEmpty Workspace (viewed on non-active screen)
   , ((alpha, xK_bracketright), sequence_ [DW.moveTo CW.Next (CW.WSIs hiddenNotNSP)]) -- Next Hidden NonEmpty Workspace
   , ((alpha .|. beta, xK_bracketright), sequence_ [CW.nextScreen, DW.moveTo CW.Next (CW.WSIs hiddenNotNSP), CW.prevScreen]) -- Next Hidden NonEmpty Workspace (viewed on non-active screen)
   , ((alpha, xK_comma), sequence_ [spawn "task_refresh", NSP.namedScratchpadAction scratchpads "gtd"])
   , ((alpha .|. beta, xK_comma), sequence_ [NSP.namedScratchpadAction scratchpads "gtd"])
   , ((alpha, xK_equal), spawn "tm-send --action='cd $(popu); ll'") -- cd to Next Dir
   , ((alpha, xK_minus), spawn "tm-send --action='pushu && popd; ll'") -- cd to Last Dir
   , ((alpha, xK_period), sequence_ [NSP.namedScratchpadAction scratchpads "scratchpad"])
   , ((alpha, xK_Print), spawn "sshot") -- Screenshot
   , ((beta, xK_Print), spawn "receipt_sshot") -- Screenshot (saved as receipt)
   , ((alpha, xK_semicolon), spawn "shellPrompt")
   , ((alpha .|. beta, xK_semicolon), spawn "shellPrompt -L")
   , ((alpha, xK_slash), NSP.namedScratchpadAction scratchpads "calculator") -- Calculator Scratchpad
   , ((alpha .|. beta, xK_slash), pushDesktop "slash")
   , ((alpha .|. beta .|. ctrl, xK_slash), sequence_ $ seqPush ++ [CW.nextScreen])
   , ((alpha .|. beta .|. ctrl .|. shift, xK_slash), sequence_ [CW.shiftNextScreen, CW.nextScreen])
   , ((alpha, xK_space), spawn "rofi -modi drun -show drun") -- Program Launcher
   , ((alpha .|. beta, xK_space), sequence_ [DW.addWorkspace "MISC", spawn "rofi -modi drun -show drun"]) -- Program Launcher (MISC)
   , ((alpha, xK_Tab), CW.toggleWS' ["NSP"]) -- Toggle to Last Workspace
   ]

   -- Shift to WS; then Focus WS
   ++ [((alpha .|. beta, k), sequence_ [withNthWorkspace' W.shift i, withNthWorkspace' W.view i])
       | (i, k) <- zip [0..7] [xK_1 .. xK_9]
      ]

   where
       a = xK_a; b = xK_b; c = xK_c; d = xK_d; e = xK_e; f = xK_f
       g = xK_g; h = xK_h; i = xK_i; j = xK_j; k = xK_k; l = xK_l
       m = xK_m; n = xK_n; o = xK_o; p = xK_p; q = xK_q; r = xK_r
       s = xK_s; t = xK_t; u = xK_u; v = xK_v; w = xK_w; x = xK_x
       y = xK_y; z = xK_z

----------------------
--  Layout Options  --
----------------------
myFull = named "TS" $ smartBorders simpleTabbed

-- Transformers (W+f)
data TABBED = TABBED deriving (Read, Show, Eq, Typeable)
instance Transformer TABBED Window where
	transform TABBED x k = k myFull (const x)

myLayout = id
    $ mkToggle (single TABBED)
    $ TwoPane (3/100) (1/2) ||| Grid

------------------------------------
--  Miscellaneous Configurations  --
------------------------------------
myFont = "xft:Source Code Pro"
myTerminal = "urxvt -e zsh -c 'tm Terminal'"

myFocusFollowsMouse = False
myClickJustFocuses = False

myBorderWidth = 5
myFocusedBorderColor = "#0000FF"

myWorkspaces :: [String]
myWorkspaces = ["NSP", "TERM","WEB"]

myXPConfig :: P.XPConfig
myXPConfig = P.def {
  P.font = myFont,
  P.position = P.CenteredAt 0.2 0.4
}

-- Measurements used by Floating Windows
l = 0.05; bigl = 0.015  -- Distance from left edge
t = 0.3; bigt = 0.03  -- Distance from top edge
w = 0.9; bigw = 0.97  -- Total Width of Window
h = 0.3; bigh = 0.94  -- Total Height of Window

scratchpads = [ NSP.NS "scratchpad" scratchpad (appName =? "scratchpad") 
                    (NSP.customFloating $ W.RationalRect bigl bigt bigw bigh)
              , NSP.NS "calculator" calculator (appName =? "calculator")
                    (NSP.customFloating $ W.RationalRect l t w h)
              , NSP.NS "weechat" weechat (appName =? "weechat")
                    (NSP.customFloating $ W.RationalRect bigl bigt bigw bigh)
              , NSP.NS "gtd" gtd (appName =? "GTD")
                    (NSP.customFloating $ W.RationalRect bigl bigt bigw bigh)
              , NSP.NS "videostream" videostream (appName =? "crx_cnciopoikihiagdjbjpnocolokfelagl")
                    (NSP.customFloating $ W.RationalRect bigl bigt bigw bigh) ]
            where 
                calculator = "urxvt -name calculator -e zsh -c 'bc -l'"
                scratchpad = "urxvt -name scratchpad -e zsh -c 'tmuxinator start ScratchPad root=$(tm-session-root --get ScratchPad)'"
                weechat = "weechat-launcher"
                gtd = "urxvt -name GTD -e zsh -c 'tmuxinator start GTD root=$(tm-session-root --get GTD)'"
                videostream = "/opt/google/chrome/google-chrome --profile-directory=Default --app-id=cnciopoikihiagdjbjpnocolokfelagl"

myManageHook = composeAll
    [ manageSpawn
    , NSP.namedScratchpadManageHook scratchpads
    , appName=? "peek"        --> doFloat
    , className=? "Pinentry-gtk-2"  --> doFloat
    , appName=? "calculator"  --> doRectFloat (W.RationalRect l t w h)
    , className=? "Clipster"  --> doRectFloat (W.RationalRect bigl bigt bigw bigh)
    , appName=? "floater"     --> doRectFloat (W.RationalRect l t w h)
    , appName=? "big-floater" --> doRectFloat (W.RationalRect bigl bigt bigw bigh)
    , appName=? "qute-editor" --> doRectFloat (W.RationalRect l t w h)]


myStartupHook = ewmhDesktopsStartup
                >> setWMName "LG3D"
                >> spawn "init-bg"
                >> spawn "xrandr --output HDMI2 --auto --rotate right"
                >> delayedSpawn 2 "calalrms"
                >> delayedSpawn 2 "xmonad-suntimes"
                >> delayedSpawn 2 "xmonad-volume"
                >> delayedSpawn 2 "xmonad-weather"
                >> delayedSpawn 2 "xmonad-timew"
                >> delayedSpawn 5 "emanage -m"
                >> spawn (xmobarTempFmt (getXmobarTemplate "1-bottom") ++ " -b --screen=2")
                >> spawn ("[[ $(x11screens) -ge 2 ]] && " ++ xmobarTempFmt (getXmobarTemplate "2-top") ++ " --screen=1")
                >> spawn ("[[ $(x11screens) -ge 2 ]] && " ++ xmobarTempFmt (getXmobarTemplate "2-bottom") ++ " -b --screen=1")

------------
--  Main  --
------------
main :: IO ()
main = do
    hostname <- HostName.getHostName
    xmproc <- spawnPipe (xmobarTempFmt (getXmobarTemplate $ "1-top-" ++ hostname) ++ " --screen=2")
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
            , DL.ppSort                  = (NSP.namedScratchpadFilterOutWorkspace .) `Monad.liftM` DW.getSortByOrder
            } >> ewmhDesktopsLogHook <+> DL.dynamicLogXinerama
      } `additionalKeys` myAdditionalKeys
