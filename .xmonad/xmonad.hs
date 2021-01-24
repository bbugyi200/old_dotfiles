{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances  #-}

-------------------------------------------------------------------------------
-- IMPORTED LIBRARIES                                                        --
-------------------------------------------------------------------------------
import           Control.Monad
import           Data.Ratio
import           Graphics.X11.ExtraTypes.XF86
import           XMonad
import           XMonad.Core
import           XMonad.Layout
import           XMonad.Layout.Grid
import           XMonad.Layout.MultiToggle
import           XMonad.Layout.Tabbed
import           XMonad.Layout.TwoPane

import           XMonad.Actions.SpawnOn               (manageSpawn, spawnHere,
                                                       spawnOn)
import           XMonad.Config.Desktop                (desktopConfig)
import           XMonad.Hooks.EwmhDesktops            (ewmh,
                                                       ewmhDesktopsLogHook,
                                                       ewmhDesktopsStartup)
import           XMonad.Hooks.ManageHelpers           (doFullFloat, doRectFloat)
import           XMonad.Hooks.SetWMName               (setWMName)
import           XMonad.Layout.NoBorders              (smartBorders)
import           XMonad.Layout.Spacing                (smartSpacing)
import           XMonad.Util.EZConfig                 (additionalKeys)
import           XMonad.Util.Run                      (hPutStrLn, spawnPipe)
import           XMonad.Util.WorkspaceCompare         (getSortByIndex)

import qualified Data.Char                            as DataChar
import qualified Network.HostName                     as HostName
import qualified System.Exit                          as Exit
import qualified XMonad.Actions.CycleWS               as CW
import qualified XMonad.Actions.DynamicWorkspaceOrder as DW
import qualified XMonad.Actions.DynamicWorkspaces     as DW
import qualified XMonad.Actions.FloatKeys             as FK
import qualified XMonad.Actions.GroupNavigation       as GNav
import qualified XMonad.Actions.Navigation2D          as N2D
import qualified XMonad.Hooks.DynamicLog              as DL
import qualified XMonad.Hooks.ManageDocks             as Docks
import qualified XMonad.Layout.ResizableTile          as RT
import qualified XMonad.Prompt                        as P
import qualified XMonad.StackSet                      as W
import qualified XMonad.Util.NamedScratchpad          as NSP

-- hlint ignore directives
{-# ANN module "HLint: ignore Evaluate" #-}

-------------------------------------------------------------------------------
-- XMobar Templates                                                          --
-------------------------------------------------------------------------------
getXmobarTemplate :: String -> String
getXmobarTemplate "C-top-athena" = "%UnsafeStdinReader%}{ %pia%  %volume%  |  %date%"
getXmobarTemplate "C-top-aphrodite" = "%UnsafeStdinReader%    (%window_count%)}{ %pia%  %battery%  |  %volume%  |  %date%"
getXmobarTemplate "C-bottom" = "%cpu%   |   %memory%   |   %dynnetwork%}%calevent%{%counter% "
getXmobarTemplate "L-top" = "}%weather%      [%suntimes%]{"
getXmobarTemplate "L-bottom" = "}{"
getXmobarTemplate "R-top" = "}{"
getXmobarTemplate "R-bottom" = "}{"

-------------------------------------------------------------------------------
-- MAIN                                                                      --
-------------------------------------------------------------------------------
main :: IO ()
main = do
    hostname <- HostName.getHostName
    xmproc <- spawnPipe (xmobarTempFmt (getXmobarTemplate $ "C-top-" ++ hostname) ++ " --screen=1")
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
            } >> GNav.historyHook >> ewmhDesktopsLogHook <+> DL.dynamicLogXinerama
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
    when (n > 3) DW.removeEmptyWorkspace

launchApp :: String -> String -> String -> X ()
launchApp ws window_name cmd = do
    DW.addWorkspace ws
    when (window_name == "") (
        spawnHere $ "hide_nsp && WS_is_Empty && " ++ cmd
        )
    when (window_name /= "") (
        spawnHere $ "hide_nsp && window_not_active '" ++ window_name ++ "' && " ++ cmd
        )

-- Only shows layout when fullscreen mode is enabled
myPpOrder :: [String] -> [String]
myPpOrder (ws:l:t:_) = [ws]

strToUpper :: String -> String
strToUpper = map DataChar.toUpper

pushWindow :: X ()
pushWindow = do
    swapNextScreen
    CW.toggleWS' ["NSP"]

swapScreens :: String -> X ()
swapScreens dir = do
    removeEmptyWorkspace'
    if dir == "next"
        then swapNextScreen
    else
        swapPrevScreen
    removeEmptyWorkspace'

delayedSpawn :: Int -> String -> X ()
delayedSpawn seconds cmd = spawn $ "sleep " ++ show seconds ++ " && " ++ cmd

-- Usage:  goToLastMonitor "S <TARGET_SCREEN_ID>" "S <CURRENT_SCREEN_ID>"
goToLastMonitor :: String -> String -> X ()
goToLastMonitor "S 0" "S 1" = do
    swapScreens "prev"
    prevScreen
goToLastMonitor "S 1" "S 2" = goToLastMonitor "S 0" "S 1"
goToLastMonitor "S 2" "S 0" = goToLastMonitor "S 0" "S 1"
goToLastMonitor _ _ = do
    swapScreens "next"
    nextScreen

-- I have no idea why it is necessary to sometimes swap these, but it is...
swapNextScreen = CW.swapNextScreen
swapPrevScreen = CW.swapPrevScreen
nextScreen = CW.nextScreen
prevScreen = CW.prevScreen

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
    $ myFull ||| TwoPane (3/100) (1/2) ||| Grid

-------------------------------------------------------------------------------
-- MISCELLANEOUS CONFIGS                                                     --
-------------------------------------------------------------------------------
myFont = "xft:Source Code Pro"
myTerminal = "tm-init"

myFocusFollowsMouse = False
myClickJustFocuses = False

myBorderWidth = 7
myFocusedBorderColor = "#0000FF"

myWorkspaces :: [String]
myWorkspaces = ["NSP", "term","web", "doc"]

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
                    NSP.nonFloating
              , NSP.NS "conky" conky (appName =? "Conky")
                    NSP.nonFloating ]
            where
                calculator = "urxvt -name calculator -e zsh -c 'wtitle Calculator && eva'"
                scratchpad = "scratchpad-launcher"
                spotify = "spotify"
                gtd = "gtd-launcher"
                conky = "conky"

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
                >> spawn (xmobarTempFmt (getXmobarTemplate "C-bottom") ++ " -b --screen=1")
                >> spawn (xmobarTempFmt (getXmobarTemplate "L-top") ++ " --screen=0")
                >> spawn (xmobarTempFmt (getXmobarTemplate "L-bottom") ++ " -b --screen=0")
                >> spawn (xmobarTempFmt (getXmobarTemplate "R-top") ++ " --screen=2")
                >> spawn (xmobarTempFmt (getXmobarTemplate "R-bottom") ++ " -b --screen=2")
                >> delayedSpawn 2 "emanage -m -M 7"
                >> delayedSpawn 2 "external_backup_check"
                >> delayedSpawn 2 "calalrms"
                >> delayedSpawn 2 "xmonad-volume"
                >> delayedSpawn 2 "sudo killall -9 xmonad-weather; sudo killall -9 poll-weather; xmonad-weather --init"
                >> delayedSpawn 2 "xmonad-suntimes"
                >> delayedSpawn 2 "/usr/bin/x11vnc -rfbauth /home/bryan/.vnc/passwd -rfbport 34590 -display :0 -o /var/tmp/x11vnc.log -bg -forever -many -usepw -auth /home/bryan/.Xauthority"

-------------------------------------------------------------------------------
-- KEY BINDING CONFIGS                                                       --
-------------------------------------------------------------------------------
------------ Modifier Masks
--
-- Mask Aliases
alt = mod1Mask
super = mod4Mask
ctrl = controlMask
shift = shiftMask
--
-- Mask Variables
alpha = super
beta = alt
chi = ctrl
delta = shift
---------------------------

myAdditionalKeys = [
   ---------- ALPHANUMERIC CHARACTERS ----------
   -- (you can sort these bindings with `<range>sort r /, [A-z]),/`)
   ((alpha, xK_0), do
           swapScreens "next"
           nextScreen
     )
   , ((alpha .|. beta, xK_0), swapScreens "next")
   , ((alpha .|. chi, xK_0), spawn "xkey Down")
   , ((alpha, xK_9), do
           swapScreens "prev"
           prevScreen
     )
   , ((alpha .|. beta, xK_9), swapScreens "prev")
   , ((alpha .|. chi, xK_9), spawn "xkey Up")
   , ((alpha, b), spawn "clipster_rofi_menu") -- clipmenu
   , ((alpha .|. beta, b), spawn "clipster -s")
   , ((alpha .|. chi, b), launchApp "bba" "" "google-chrome-stable https://bba.bloomberg.net")
   , ((alpha, c), do
            spawn "wmctrl -a Calendar"
            launchApp "cal" "Calendar" "$(firefox_exe) --new-window https://calendar.google.com/calendar/u/0/r/month?pli=1 &"
    )
   , ((alpha, d), windows W.focusDown)
   , ((alpha, e), spawn "xkey End")
   , ((alpha, f), launchApp "fox" "" "$(firefox_exe) --new-window https://google.com &")
   , ((alpha .|. beta, f), sendMessage $ Toggle TABBED)
   , ((alpha, g), do
           spawn "wmctrl -a chrome"
           launchApp "gc" "google-chrome" "google-chrome-stable"
     )
   , ((alpha, h), prevScreen)
   , ((alpha .|. chi, h), sendMessage Shrink)
   , ((alpha, i), do
           spawn "bash -c '[[ $(wmctrl -lx | grep \"qutebrowser\" | wc -l) -lt 2 ]] && wmctrl -a qutebrowser'"
           launchApp "web" "qutebrowser" "qutebrowser --enable-webengine-inspector"
     )
   , ((alpha, j), N2D.windowGo N2D.D True)
   , ((alpha .|. beta, j), sendMessage RT.MirrorShrink) -- Shrink Master Area
   , ((alpha, k), N2D.windowGo N2D.U True)
   , ((alpha .|. beta, k), sendMessage RT.MirrorExpand) -- Expand Master Area
   , ((alpha, l), nextScreen)
   , ((alpha .|. beta, l), sendMessage NextLayout)
   , ((alpha .|. chi, l), sendMessage Expand)
   , ((alpha .|. beta .|. chi, l), spawn "my-screenlock --no-blur")
   , ((alpha, m), launchApp "mac" "" "vncviewer C02DR3Z2MD6R")
   , ((alpha .|. beta, m), do
            spawn "wmctrl -a Messages"
            launchApp "msg" "Messages" "$(firefox_exe) --new-window https://messages.google.com/web/conversations &"
    )
   , ((alpha .|. chi, m), do
            spawn "wmctrl -a Gmail"
            launchApp "mail" "Gmail" "init-mail"
    )
   , ((alpha, n), launchApp "notes" "" "nixnote2")
   , ((alpha .|. beta .|. delta, n), do
           ws_name <- io $ readFile "/tmp/xmonad.workspace"
           DW.addWorkspace ws_name
     )
   , ((alpha, o), do
           orig_sid <- gets (W.screen . W.current . windowset)
           GNav.nextMatch GNav.History (return True)
           new_sid <- gets (W.screen . W.current . windowset)
           when (orig_sid /= new_sid) $ goToLastMonitor (show orig_sid) (show new_sid)
     )
   , ((alpha, p), launchApp "dev" "" "mkdvtm es-prod")
   , ((alpha, q), spawn "qb_prompt")
   , ((alpha .|. beta .|. chi, q), do
           spawn "sync"
           io (Exit.exitWith Exit.ExitSuccess)
     )
   , ((alpha, r), spawn "killall xmobar; generate_xmobar_config; xmonad --recompile && xmonad --restart")
   , ((alpha .|. chi, r), DW.removeWorkspace)  -- Remove Current Workspace
   , ((alpha .|. delta, r), removeEmptyWorkspace') -- Remove Current Workspace if Empty
   , ((alpha, s), do
            spawn "wmctrl -a Slack || wmctrl -a 'You have new messages'"
            launchApp "slack" "" "init-slack"
    )
   , ((alpha .|. beta, s), do
            spawn "wmctrl -a 'System Information'"
            launchApp "stat" "hardinfo" "hardinfo"
    )
   , ((alpha .|. chi, s), windows W.swapDown) -- Swap Windows
   , ((alpha, t), spawn "new_enote_task") -- evernote (inbox)
   , ((alpha, u), windows W.focusUp)
   , ((alpha, w), spawn "close-window") -- Close Focused Window
   , ((alpha .|. beta, w), do
            spawn "wmctrl -a 'Capturing'"
            launchApp "stat" "wireshark" "sudo wireshark"
    )
   , ((alpha, x), launchApp "term" "" myTerminal)
   , ((alpha .|. beta, x), launchApp "term'" "" "alacritty -t tmux_primes -e zsh -c 'tm-init-prime tmux_primes'")
   , ((alpha, v), launchApp "mpv" "" "umpv")
   , ((alpha, z), launchApp "doc" "" "zathura")
   , ((alpha .|. beta, z), launchApp "doc'" "" "zcopy")

   ---------- KEYPAD CHARACTERS ----------
   , ((alpha, xK_KP_Begin), withFocused $ windows . W.sink)
   , ((alpha, xK_KP_Down), withFocused $ FK.keysMoveWindow (0, 100))
   , ((alpha .|. chi, xK_KP_Down), withFocused $ FK.keysResizeWindow (0, -50) (0, 1))
   , ((alpha, xK_KP_Left), withFocused $ FK.keysMoveWindow (-100, 0))
   , ((alpha .|. chi, xK_KP_Left), withFocused $ FK.keysResizeWindow (-50, 0) (0, 0))
   , ((alpha, xK_KP_Right), withFocused $ FK.keysMoveWindow (100, 0))
   , ((alpha .|. chi, xK_KP_Right), withFocused $ FK.keysResizeWindow (50, 0) (0, 0))
   , ((alpha, xK_KP_Up), withFocused $ FK.keysMoveWindow (0, -100))
   , ((alpha .|. chi, xK_KP_Up), withFocused $ FK.keysResizeWindow (0, 50) (0, 1))

   ---------- MISCELLANEOUS CHARACTERS ----------
   -- (you can sort these bindings with `:<range>sort r /K_[A-z]/`)
   , ((0, xF86XK_Calculator), NSP.namedScratchpadAction scratchpads "calculator") -- Scratchpad Calculator
   , ((0, xF86XK_AudioPlay), spawn "dbus-send --print-reply --dest=org.mpris.MediaPlayer2.spotify /org/mpris/MediaPlayer2 org.mpris.MediaPlayer2.Player.PlayPause")
   , ((0, xF86XK_AudioPrev), spawn "dbus-send --print-reply --dest=org.mpris.MediaPlayer2.spotify /org/mpris/MediaPlayer2 org.mpris.MediaPlayer2.Player.Previous")
   , ((0, xF86XK_AudioNext), spawn "dbus-send --print-reply --dest=org.mpris.MediaPlayer2.spotify /org/mpris/MediaPlayer2 org.mpris.MediaPlayer2.Player.Next")
   , ((alpha, xK_apostrophe), NSP.namedScratchpadAction scratchpads "spotify") -- Scratchpad Add Task to Inbox
   , ((alpha, xK_backslash), DW.moveTo CW.Next (CW.WSIs hiddenNotNSP))
   , ((alpha .|. beta .|. chi, xK_backslash), pushWindow)
   , ((alpha .|. beta .|. chi .|. delta, xK_backslash), CW.shiftNextScreen)
   , ((alpha, xK_bracketleft), DW.moveTo CW.Prev (CW.WSIs hiddenNotNSP))
   , ((alpha .|. beta, xK_bracketleft), do
           prevScreen
           DW.moveTo CW.Next (CW.WSIs hiddenNotNSP)
           nextScreen
     ) -- Prev Hidden NonEmpty Workspace (viewed on non-active screen)
   , ((alpha, xK_bracketright), DW.moveTo CW.Next (CW.WSIs hiddenNotNSP))
   , ((alpha .|. beta, xK_bracketright), do
           nextScreen
           DW.moveTo CW.Next (CW.WSIs hiddenNotNSP)
           prevScreen
     ) -- Next Hidden NonEmpty Workspace (viewed on non-active screen)
   , ((alpha, xK_comma), NSP.namedScratchpadAction scratchpads "conky")
   , ((alpha, xK_equal), spawn "set_volume 2%+")
   , ((alpha, xK_minus), spawn "set_volume 2%-")
   , ((alpha .|. chi, xK_minus), spawn "xkey Delete")
   , ((alpha, xK_period), NSP.namedScratchpadAction scratchpads "scratchpad")
   , ((alpha, xK_Print), spawn "sshot") -- Screenshot
   , ((alpha .|. beta, xK_Print), spawn "saved_sshot") -- Save Screenshot to File
   , ((alpha .|. chi, xK_Print), spawn "print_sshot") -- Print Screenshot
   , ((alpha, xK_semicolon), spawn "shellPrompt")
   , ((alpha .|. beta, xK_semicolon), spawn "shellPrompt -L")
   , ((alpha, xK_slash), NSP.namedScratchpadAction scratchpads "calculator") -- Calculator Scratchpad
   , ((alpha .|. beta .|. chi, xK_slash), do
           pushWindow
           nextScreen
     )
   , ((alpha .|. beta .|. chi .|. delta, xK_slash), do
           CW.shiftNextScreen
           nextScreen
     )
   , ((alpha, xK_space), spawn "rofi -modi drun -show drun")
   , ((alpha, xK_Tab), sequence_ [
           DW.setWorkspaceIndex 1,
           DW.addWorkspacePrompt myXPConfig,
           DW.setWorkspaceIndex 2,
           DW.withWorkspaceIndex W.view 1,
           DW.withWorkspaceIndex W.shift 2,
           removeEmptyWorkspaceAfter' $ DW.withWorkspaceIndex W.view 2
       ]
     ) -- Shift current window to _______
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
