-------------------------------------------------------------------------------
-------------------------------- Imports --------------------------------------
import XMonad
import XMonad.Actions.SpawnOn (spawnOn,spawnHere,manageSpawn)
import XMonad.Hooks.SetWMName (setWMName)
import XMonad.Config.Desktop (desktopConfig)
import XMonad.Hooks.ManageDocks (avoidStruts)
import XMonad.Util.Run (spawnPipe,hPutStrLn)
import XMonad.Util.EZConfig (additionalKeys)
import XMonad.Layout.Spacing (smartSpacing)
import XMonad.Util.WorkspaceCompare (getSortByIndex)
import XMonad.Hooks.EwmhDesktops (ewmh,ewmhDesktopsLogHook,ewmhDesktopsStartup)
import XMonad.Hooks.ManageHelpers (doRectFloat,doFullFloat)

import Data.Maybe (isNothing,isJust)
import Control.Monad (liftM,when)
import Network.HostName (getHostName)

import Graphics.X11.ExtraTypes.XF86

import qualified XMonad.StackSet as W
import qualified XMonad.Prompt as P
import qualified XMonad.Util.NamedScratchpad as NSP
import qualified XMonad.Hooks.DynamicLog as DL
import qualified XMonad.Actions.CycleWS as CW
import qualified XMonad.Actions.DynamicWorkspaces as DW
import qualified XMonad.Actions.DynamicWorkspaceOrder as DW
import qualified XMonad.Layout.ResizableTile as RT
import qualified XMonad.Actions.Navigation2D as N2D

---------------------------------- Functions ----------------------------------
-- Function that prevents cycling to workspaces available on other screens
hiddenNotNSP :: X (WindowSpace -> Bool)
hiddenNotNSP = do
  sort <- DW.getSortByOrder
  hs <- gets (map W.tag . sort . NSP.namedScratchpadFilterOutWorkspace . W.hidden . windowset)
  return (\w -> (W.tag w) `elem` hs)

-- | This is a re-implementation of DW.withNthworkspace with "skipTags"
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
getXmobarTemplate "1-top-athena" = "%UnsafeStdinReader% }%timew%{ %pia%%dropbox%  |  %volume%  |  %date%"
getXmobarTemplate "1-top-aphrodite" = "%UnsafeStdinReader% }%timew%{ %pia%%dropbox%  |  %battery%  |  %volume%  |  %date%"
getXmobarTemplate "1-bottom" = "%cpu%  |  %memory%}%calevent%{%counter%%dynnetwork%"
getXmobarTemplate "2-top" = "}%KVAY%     [%sunrise% / %sunset%]{"   -- KVAY: Mount Holly; KSMQ: Piscataway Township
getXmobarTemplate "2-bottom" = "}{"

removeEmptyWorkspaceAfter' f = do
    workspaceList <- gets (W.workspaces . windowset)
    let n = length $ workspaceList
    when (n > 3) $ DW.removeEmptyWorkspaceAfter f
    when (n <= 3) $ f

removeEmptyWorkspace' = do
    workspaceList <- gets (W.workspaces . windowset)
    let n = length $ workspaceList
    when (n > 3) $ DW.removeEmptyWorkspace

launch_app :: String -> String -> X ()
launch_app ws cmd = sequence_ [DW.addWorkspace ws, (spawnHere $ "WS_is_Empty && " ++ cmd)]


------------------------------- Key Bindings ----------------------------------

------- Modifier Masks (mod1Mask: alt, mod4Mask: super)
--
-- The `alpha` and `beta` keys will always be set to either 'super' or 'alt',
-- depending on which key you want as your primary meta key.
--
-- NOTE: I have used Xmodmap to swap the 'super' and 'alt' keys on my keyboard.
--       This has no effect on this configuration (i.e. the alt key still corresponds
--       to `mod1Mask`), but most other programs will recognize 'super' as 'alt' and
--       vice-versa.

alpha = mod1Mask
beta = mod4Mask
ctrl = controlMask
shift = shiftMask

myAdditionalKeys = [
   ---------- ALPHANUMERIC CHARACTERS ----------
   -- (you can sort these bindings with `<range>sort r /, [A-z]),/`)
   ((alpha, xK_0), spawn "tmux -L $(tm-socket) switchc -n") -- Tmux Next Session
   , ((alpha, xK_9), spawn "tmux -L $(tm-socket) switchc -p") -- Tmux Previous Session
   , ((alpha .|. beta, a), spawn "alarm") -- Alarm
   , ((alpha, a), spawn "sleep 0.2 && xdotool key alt+a") -- Tmux Prefix
   , ((alpha .|. beta, a), launch_app "ANKI" "anki")
   , ((alpha, b), spawn "clipster_rofi_menu") -- clipmenu
   , ((alpha .|. beta, b), spawn "clipster_gtk")
   , ((alpha, c), launch_app "WEB" "qutebrowser --enable-webengine-inspector")
   , ((alpha, d), windows $ W.focusDown)
   , ((alpha, e), spawn "tm-send --action=clear") -- clear screen
   , ((alpha, f), sendMessage NextLayout) -- Next Layout
   , ((alpha, h), sequence_ [sendMessage FirstLayout, N2D.windowGo N2D.L False])
   , ((alpha .|. beta, h), sendMessage Shrink) -- Next Layout
   , ((alpha .|. shift, h), spawn "tm-send --action 'cd $(tmdir --get $(tmux display-message -p \"#S\")) && lls'") -- cd to Tmux Home Dir
   , ((alpha, j), sequence_ [sendMessage FirstLayout, N2D.windowGo N2D.D False])
   , ((alpha .|. beta, j), sendMessage RT.MirrorShrink) -- Shrink Master Area
   , ((alpha, k), sequence_ [sendMessage FirstLayout, N2D.windowGo N2D.U False])
   , ((alpha .|. beta, k), sendMessage RT.MirrorExpand) -- Expand Master Area
   , ((alpha .|. shift, k), spawn "tm-kill") -- Kill Screen
   , ((alpha, l), sequence_ [sendMessage FirstLayout, N2D.windowGo N2D.R False])
   , ((alpha .|. beta, l), sendMessage Expand)
   , ((alpha .|. shift, l), spawn "my-screenlock") -- screenlock
   , ((alpha, m), sequence_ [DW.addHiddenWorkspace "MISC", windows $ W.shift "MISC", removeEmptyWorkspaceAfter' $ windows $ W.view "MISC"]) -- Shift current window to MISC
   , ((alpha .|. beta, m), spawn "toggle_monitor && sleep 1 && killall xmobar; xmonad --restart") -- Toggle External Monitor
   , ((alpha, n), spawn "tmux -L $(tm-socket) next-window") -- Tmux Next
   , ((alpha .|. beta, n), sequence_ [DW.addWorkspacePrompt myXPConfig, DW.setWorkspaceIndex 1,
                           CW.toggleWS' ["NSP"], DW.withWorkspaceIndex W.shift 1,
                           removeEmptyWorkspaceAfter' $ DW.withWorkspaceIndex W.view 1]) -- Shift current window to _______
   , ((alpha, o), CW.toggleWS' ["NSP"]) -- Toggle to Last Workspace
   , ((alpha .|. beta, o), spawn "dmenu_books --application=okular") -- Open New Book in Okular
   , ((alpha, p), spawn "tmux -L $(tm-socket) previous-window") -- Tmux Previous
   , ((alpha .|. beta, p), spawn "PIA") -- Toggle PIA
   , ((alpha .|. shift, p), spawn "pause_task")
   , ((alpha, q), spawn "tm-send --action=quit") -- Quit Screen
   , ((alpha .|. ctrl, r), DW.removeWorkspace)  -- Remove Current Workspace
   , ((alpha .|. shift, r), removeEmptyWorkspace') -- Remove Current Workspace if Empty
   , ((alpha .|. beta .|. ctrl, r), spawn "confirm --dmenu 'systemctl reboot -i'") -- Restart
   , ((alpha, r), spawn "killall xmobar; xmonad --recompile && xmonad --restart") -- Restarts XMonad
   , ((alpha, s), sequence_ [removeEmptyWorkspace', CW.swapNextScreen, removeEmptyWorkspace']) -- Swap
   , ((alpha .|. beta, s), windows W.swapDown)    -- Shift Local
   , ((alpha .|. beta .|. ctrl, s), spawn "confirm --dmenu 'smart_shutdown'") -- Shutdown
   , ((alpha, t), spawn "rofi -dmenu -format 'q' -p 'Inbox' | xargs task add +inbox | tail -1 | xargs -I _ notify-send -u low _") -- taskwarrior (inbox)
   , ((alpha .|. beta, t), spawn "rofi -format 'q' -dmenu -p 'Due Today' | xargs task add due:today | tail -1 | xargs -I _ notify-send -u low _ && task_refresh") -- taskwarrior (due today)
   , ((alpha .|. shift, t), spawn "task_hotstart")
   , ((alpha, u), windows $ W.focusUp)
   , ((alpha, v), launch_app "VLC" "vlc")
   , ((alpha, x), launch_app "TERM" myTerminal)
   , ((alpha, w), spawn "close-window") -- Close Focused Window
   , ((alpha, z), launch_app "ZATH" "zathura")

   ---------- SPECIAL CHARACTERS ----------
   -- (you can sort these bindings with `:<range>sort r /K_[A-z]/`)
   , ((0, xF86XK_Calculator), NSP.namedScratchpadAction scratchpads "calculator") -- Scratchpad Calculator
   , ((alpha, xK_KP_Add), spawn "next_task")
   , ((alpha, xK_KP_Delete), spawn "task start.any: stop && timew delete @1 && task_refresh")
   , ((alpha, xK_KP_Divide), spawn "wait_task -N")
   , ((alpha, xK_KP_Enter), spawn "task start.any: done && task_refresh")
   , ((alpha, xK_KP_Insert), spawn "task start.any: stop && task_refresh")
   , ((alpha, xK_KP_Subtract), spawn "last_task")
   , ((alpha, xK_KP_Multiply), spawn "wait_task -D 1h -N")
   , ((alpha, xK_Print), spawn "sshot") -- Screenshot
   , ((beta, xK_Print), spawn "receipt_sshot") -- Screenshot (saved as receipt)
   , ((alpha, xK_Tab), CW.nextScreen) -- Next Screen
   , ((alpha, xK_apostrophe), NSP.namedScratchpadAction scratchpads "weechat") -- Scratchpad Add Task to Inbox
   , ((alpha, xK_backslash), CW.nextScreen) -- Next Screen
   , ((alpha .|. beta, xK_backslash), sequence_ [CW.swapNextScreen, CW.toggleWS' ["NSP"]]) -- Send current WS to Next Screen (keep focus)
   , ((alpha, xK_bracketleft), sequence_ [CW.moveTo CW.Prev (CW.WSIs hiddenNotNSP)]) -- Prev Hidden NonEmpty Workspace
   , ((alpha .|. beta, xK_bracketleft), sequence_ [CW.nextScreen, CW.moveTo CW.Prev (CW.WSIs hiddenNotNSP), CW.prevScreen]) -- Prev Hidden NonEmpty Workspace (viewed on non-active screen)
   , ((alpha, xK_bracketright), sequence_ [CW.moveTo CW.Next (CW.WSIs hiddenNotNSP)]) -- Next Hidden NonEmpty Workspace
   , ((alpha .|. beta, xK_bracketright), sequence_ [CW.nextScreen, CW.moveTo CW.Next (CW.WSIs hiddenNotNSP), CW.prevScreen]) -- Next Hidden NonEmpty Workspace (viewed on non-active screen)
   , ((alpha, xK_comma), sequence_ [spawn "task_refresh", NSP.namedScratchpadAction scratchpads "gtd"]) -- Scratchpad GTD
   , ((alpha, xK_equal), spawn "tm-send --action='cd $(popu); lls'") -- cd to Next Dir
   , ((alpha .|. beta, xK_equal), NSP.namedScratchpadAction scratchpads "calculator") -- Calculator Scratchpad
   , ((alpha, xK_minus), spawn "tm-send --action='pushu && popd; lls'") -- cd to Last Dir
   , ((alpha, xK_period), sequence_ [NSP.namedScratchpadAction scratchpads "scratchpad"])
   , ((alpha, xK_semicolon), spawn "shellPrompt -d")
   , ((alpha .|. beta, xK_slash), sequence_ [CW.swapNextScreen, CW.toggleWS' ["NSP"], CW.nextScreen]) -- Send current WS to Next Screen (send focus)
   , ((alpha, xK_space), spawn "rofi -modi drun -show drun") -- Program Launcher
   , ((alpha .|. beta, xK_space), sequence_ [DW.addWorkspace "MISC", spawn "rofi -modi drun -show drun"]) -- Program Launcher (MISC)
   ]

   -- View WS
   ++ [((alpha, k), withNthWorkspace' W.view i)
       | (i, k) <- zip [0..7] $ [xK_1 .. xK_8]
      ]

   -- Shift to WS; then Focus WS
   ++ [((alpha .|. beta, k), sequence_ [withNthWorkspace' W.shift i, withNthWorkspace' W.view i])
       | (i, k) <- zip [0..7] $ [xK_1 .. xK_8]
      ]

   where
       a = xK_a; b = xK_b; c = xK_c; d = xK_d; e = xK_e; f = xK_f
       g = xK_g; h = xK_h; i = xK_i; j = xK_j; k = xK_k; l = xK_l
       m = xK_m; n = xK_n; o = xK_o; p = xK_p; q = xK_q; r = xK_r
       s = xK_s; t = xK_t; u = xK_u; v = xK_v; w = xK_w; x = xK_x
       y = xK_y; z = xK_z

-------------------------------- Misc Configs ---------------------------------
myTerminal = "urxvt -e zsh -c 'tm-init Terminal'"

myFocusFollowsMouse = False
myClickJustFocuses = False

-- Colors --
yellow = "#F8FB27"
red = "#FF0000"
blue = "#0000FF"
------------

myBorderWidth = 5
myFocusedBorderColor = blue

myWorkspaces :: [String]
myWorkspaces = ["TERM","WEB","NSP"]

myXPConfig :: P.XPConfig
myXPConfig = P.def {
  P.font = "xft:Source Code Pro",
  P.position = P.CenteredAt 0.2 0.4
}

myLayout = resizableTall ||| Full
  where
    resizableTall = smartSpacing 5 $ RT.ResizableTall nmaster delta ratio []
    nmaster = 1
    ratio = 1/2
    delta = 3/100

-- Measurements used by Floating Windows
l = 0.25; bigl = 0.05  -- Distance from left edge
t = 0.4; bigt = 0.05  -- Distance from top edge
w = 0.5; bigw = 0.9
h = 0.5; bigh = 0.9

scratchpads = [ NSP.NS "scratchpad" scratchpad (appName =? "scratchpad") 
                    (NSP.customFloating $ W.RationalRect l t w h)
              , NSP.NS "calculator" calculator (appName =? "calculator")
                    (NSP.customFloating $ W.RationalRect l t w h)
              , NSP.NS "weechat" weechat (appName =? "weechat")
                    (NSP.customFloating $ W.RationalRect bigl bigt bigw bigh)
              , NSP.NS "gtd" gtd (appName =? "GTD")
                    (NSP.customFloating $ W.RationalRect bigl bigt bigw bigh) ]
            where 
                role = stringProperty "WM_WINDOW_ROLE"
                calculator = "urxvt -name calculator -e zsh -c 'bc -l'"
                scratchpad = "urxvt -name scratchpad -e zsh -c 'tmuxinator start ScratchPad root=$(tmdir --get ScratchPad)'"
                weechat = "urxvt -name weechat -e zsh -c 'tmuxinator start WeeChat root=$(tmdir --get WeeChat)'"
                gtd = "urxvt -name GTD -e zsh -c 'tmuxinator start GTD root=$(tmdir --get GTD)'"

myManageHook = composeAll
    [ manageSpawn
    , NSP.namedScratchpadManageHook scratchpads
    , className=? "Peek"            --> doFloat
    , className=? "Pinentry"        --> doFloat
    , appName=? "calculator"        --> doRectFloat (W.RationalRect l t w h)
    , className=? "Clipster"        --> doRectFloat (W.RationalRect bigl bigt bigw bigh)
    , appName=? "floater"           --> doRectFloat (W.RationalRect l t w h)
    , appName=? "big-floater"       --> doRectFloat (W.RationalRect bigl bigt bigw bigh)
    , appName=? "qute-editor"       --> doRectFloat (W.RationalRect 0.3 0.4 0.3 0.15)]

myStartupHook = ewmhDesktopsStartup
                >> setWMName "LG3D"
                >> spawn "maintCheck"
                >> spawn "init-bg"
                >> spawn "sleep 2 && volume-xmonad"
                >> spawn "sleep 2 && calalrms"
                >> spawn ((xmobarTempFmt $ getXmobarTemplate "1-bottom") ++ " --position=Bottom")
                >> spawn ("[[ $(x11screens) -ge 2 ]] && " ++ (xmobarTempFmt $ getXmobarTemplate "2-top") ++ " --screen=1")
                >> spawn ("[[ $(x11screens) -ge 2 ]] && " ++ (xmobarTempFmt $ getXmobarTemplate "2-bottom") ++ " --position=Bottom --screen=1")

-------------------------------- Main -----------------------------------------
main :: IO ()
main = do
    hostname <- getHostName
    xmproc <- spawnPipe (xmobarTempFmt $ getXmobarTemplate $ "1-top-" ++ hostname)
    xmonad $ ewmh desktopConfig
        {
            terminal                = myTerminal
          , modMask                 = alpha
          , borderWidth             = myBorderWidth
          , focusedBorderColor      = myFocusedBorderColor
          , focusFollowsMouse       = myFocusFollowsMouse
          , clickJustFocuses        = myClickJustFocuses
          , workspaces              = myWorkspaces
          , manageHook              = myManageHook
          , layoutHook              = avoidStruts $ myLayout
          , startupHook             = myStartupHook
          , logHook                 = DL.dynamicLogWithPP DL.xmobarPP
            { DL.ppOutput                = hPutStrLn xmproc
            , DL.ppOrder                 = \(ws:l:t:_)   -> [ws]
            , DL.ppCurrent               = DL.xmobarColor "yellow" "" . DL.wrap "[" "]"
            , DL.ppHidden                = DL.xmobarColor "white" ""
            , DL.ppHiddenNoWindows       = DL.xmobarColor "darkgrey" ""
            , DL.ppWsSep                 = "    "
            , DL.ppTitle                 = DL.xmobarColor "green"  "" . DL.shorten 40
            , DL.ppVisible               = DL.xmobarColor "yellow" ""
            , DL.ppUrgent                = DL.xmobarColor "red" "yellow"
            , DL.ppSort                  = (NSP.namedScratchpadFilterOutWorkspace .) `liftM` DW.getSortByOrder
            } >> ewmhDesktopsLogHook <+> DL.dynamicLogXinerama
      } `additionalKeys` myAdditionalKeys
