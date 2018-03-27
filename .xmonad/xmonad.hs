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
import XMonad.Hooks.ManageHelpers (doRectFloat)

import Data.Maybe (isNothing,isJust)
import Control.Monad (liftM,when)
import Graphics.X11.ExtraTypes.XF86
import Network.HostName (getHostName)

import qualified XMonad.StackSet as W
import qualified XMonad.Prompt as P
import qualified XMonad.Util.NamedScratchpad as NSP
import qualified XMonad.Hooks.DynamicLog as DL
import qualified XMonad.Actions.CycleWS as CW
import qualified XMonad.Actions.DynamicWorkspaces as DW
import qualified XMonad.Actions.DynamicWorkspaceOrder as DW

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
getXmobarTemplate "1-top-athena" = "%UnsafeStdinReader% }%timew%{ %counter%%pia%%dynnetwork%  |  %dropbox%  |  %volume%  |  %date%"
getXmobarTemplate "1-top-aphrodite" = "%UnsafeStdinReader% }%timew%{ %pia%%dropbox%  |  %battery%  |  %volume%  |  %date%"
getXmobarTemplate "1-bottom" = "%cpu%  |  %memory%}%calevent%{%counter%%dynnetwork%"
getXmobarTemplate "2-top" = "}%KVAY%{"   -- KVAY: Mount Holly; KSMQ: Piscataway Township
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
   ((alpha, xK_0), spawn "tmux switchc -n") -- Tmux Next Session
   , ((alpha, xK_9), spawn "tmux switchc -p") -- Tmux Previous Session
   , ((alpha .|. beta, a), spawn "alarm") -- Alarm
   , ((alpha, a), spawn "sleep 0.2 && xdotool key alt+a") -- Tmux Prefix
   , ((alpha .|. ctrl, a), spawn "khal-alarms") -- Calendar Alarms
   , ((alpha, b), spawn "clipmenu") -- clipmenu
   , ((alpha .|. ctrl, c), NSP.namedScratchpadAction scratchpads "calculator") -- Calculator Scratchpad
   , ((alpha, e), spawn "tm-send --action=clear") -- clear screen
   , ((alpha, f), windows $ W.focusUp)     -- Focus Local
   , ((alpha .|. beta, f), windows W.swapDown)    -- Shift Local
   , ((alpha, h), spawn "tm-send --action 'clear && cd $(defaultTmuxDir --get $(tmux display-message -p \"#S\"))'") -- cd to Tmux Home Dir
   , ((alpha .|. beta, j), sendMessage Shrink) -- Shrink Master Area
   , ((alpha .|. beta, k), sendMessage Expand) -- Expand Master Area
   , ((alpha, k), spawn "tm-kill") -- Kill Screen
   , ((alpha, l), spawn "my-screenlock") -- screenlock
   , ((alpha, m), sequence_ [DW.addHiddenWorkspace "MISC", windows $ W.shift "MISC", removeEmptyWorkspaceAfter' $ windows $ W.view "MISC"]) -- Shift current window to MISC
   , ((alpha .|. beta, m), spawn "toggle_monitor && sleep 1 && killall xmobar; xmonad --restart") -- Toggle External Monitor
   , ((alpha, n), spawn "tmux next-window") -- Tmux Next
   , ((alpha .|. beta, n), sequence_ [DW.addWorkspacePrompt myXPConfig, DW.setWorkspaceIndex 1,
                           CW.toggleWS' ["NSP"], DW.withWorkspaceIndex W.shift 1,
                           removeEmptyWorkspaceAfter' $ DW.withWorkspaceIndex W.view 1]) -- Shift current window to _______
   , ((alpha, o), CW.toggleWS' ["NSP"]) -- Toggle to Last Workspace
   , ((alpha .|. beta, o), spawn "dmenu_books --application=okular") -- Open New Book in Okular
   , ((alpha, p), spawn "tmux previous-window") -- Tmux Previous
   , ((alpha .|. beta, p), spawn "PIA") -- Toggle PIA
   , ((alpha .|. ctrl, p), spawn "pause_task")
   , ((alpha, q), spawn "tm-send --action=quit") -- Quit Screen
   , ((alpha .|. ctrl, r), DW.removeWorkspace)  -- Remove Current Workspace
   , ((alpha .|. shift, r), removeEmptyWorkspace') -- Remove Current Workspace if Empty
   , ((ctrl .|. alpha .|. beta, r), spawn "confirm --dmenu 'systemctl reboot -i'") -- Restart
   , ((alpha, r), spawn "killall xmobar; xmonad --recompile && xmonad --restart") -- Restarts XMonad
   , ((alpha, s), sequence_ [removeEmptyWorkspace', CW.swapNextScreen, removeEmptyWorkspace']) -- Swap
   , ((alpha .|. beta, s), sequence_ [removeEmptyWorkspace', CW.swapNextScreen, removeEmptyWorkspace', CW.nextScreen]) -- Swap (keep focus on window)
   , ((ctrl .|. alpha .|. beta, s), spawn "confirm --dmenu 'task start.any: stop; dbox_sync && shutdown now'") -- Shutdown
   , ((alpha, t), spawn "rofi -dmenu -format 'q' -p 'Inbox' | xargs task add +inbox | tail -1 | xargs -I _ notify-send -u low _") -- taskwarrior (inbox)
   , ((alpha .|. beta, t), spawn "rofi -format 'q' -dmenu -p 'Due Today' | xargs task add due:today | tail -1 | xargs -I _ notify-send -u low _ && task_refresh -T") -- taskwarrior (due today)
   , ((alpha, w), spawn "close-window") -- Close Focused Window

   ---------- SPECIAL CHARACTERS ----------
   -- (you can sort these bindings with `:<range>sort r /K_[A-z]/`)
   , ((0, xF86XK_Calculator), NSP.namedScratchpadAction scratchpads "calculator") -- Scratchpad Calculator
   , ((alpha, xK_KP_Add), spawn "next_task")
   , ((alpha, xK_KP_Delete), spawn "rofi -dmenu -format 'q' -p 'Hotfix' | xargs task add project:Hotfix && task start.any: stop; task rc.context:none +LATEST start && task_refresh -T")
   , ((alpha, xK_KP_Enter), spawn "task start.any: done && task_refresh -T")
   , ((alpha, xK_KP_Insert), spawn "task start.any: stop && task_refresh -T")
   , ((alpha, xK_KP_Subtract), spawn "last_task")
   , ((alpha, xK_KP_Multiply), spawn "wait_task")
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
   , ((alpha, xK_equal), spawn "tm-send --action='cd $(popu); clear'") -- cd to Next Dir
   , ((alpha, xK_minus), spawn "tm-send --action='pushu && popd; clear'") -- cd to Last Dir
   , ((alpha, xK_semicolon), NSP.namedScratchpadAction scratchpads "scratchpad") -- Scratchpad
   , ((alpha .|. beta, xK_slash), sequence_ [CW.swapNextScreen, CW.toggleWS' ["NSP"], CW.nextScreen]) -- Send current WS to Next Screen (send focus)
   , ((beta .|. alpha, xK_space), sendMessage NextLayout) -- Next Layout
   , ((alpha .|. beta, xK_space), sequence_ [DW.addWorkspace "MISC", spawn "rofi -modi drun -show drun"]) -- Program Launcher (MISC)
   , ((alpha, xK_space), spawn "rofi -modi drun -show drun") -- Program Launcher
   ]

   -- Launch Applications
   ++ [((alpha, key), sequence_ [DW.addWorkspace ws, (spawnHere $ "WS_is_Empty && " ++ cmd)])
       | (key, cmd, ws) <- zip3
       [x, c, z, v, xK_KP_End, xK_KP_Down]
       [myTerminal,"qutebrowser","zathura","okular","anki","slack"]
       ["TERM","WEB","ZATH","OKULAR","ANKI","SLACK"]
      ]

   -- Launch Second Applications
   ++ [((alpha .|. beta, key), sequence_ [CW.nextScreen, DW.addWorkspace ws, (spawnOn ws $ "WS_is_Empty && " ++ cmd)])
       | (key, cmd, ws) <- zip3
       [c, x, z, v]
       ["qutebrowser", myTerminal, "zathura", "zathura"]
       ["WEB'", "TERM'", "ZATH'", "ZATH"]
      ]

   -- Shift to WS; then Focus WS
   ++ [((alpha, k), sequence_ [withNthWorkspace' W.shift i, withNthWorkspace' W.view i])
       | (i, k) <- zip [0..8] $ [xK_1 .. xK_8]
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

myLayout = tiled ||| Mirror tiled ||| Full
  where
    tiled = smartSpacing 5 $ Tall nmaster delta ratio
    nmaster = 1
    ratio = 1/2
    delta = 3/100

scratchpads = [ NSP.NS "scratchpad" scratchpad (appName =? "scratchpad") 
                    (NSP.customFloating $ W.RationalRect l t w h)
              , NSP.NS "calculator" "galculator" (className =? "Galculator")
                    (NSP.customFloating $ W.RationalRect l t w h)
              , NSP.NS "weechat" weechat (appName =? "weechat")
                    (NSP.customFloating $ W.RationalRect bigl bigt bigw bigh) ]
            where 
                role = stringProperty "WM_WINDOW_ROLE"
                scratchpad = "urxvt -name scratchpad -cd ~/Dropbox/notes -e zsh -c 'clear && ls -a && echo; zsh'" 
                weechat = "urxvt -name weechat -cd ~/.task -e zsh -c 'weechat'"
                l = 0.25; bigl = 0.05  -- Distance from left edge
                t = 0.4; bigt = 0.05  -- Distance from top edge
                w = 0.5; bigw = 0.9
                h = 0.5; bigh = 0.9

myManageHook = composeAll
    [ manageSpawn
    , NSP.namedScratchpadManageHook scratchpads
    , className=? "Galculator"      --> doFloat
    , className=? "Peek"            --> doFloat
    , className=? "Pinentry"        --> doFloat
    , appName=? "qute-editor"     --> doRectFloat (W.RationalRect l t w h)]
    where
        l = 0.3
        t = 0.4
        w = 0.3
        h = 0.15

myStartupHook = ewmhDesktopsStartup
                >> setWMName "LG3D"
                >> spawn "maintCheck"
                >> spawn "init-bg"
                >> spawn "sleep 3 && volume-xmonad"
                >> spawn "alarm --resume"
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
