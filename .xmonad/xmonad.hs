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
getXmobarTemplate "athena" = "%UnsafeStdinReader% }%watson%{ %alarm%%pia%%dynnetwork%  |  %dropbox%  |  %volume%  |  %date%"
getXmobarTemplate "aphrodite" = "%UnsafeStdinReader% }%watson%{ %alarm%%pia%%dynnetwork%  |  %dropbox%  |  %battery%  |  %volume%  |  %date%"
getXmobarTemplate "secondary" = "%cpu%  |  %memory%}%KVAY%{"   -- KVAY: Mount Holly; KSMQ: Piscataway Township

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
--       to `mod1Mask`), but most other programs will recognize 'super' as 'alt'.

alpha = mod1Mask
beta = mod4Mask
ctrl = controlMask
shift = shiftMask

myAdditionalKeys = [
   -- Alarm
   ((alpha .|. beta, a), spawn "alarm-xmonad")

   -- Tmux Prefix
   , ((alpha, a), spawn "xdotool keyup Meta_L Meta_R Super_L Super_R && xdotool key ctrl+a")

   -- clipmenu
   , ((alpha, b), spawn "clipmenu")

   -- Close Focused Window
   , ((alpha, w), spawn "close-window")

   -- Expand or Shrink Master Area
   , ((alpha, j), sendMessage Shrink)
   , ((alpha, k), sendMessage Expand)

   -- Local WS Commands
   , ((alpha, f), windows $ W.focusUp)     -- Focus
   , ((alpha .|. ctrl, f), windows W.swapDown)    -- Shift

   -- Next Layout
   , ((beta .|. alpha, xK_space), sendMessage NextLayout)  -- Toggles Fullscreen

   -- Next Screen
   , ((alpha, xK_backslash), CW.nextScreen)
   , ((alpha, xK_Tab), CW.nextScreen)

   -- Open New Book in Okular
   , ((alpha .|. ctrl, o), spawn "dmenu_books --application=okular")

   -- Prev/Next Hidden NonEmpty Workspace
   , ((alpha, xK_bracketleft), sequence_ [CW.moveTo CW.Prev (CW.WSIs hiddenNotNSP)])
   , ((alpha, xK_bracketright), sequence_ [CW.moveTo CW.Next (CW.WSIs hiddenNotNSP)])

   -- Prev/Next Hidden NonEmpty Workspace (viewed on non-active screen)
   , ((alpha .|. ctrl, xK_bracketleft), sequence_ [CW.nextScreen, CW.moveTo CW.Prev (CW.WSIs hiddenNotNSP), CW.prevScreen])
   , ((alpha .|. ctrl, xK_bracketright), sequence_ [CW.nextScreen, CW.moveTo CW.Next (CW.WSIs hiddenNotNSP), CW.prevScreen])

   -- Program Launcher
   , ((alpha, xK_space), spawn "rofi -modi drun -show drun")
   , ((alpha .|. ctrl, xK_space), sequence_ [DW.addWorkspace "MISC", spawn "rofi -modi drun -show drun"])

   -- Restarts XMonad
   , ((alpha, r), spawn "killall xmobar; xmonad --recompile && xmonad --restart")

   -- Remove Workspaces
   , ((alpha .|. ctrl, r), DW.removeWorkspace)  -- Remove Current Workspace
   , ((alpha .|. shift, r), removeEmptyWorkspace') -- if Empty

   -- Screenshot Commands
   , ((alpha, xK_Print), spawn "sshot")
   , ((alpha .|. ctrl, xK_Print), spawn "receipt_sshot")

   -- Scratchpad
   , ((alpha, xK_semicolon), NSP.namedScratchpadAction scratchpads "scratchpad")
   , ((0, xF86XK_Calculator), NSP.namedScratchpadAction scratchpads "calculator")
   , ((ctrl .|. alpha, c), NSP.namedScratchpadAction scratchpads "calculator")
   , ((alpha, xK_apostrophe), NSP.namedScratchpadAction scratchpads "taskwarrior")

   -- screenlock
   , ((alpha, l), spawn "screenlock")

   -- Send current WS to Next Screen
   , ((ctrl .|. alpha, xK_slash), sequence_ [CW.swapNextScreen, CW.toggleWS' ["NSP"], CW.nextScreen]) -- send focus
   , ((ctrl .|. alpha, xK_backslash), sequence_ [CW.swapNextScreen, CW.toggleWS' ["NSP"]]) -- don't send focus

   -- Shift current window to MISC
   , ((alpha, m), sequence_ [DW.addHiddenWorkspace "MISC", windows $ W.shift "MISC", removeEmptyWorkspaceAfter' $ windows $ W.view "MISC"])

   -- Shift current window to _______
   , ((ctrl, xK_0), sequence_ [DW.addWorkspacePrompt myXPConfig, DW.setWorkspaceIndex 1, CW.toggleWS' ["NSP"], DW.withWorkspaceIndex W.shift 1, removeEmptyWorkspaceAfter' $ DW.withWorkspaceIndex W.view 1])

   -- Shutdown / Restart
   , ((ctrl .|. alpha .|. beta, s),
   spawn "confirm --dmenu 'task start.not: stop && dbox_sync && shutdown now'")
   , ((ctrl .|. alpha .|. beta, r),
   spawn "confirm --dmenu 'systemctl reboot -i'")

   -- Swap
   , ((alpha, s), sequence_ [removeEmptyWorkspace', CW.swapNextScreen, removeEmptyWorkspace'])
   , ((ctrl .|. alpha, s), sequence_ [removeEmptyWorkspace', CW.swapNextScreen, removeEmptyWorkspace', CW.nextScreen])

   -- taskwarrior
   , ((alpha, t), spawn "rofi -dmenu -p 'Inbox' | sed \"s/\\([\\\'\\\"]\\)/\\\\\\\\\\1/g\" | xargs task add +inbox | tail -1 | xargs -I _ notify-send -u low _")

   -- Toggle to Last Workspace
   , ((alpha, o), CW.toggleWS' ["NSP"])

   -- Toggle External Monitor
   , ((ctrl .|. alpha, m), spawn "toggle_monitor && sleep 1 && killall xmobar; xmonad --restart")

   -- Toggle PIA
   , ((ctrl .|. alpha, p), spawn "PIA")

   -- TMUX
   , ((alpha, xK_9), spawn "tmux switchc -p")
   , ((alpha, xK_0), spawn "tmux switchc -n")
   , ((alpha, n), spawn "tmux next-window")
   , ((alpha, p), spawn "tmux previous-window")
   , ((alpha, k), spawn "tm-kill")
   , ((alpha, e), spawn "tm-send --action=clear")
   , ((alpha, q), spawn "tm-send --action=quit")
   , ((alpha, xK_minus), spawn "tm-send --action='pushu && popd'")
   , ((alpha, xK_equal), spawn "tm-send --action='cd $(popu)'")
   , ((alpha, h), spawn "tm-send --action \
        \ 'clear && cd $(defaultTmuxDir --get $(tmux display-message -p \"#S\"))'")
   ]

   -- Launch Applications
   ++ [((alpha, key), sequence_ [DW.addWorkspace ws, (spawnHere $ "WS_is_Empty && " ++ cmd)])
       | (key, cmd, ws) <- zip3
       [x, c, z, v, xK_1, xK_2]
       [myTerminal,"qutebrowser","zathura","okular","anki","slack"]
       ["TERM","WEB","ZATH","OKULAR","ANKI","SLACK"]
      ]

   -- Launch Second Applications
   ++ [((ctrl .|. alpha, key), sequence_ [CW.nextScreen, DW.addWorkspace ws, (spawnOn ws $ "WS_is_Empty && " ++ cmd)])
       | (key, cmd, ws) <- zip3
       [c, x, z, v]
       ["qutebrowser", myTerminal, "zathura", "zathura"]
       ["WEB'", "TERM'", "ZATH'", "ZATH"]
      ]

   -- Shift to WS; then Focus WS
   ++ [((ctrl, k), sequence_ [withNthWorkspace' W.shift i, withNthWorkspace' W.view i])
       | (i, k) <- zip [0..8] $ [xK_1 .. xK_9]
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
myXPConfig = def {P.position = P.Bottom}

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
              , NSP.NS "taskwarrior" taskwarrior (appName =? "taskwarrior")
                    (NSP.customFloating $ W.RationalRect 0.05 0.05 0.9 0.9) ]
            where 
                role = stringProperty "WM_WINDOW_ROLE"
                scratchpad = "urxvt -name scratchpad -cd ~/Dropbox/notes/misc -e zsh -c 'clear && LocalAlias -v; zsh'" 
                taskwarrior = "urxvt -name taskwarrior -cd ~/.task -e zsh -c 'clear && task next +READY limit:25; zsh'"
                l = 0.25 -- Distance from left edge
                t = 0.4  -- Distance from top edge
                w = 0.5
                h = 0.5

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
                >> spawn "alarm-xmonad --resume"
                >> spawn ("[[ $(x11screens) -ge 2 ]] && " ++ (xmobarTempFmt $ getXmobarTemplate "secondary") ++ " --screen=1")

-------------------------------- Main -----------------------------------------
main :: IO ()
main = do
    hostname <- getHostName
    xmproc <- spawnPipe (xmobarTempFmt $ getXmobarTemplate hostname)
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
