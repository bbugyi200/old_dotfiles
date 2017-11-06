-------------------------------------------------------------------------------
-------------------------------- Imports --------------------------------------
import System.IO
import XMonad
import XMonad.Actions.PhysicalScreens
import XMonad.Config.Desktop
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Util.Run
import XMonad.Util.EZConfig
import XMonad.Layout.Spacing

import qualified XMonad.StackSet as W

------------------------------- Key Bindings ----------------------------------
myAdditionalKeys =
	[ ((mod1Mask, xK_r), broadcastMessage ReleaseResources >> restart "xmonad" True)
	, ((mod4Mask, xK_KP_Delete), spawn "ham start")
	, ((mod4Mask, xK_KP_Insert), spawn "ham stop")
	, ((mod1Mask, xK_e), spawn "clear_screen")
	, ((mod1Mask, xK_q), spawn "quit_screen")
	, ((mod1Mask, xK_backslash), onNextNeighbour W.view)
	]

-------------------------------- Misc Configs ---------------------------------
myTerminal = "termite -e 'tm-init Terminal'"
myModMask = mod1Mask

myFocusFollowsMouse = False
myClickJustFocuses = False

xmobarEscape = concatMap doubleLts
  where doubleLts '<' = "<<"
        doubleLts x   = [x]

myWorkspaces :: [String]        
myWorkspaces = clickable . (map xmobarEscape) $ ["I","II","III","IV","V","VI","VII","VIII","IX","X"]
  where                                                                       
         clickable l = [ "<action=xdotool key super+" ++ show (n) ++ ">" ++ ws ++ "</action>" |
                             (i,ws) <- zip [1..10] l,                                        
                            let n = i ]

myLayout = tiled ||| Mirror tiled ||| Full
  where
    tiled = smartSpacing 5 $ Tall nmaster delta ratio
    nmaster = 1
    ratio = 1/2
    delta = 3/100

-------------------------------- Main -----------------------------------------
main = do
	xmproc <- spawnPipe "xmobar /home/bryan/.xmobarrc"
	xmonad $ desktopConfig
		{
		  terminal				= myTerminal
		  , modMask				= myModMask
		  , focusFollowsMouse 	= myFocusFollowsMouse
		  , clickJustFocuses  	= myClickJustFocuses
		  , workspaces			= myWorkspaces
		  , manageHook 			= manageDocks <+> manageHook desktopConfig
		  , layoutHook			= avoidStruts $ myLayout
		  , logHook 			= dynamicLogWithPP xmobarPP
			{ ppOutput 			= hPutStrLn xmproc,
			  ppOrder           = \(ws:l:t:_)   -> [ws],
			  ppCurrent 		= xmobarColor "yellow" "" . wrap "[" "]",
			  ppHiddenNoWindows = xmobarColor "grey" "",
			  ppTitle   		= xmobarColor "green"  "" . shorten 40,
			  ppVisible 		= wrap "(" ")",
			  ppUrgent  		= xmobarColor "red" "yellow"
			}
	  } `additionalKeys` myAdditionalKeys
