import Control.Monad

import XMonad hiding ( (|||) )
import XMonad.Config.Desktop
import XMonad.Config.Gnome
import System.IO
import System.Directory
import qualified Data.Map as M
import qualified XMonad.StackSet as W

-- Hooks
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers (isFullscreen,doFullFloat)
import XMonad.Hooks.UrgencyHook
import XMonad.Hooks.SetWMName
import XMonad.Hooks.ToggleHook

-- Layout
import XMonad.Layout hiding ( (|||) )
import XMonad.Layout.MosaicAlt
import XMonad.Layout.Maximize
import XMonad.Layout.ThreeColumns
import XMonad.Layout.Tabbed
import XMonad.Layout.Circle
import XMonad.Layout.LayoutCombinators
import XMonad.Layout.Named
import XMonad.Layout.Grid
--import XMonad.Layout.Groups.Wmii
import XMonad.Layout.Reflect       -- ability to reflect layouts
import XMonad.Layout.MultiToggle   -- apply layout modifiers dynamically
import XMonad.Layout.MultiToggle.Instances
import XMonad.Layout.NoBorders

-- Key Definition
import XMonad.Prompt
import XMonad.Prompt.Shell
import XMonad.Prompt.XMonad
import XMonad.Config.Azerty

-- Cycle des workspaces
import XMonad.Actions.CycleWS
import XMonad.Actions.UpdatePointer
import XMonad.Actions.DynamicWorkspaces
import XMonad.Actions.DynamicWorkspaceOrder

-- Util
import XMonad.Util.Run(spawnPipe,safeSpawn)
import XMonad.Util.EZConfig(additionalKeys)
import XMonad.Util.Scratchpad
import XMonad.Util.NamedScratchpad
import XMonad.Util.Loggers
import XMonad.Util.WorkspaceCompare

-- Acitivity management
import DynamicActivity

-- Specific Host configuration
import Host

-- Modular Configuration
myTerminal = "urxvt"
myWorkspaces = ["1","2","3","4","5","6","7","8","9","0"]

myManageHook = manageDocks <+> namedScratchpadManageHook myScratchpads <+> scratchpadManageHook (W.RationalRect 0.125 0.25 0.75 0.5) <+> (composeAll . concat $
        [   [ className =? x --> doFloat | x <- myFloats]
          , [isFullscreen --> (doF W.focusDown <+> doFullFloat)]
	])
        where
          myFloats = ["Gimp","Xmessage","vmware"]


-------------------------------------------------------------------------------------------
-- ACTIVITY CONFIGURATION
--
myActivityConf :: ActivityConfig
active_activity_color = "#FFA500"
inactive_activity_color = "#585858"
urgent_activity_color = "#FF0000"
background_activity_color = "#161616"
myActivityConf = defaultActivityConfig { ppActivityActive   = dzenColor active_activity_color background_activity_color,
                                         ppActivityInactive = dzenColor inactive_activity_color background_activity_color,
                                         ppActivityUrgent   = dzenColor urgent_activity_color background_activity_color
                                       }

-------------------------------------------------------------------------------------------
-- LAYOUT DEFINITIONS
--
myLayout = avoidStruts $
           mkToggle1 MIRROR $
           mkToggle1 REFLECTX $
           mkToggle1 REFLECTY $
           mkToggle1 NBFULL $
           named "tiled" tiled
           ||| Grid
           ||| tabbed shrinkText defaultTheme
           ||| threeCol
           ||| named "Full" Full
--           ||| named "Stacked" (wmii shrinkText defaultTheme ||| tiled ||| threeCol)
   where
     tiled   = Tall nmaster delta ratio  -- default tiling algorithm partitions the screen into two panes
     threeCol = ThreeCol nmaster delta ratio
     nmaster = 1      -- The default number of windows in the master pane
     ratio   = 1/2    -- Default proportion of screen occupied by master pane
     delta   = 2/100  -- Percent of screen to increment by when resizing panes

-- Prompt setup:
myPrompt = defaultXPConfig {
             position    = Top
           , font        = "xft:Consolas-14"
           , height      = 24
           -- Zenburn!:
           , bgColor     = "#3F3F3F"
           , fgColor     = "#EFEFEF"
           , fgHLight    = "#000D18"
           , bgHLight    = "#8FAF9F"
           , borderColor = "#719E7F"
           }


colorBlack          = "#000000"
colorBlackAlt       = "#040404"
colorGray           = "#444444"
colorGrayAlt        = "#282828"
colorDarkGray       = "#161616"
colorWhite          = "#cfbfad"
colorWhiteAlt       = "#8c8b8e"
colorDarkWhite      = "#606060"
colorCream          = "#a9a6af"
colorDarkCream      = "#5f656b"
colorMagenta        = "#a488d9"
colorMagentaAlt     = "#7965ac"
colorDarkMagenta    = "#8e82a2"
colorBlue           = "#60a0e0"
colorBlueAlt        = "#598691"
colorDarkBlue       = "#464a4a"
colorRed 			= "#FF0000"
colorGreen 			= "#00FF00"
colorNormalBorder   = colorDarkWhite
colorFocusedBorder  = colorMagenta

barFont = "-misc-fixed-medium-r-semicondensed-*-12-110-75-75-c-60-koi8-r"

myStatusBar = "dzen2 -x '0' -y '0' -h '14' -w '960' -ta 'l' -bg '" ++ colorDarkGray ++ "' -fg '" ++ colorCream ++ "' -fn '" ++ barFont ++ "'"
mySecondStatusBar = "/home/ben64/Documents/tool/dzen2conf/conf_dzen.py"

myIconDir = "/home/ben64/.xmonad/icons/"

myLogHook :: Handle -> X ()
myLogHook h = dynamicLogWithPP $ defaultPP
    {
        ppCurrent           =   dzenColor colorGreen    colorDarkGray . hideScratchpad . dropActivityName myActivityConf
      , ppVisible           =   dzenColor "#FFFF00" colorDarkGray . hideScratchpad . dropActivityName myActivityConf
      , ppHidden            =   dzenColor colorBlueAlt  colorDarkGray . hideScratchpad . dropActivityName myActivityConf
      , ppHiddenNoWindows   =   dzenColor colorGray       colorDarkGray . hideScratchpad . dropActivityName myActivityConf
      , ppUrgent            =   dzenColor colorRed    colorDarkGray . hideScratchpad . dropActivityName myActivityConf
      , ppWsSep             =   ""
      , ppSep               =   " | "
      , ppLayout            =   dzenColor colorMagentaAlt colorDarkGray .
                                (\x -> case x of
                                    "tiled" -> icon x
                                    "Full" -> icon x
                                    "Mirror Tall" -> icon x
                                    "Mirror Tiled" -> icon "Mirror Tall"
                                    "ThreeCol" -> icon x
                                    "Grid" -> icon x
                                    _      -> x
                                )
      , ppTitle             =   (" " ++) . dzenColor colorWhiteAlt colorDarkGray . dzenEscape
      , ppOutput            =   hPutStrLn h
      , ppExtras            =   [print_activities myActivityConf]
      , ppSort              =   liftM2 (.) (filterWorkspaces myActivityConf) getSortByOrder
      -- Display extra in first position
      , ppOrder             =   \(ws:layout:t:extra) -> extra ++ [ws,layout,t]
    }
    where
      hideScratchpad ws = if ws == "NSP" then "" else pad ws -- hide sp in ws list (thanks to p.brisbin)
      icon layout= "^i(" ++ myIconDir ++ layout ++ ".xbm)"
-----------------------------------------------------------------------------------------------------
-- SCRATCHPAD
--
myScratchpads = [
		NS "python" "urxvt -title ipython -e zsh -i -c ipython" (title =? "ipython") (customFloating $ W.RationalRect 0.125 0.25 0.75 0.5),
		NS "htop" "urxvt -e htop" (title =? "htop") (customFloating $ W.RationalRect 0.125 0.25 0.75 0.5),
		NS "weechat" "urxvt -e weechat-curses" (title =? "weechat-curses") (customFloating $ W.RationalRect 0.125 0.25 0.75 0.5),
        NS "mutt" "urxvt -e mutt" (title =? "mutt") (customFloating $ W.RationalRect 0.125 0.125 0.75 0.75),
        NS "syslog" "urxvt -title syslog -e tail -f /var/log/syslog" (title =? "syslog") (customFloating $ W.RationalRect 0.125 0.125 0.75 0.75)
		]
-----------------------------------------------------------------------------------------------------
-- SHORTCUT DEFINITIONS
--

modm = mod4Mask
activityMod = modm .|. mod5Mask  -- Combination to use activity
layoutMod = modm .|. controlMask -- Combination to use layout modifier
wsMod = modm .|. mod1Mask        -- Combination to use ws modifier


myGeneralKeys =
  [
    -- Restart xmonad and all dzen2 bar
    ((mod4Mask, xK_q), spawn "if type xmonad; then kill $(ps ax | grep conf_dzen | grep -v grep | awk '{print $1}'); killall dzen2; xmonad --recompile && xmonad --restart; else xmessage xmonad not in \\$PATH: \"$PATH\"; fi")
  , ((mod4Mask, xK_semicolon), sendMessage (IncMasterN (-1)))

    -- Workspace Navigation
  , ((mod4Mask, xK_Right), gotoNextActivityWorkspace myActivityConf >> runLogHook)
  , ((mod4Mask, xK_Left), gotoPrevActivityWorkspace myActivityConf >> runLogHook)
  , ((mod4Mask .|. shiftMask, xK_Right), shiftToNextActivityWorkspace myActivityConf >> gotoNextActivityWorkspace myActivityConf)
  , ((mod4Mask .|. shiftMask, xK_Left), shiftToPrevActivityWorkspace myActivityConf >> gotoPrevActivityWorkspace myActivityConf)

    -- Screen Navigation
  , ((mod4Mask .|. controlMask, xK_Right), nextScreen)
  , ((mod4Mask .|. controlMask, xK_Left), prevScreen)

    -- Layout navigation
  , ((mod4Mask, xK_g), sendMessage $ JumpToLayout "tiled")
  , ((mod4Mask,  xK_f), sendMessage $ JumpToLayout "Full")

    -- Layout Modifier
  , ((layoutMod, xK_m), sendMessage $ Toggle MIRROR)
  , ((layoutMod, xK_x), sendMessage $ Toggle REFLECTX)
  , ((layoutMod, xK_y), sendMessage $ Toggle REFLECTY)

    -- Useful toolz
  , ((mod4Mask, xK_s), spawn "urxvt")
  , ((mod4Mask, xK_i), spawn "urxvt -e ipython")
  , ((mod4Mask, xK_e), spawn "emacs")
  , ((mod4Mask, xK_c), spawn "chrome")
  , ((mod4Mask .|. shiftMask, xK_i), spawn "urxvt -e irb1.9.1")
  , ((mod4Mask, xK_v), spawn "urxvt -e mc -u")

    -- Scratchpads
  , ((mod4Mask, xK_twosuperior), scratchpadSpawnAction defaultConfig {terminal=myTerminal})
  , ((mod4Mask, xK_a), namedScratchpadAction myScratchpads "python")
  , ((mod4Mask, xK_z), namedScratchpadAction myScratchpads "htop")
  , ((mod4Mask, xK_r), namedScratchpadAction myScratchpads "weechat")
  , ((mod4Mask, xK_m), namedScratchpadAction myScratchpads "mutt")
  , ((mod4Mask, xK_x), namedScratchpadAction myScratchpads "syslog")

    -- WMII
  -- , ((mod4Mask .|. controlMask, xK_h), focusGroupUp)
  -- , ((mod4Mask .|. controlMask, xK_l), focusGroupDown)
  -- , ((mod4Mask .|. controlMask .|. shiftMask, xK_h), moveToGroupUp False)
  -- , ((mod4Mask .|. controlMask .|. shiftMask, xK_l), moveToGroupDown False)
  -- , ((mod4Mask .|. controlMask, xK_j), focusDown)
  -- , ((mod4Mask .|. controlMask, xK_k), focusUp)
  -- , ((mod4Mask .|. controlMask .|. shiftMask, xK_j), swapDown)
  -- , ((mod4Mask .|. controlMask .|. shiftMask, xK_k), swapUp)
  -- , ((mod4Mask .|. controlMask, xK_space), groupToNextLayout)
  -- , ((mod4Mask .|. controlMask, xK_i), zoomGroupOut)
  -- , ((mod4Mask .|. controlMask, xK_o), zoomGroupIn)
  -- , ((mod4Mask .|. controlMask, xK_f), sendMessage NextLayout)

    -- Activities
  , ((activityMod, xK_Right), nextActivity myActivityConf >> runLogHook)
  , ((activityMod, xK_Left), prevActivity myActivityConf >> runLogHook)
  , (( mod4Mask, xK_Down), prevActivity myActivityConf >> runLogHook)
  , (( mod4Mask, xK_Up), nextActivity myActivityConf >> runLogHook)

  , ((mod4Mask .|. shiftMask, xK_Up), shiftToNextActivity myActivityConf >> runLogHook)
  , ((mod4Mask .|. shiftMask, xK_Up), shiftToNextActivity myActivityConf >> runLogHook)
  , ((mod4Mask .|. shiftMask, xK_Down), shiftToPrevActivity myActivityConf >> runLogHook)

  , ((activityMod, xK_n), promptActivityAdd myPrompt myActivityConf "New Activity : " >> runLogHook)
  , ((activityMod, xK_d), delCurrentActivity myActivityConf >> runLogHook)
  , ((wsMod, xK_n), promptAddActivityWorkspace myPrompt myActivityConf "Add Workspace : " >> runLogHook)
  , ((wsMod, xK_r), promptRenameCurrentActivityWorkspace myPrompt myActivityConf "Rename Workspace : " >> runLogHook)
  , ((wsMod, xK_d), delCurrentActivityWorkspace myActivityConf >> runLogHook)
  , ((activityMod, xK_w), debugActivity)
  ]
  ++
  -- Direct Access to Workspace, to Activity, and shift
  [ ((m .|. mod4Mask, k), f i)
  | (i, k) <- zip [0..10] [0x26,0xe9,0x22,0x27,0x28,0x2d,0xe8,0x5f,0xe7,0xe0]
  , (f, m) <- [(gotoActivityWorkspace myActivityConf, 0),(gotoActivity myActivityConf,activityMod),(shifttoActivityWorkspace myActivityConf,shiftMask),(shiftToActivity myActivityConf, activityMod .|. shiftMask)]
  ]

-- Concat General keys with specific host keys
--get_keys :: IO [((ButtonMask, KeySym), X ())]
--get_keys = do
--	exist <- doesFileExist "lib/Host.hs"
--	if exist
--		then myGeneralKeys -- ++ myHostKeys
--		else myGeneralKeys
-- myKeys = myGeneralKeys ++ myHostKeys
get_keys :: [((ButtonMask, KeySym), X ())]
get_keys = myGeneralKeys ++ myHostKeys

-----------------------------------------------------------------------------------------------------
-- MAIN
--
main = do
	-- Launch right bar
	safeSpawn mySecondStatusBar ["--font"," " ++ barFont]
	dzenStatusBar <- spawnPipe myStatusBar
	xmonad $ withUrgencyHook dzenUrgencyHook { args = ["-fn", barFont, "-bg", colorDarkCream, "-fg", colorBlue]}
               $ ewmh gnomeConfig{
		  --logHook             = myLogHook dzenStatusBar >> setWMName "LG3D"
		  logHook    = myLogHook dzenStatusBar >> setWMName "LG3D" >> updatePointer (Relative 0.5 0.5)
		, terminal   = myTerminal
		, layoutHook = lessBorders OnlyFloat $ avoidStruts $ myLayout
		, manageHook = myManageHook <+> manageHook defaultConfig
		, workspaces = myWorkspaces
		, modMask    = mod4Mask 	-- Rebind Mod to the Window Key
		, handleEventHook = handleEventHook defaultConfig <+> fullscreenEventHook
		, startupHook = startupHook gnomeConfig >> setWMName "LG3D"
      }  `additionalKeys` get_keys
