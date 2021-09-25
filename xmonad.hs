--
-- xmonad example config file.
--
-- A template showing all available configuration hooks,
-- and how to override the defaults in your own xmonad.hs conf file.
--
-- Normally, you'd only override those defaults you care about.
--
import XMonad hiding ( (|||) )
import Data.Monoid
import System.Exit

import qualified XMonad.StackSet as W
import qualified Data.Map        as M

-- Layouts
import XMonad.Layout.Spacing
import XMonad.Layout.TwoPane
import XMonad.Layout.Fullscreen
import XMonad.Layout.ToggleLayouts
import XMonad.Layout.Grid
import XMonad.Layout.SimpleDecoration
import XMonad.Layout.NoBorders
import XMonad.Layout.CenteredMaster
import XMonad.Layout.Tabbed
import XMonad.Layout.LayoutCombinators
import XMonad.Layout.SubLayouts
import XMonad.Layout.Renamed
import XMonad.Layout.NoFrillsDecoration
import XMonad.Layout.Circle
import XMonad.Layout.WindowNavigation
import XMonad.Layout.Simplest

--Hooks

import XMonad.Hooks.ManageDocks
import XMonad.Hooks.DynamicLog 
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.UrgencyHook

-- Actions
import XMonad.Actions.CycleWS
import XMonad.Actions.RotSlaves

-- Utils
import XMonad.Util.NamedScratchpad




import XMonad.Layout.Groups.Examples
import XMonad.Prompt.Shell
import XMonad.Prompt

import Graphics.X11.ExtraTypes.XF86

import XMonad.Actions.GridSelect
import XMonad.Actions.NoBorders
import XMonad.Util.SpawnNamedPipe
import Data.Maybe
import XMonad.Util.Run
import XMonad.Util.SpawnOnce
import XMonad.Actions.UpdatePointer

-- my stuff end





-- The preferred terminal program, which is used in a binding below and by
-- certain contrib modules.
--
--myTerminal      = "kitty"
myTerminal      = "st"

-- Whether focus follows the mouse pointer.
myFocusFollowsMouse :: Bool
myFocusFollowsMouse = True

-- Whether clicking on a window to focus also passes the click to the window
myClickJustFocuses :: Bool
myClickJustFocuses = False

-- Width of the window border in pixels.
myBorderWidth   = 2
--
myModMask       = mod1Mask
--myModMask = mod4Mask

-- The default number of workspaces (virtual screens) and their names.
-- By default we use numeric strings, but any string may be used as a
-- workspace name. The number of workspaces is determined by the length
-- of this list.
--
-- A tagging example:
--
-- > workspaces = ["web", "irc", "code" ] ++ map show [4..9]
--
myWorkspaces    = ["1","2","3","4","5","6","7","8","卑"]

-- Border colors for unfocused and focused windows, respectively.
--
--myNormalBorderColor  = "#5f676a"
myNormalBorderColor  = "#0f1126"
--myFocusedBorderColor = "#44bcd8"
myFocusedBorderColor = "orange"

------------------------------------------------------------------------
-- Key bindings. Add, modify or remove key bindings here.
--
myKeys conf@(XConfig {XMonad.modMask = modm}) = M.fromList $

    -- launch a terminal
    [ ((modm,  xK_Return), spawn $ XMonad.terminal conf)

    -- launch rofi
    , ((modm,               xK_d     ), spawn "rofi -show run")

    -- close focused window
    , ((modm .|. shiftMask, xK_c     ), kill)

     -- Rotate through the available layout algorithms
    , ((modm,               xK_space ), sendMessage NextLayout)

    --  Reset the layouts on the current workspace to default
    , ((modm, xK_r ), setLayout $ XMonad.layoutHook conf)

    -- Resize viewed windows to the correct size
    , ((modm,               xK_n     ), refresh)

    -- Move focus to the next window
    , ((modm,               xK_Tab   ), windows W.focusDown)

    -- Move focus to the next window
    , ((modm,               xK_j     ), windows W.focusDown)

    -- Move focus to the previous window
    , ((modm,               xK_k     ), windows W.focusUp  )

    -- Move focus to the master window
    , ((modm,               xK_m     ), windows W.focusMaster  )

    -- Swap the focused window and the master window
    , ((modm .|. shiftMask, xK_Return), windows W.swapMaster)

    -- Swap the focused window with the next window
    , ((modm .|. shiftMask, xK_j     ), windows W.swapDown  )

    -- Swap the focused window with the previous window
    , ((modm .|. shiftMask, xK_k     ), windows W.swapUp    )

    -- Shrink the master area
    , ((modm,               xK_h     ), sendMessage Shrink)

    -- Expand the master area
    , ((modm,               xK_l     ), sendMessage Expand)

    -- Push window back into tiling
    , ((modm,               xK_t     ), withFocused $ windows . W.sink)

    -- Increment the number of windows in the master area
    , ((modm              , xK_comma ), sendMessage (IncMasterN 1))

    -- Deincrement the number of windows in the master area
    , ((modm              , xK_period), sendMessage (IncMasterN (-1)))

    -- Exit Xmonad
    , ((modm .|. shiftMask, xK_q     ), io (exitWith ExitSuccess))
 

    -- Restart xmonad
    , ((modm              , xK_q     ), spawn "xmonad --recompile; xmonad --restart")

    -- Run xmessage with a summary of the default keybindings (useful for beginners)
    , ((modm .|. shiftMask, xK_slash),spawn ("echo \"" ++ help ++ "\" | xmessage -file -"))

    -- Lower volume 
    , ((0, xF86XK_AudioLowerVolume ), spawn ("pactl set-sink-volume @DEFAULT_SINK@ -1%"))

    -- Raise volume 
    , ((0, xF86XK_AudioRaiseVolume ), spawn ("pactl set-sink-volume @DEFAULT_SINK@ +1%"))

    -- Mute
    , ((0, xF86XK_AudioMute        ), spawn ("pactl set-sink-mute @DEFAULT_SINK@ toggle"))
    
    --Play and pause
    , ((0, xF86XK_AudioPlay        ), spawn ("~/.scripts/players.sh"))

  --Pause
  , ((0, xF86XK_AudioStop        ), spawn ("~/.scripts/players.sh 1"))

    -- next
    , ((0, xF86XK_AudioNext        ), spawn ("playerctl next"))
    
    -- Previous
    , ((0, xF86XK_AudioPrev        ), spawn ("playerctl previous"))

    -- seek forward
    , ((modm, xF86XK_AudioRaiseVolume ), spawn ("~/.scripts/players.sh 3"))

    -- seek backwards
    , ((modm, xF86XK_AudioLowerVolume), spawn ("~/.scripts/players.sh 2"))

    -- Take SS
    , ((modm .|. shiftMask, xK_s), spawn ("flameshot gui"))

--------------------------------------------------------------------------------
-- Layout Section

-- Go to layout
    -- Toggle Full layout 
    , ((modm,   xK_f),  sendMessage $ ToggleLayout)

    -- Go to Grid
    , ((modm, xK_g),    sendMessage $ JumpToLayout "Spacing Grid") 
    
    -- Go to tiled 
    , ((modm, xK_s),    sendMessage $ JumpToLayout "Super Tall") 

-- Group Layout Keybinds 
    
    -- Pull into group
    , ((modm .|. controlMask, xK_h), sendMessage $ pullGroup L)
    , ((modm .|. controlMask, xK_l), sendMessage $ pullGroup R)
    , ((modm .|. controlMask, xK_k), sendMessage $ pullGroup U)
    , ((modm .|. controlMask, xK_j), sendMessage $ pullGroup D)

    -- Merge all windows into group and remove window from group
    , ((modm .|. controlMask, xK_m), withFocused (sendMessage . MergeAll))
    , ((modm .|. controlMask, xK_u), withFocused (sendMessage . UnMerge))

    -- navigate group
    , ((modm .|. controlMask, xK_period), onGroup W.focusUp')
    , ((modm .|. controlMask, xK_comma), onGroup W.focusDown')

    -- Switch SubLayout's Layout
    , ((modm .|. controlMask, xK_space), toSubl NextLayout)

-- Window navigation (ignores group's inner windows)
    , ((modm,                 xK_Right), sendMessage $ Go R)
    , ((modm,                 xK_Left ), sendMessage $ Go L)
    , ((modm,                 xK_Up   ), sendMessage $ Go U)
    , ((modm,                 xK_Down ), sendMessage $ Go D)
    , ((modm .|. controlMask, xK_Right), sendMessage $ Swap R)
    , ((modm .|. controlMask, xK_Left ), sendMessage $ Swap L)
    , ((modm .|. controlMask, xK_Up   ), sendMessage $ Swap U)
    , ((modm .|. controlMask, xK_Down ), sendMessage $ Swap D)

-- Layout
    , ((modm .|. controlMask, xK_period), rotSlavesUp)

--------------------------------------------------------------------------------
-- 
    -- Toggle the status bar gap
    , ((modm .|. shiftMask, xK_space), sendMessage ToggleStruts)

    -- Toggle screen
    , ((mod4Mask,   xK_Tab          ), swapNextScreen   )

    -- Launch file manager
    , ((mod4Mask,   xK_e            ), spawn "nemo"   )



    ]
    
    ++

    --
    -- mod-[1..9], Switch to workspace N
    -- mod-shift-[1..9], Move client to workspace N
    --
    [((m .|. modm, k), windows $ f i)
        | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
        , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]
    ++

    --
    -- mod-{w,e,r}, Switch to physical/Xinerama screens 1, 2, or 3
    -- mod-shift-{w,e,r}, Move client to screen 1, 2, or 3
    --
    [((m .|. modm, key), screenWorkspace sc >>= flip whenJust (windows . f))
        | (key, sc) <- zip [xK_w, xK_e, xK_r] [0..]
        , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]


------------------------------------------------------------------------
-- Mouse bindings: default actions bound to mouse events
--
myMouseBindings (XConfig {XMonad.modMask = modm}) = M.fromList $

    -- mod-button1, Set the window to floating mode and move by dragging
    [ ((modm, button3), (\w -> focus w >> mouseMoveWindow w
                                       >> windows W.shiftMaster))

    -- mod-button2, Set the window to floating mode and resize by dragging
    , ((modm, button2), (\w -> focus w >> mouseResizeWindow w
                                       >> windows W.shiftMaster))

    -- mod-button5, Cycle ws backwards
    , ((modm, button5), \w -> moveTo Next HiddenWS )

    -- mod-button5, Cycle ws forward
    , ((modm, button4), \w -> moveTo Prev HiddenWS )

    ]





-------------------------------------------------------------------------------
-- Layouts:

-- You can specify and transform your layouts by modifying these values.
-- If you change layout bindings be sure to use 'mod-shift-space' after
-- restarting (with 'mod-q') to reset your layout state to the new
-- defaults, as xmonad preserves your old layout settings by default.
--
-- The available layouts.  Note that each layout is separated by |||,
-- which denotes layout choice.

gaps i = spacingRaw True (Border i i i i) True (Border i i i i) True 

myLayout  = toggleLayouts full 
            (   
            smartBorders 
                tiled2 
            ||| twopanes
            ||| grid
            ||| circle
            ) 

full      = noBorders Full 

twopanes  = lessBorders Screen
            $ avoidStruts 
            $ gaps 3
            $ TwoPane (3/100) (1/2)
            
grid      = lessBorders Screen
            $ avoidStruts
            $ windowNavigation
            $ gaps 3
              Grid 

tiled2      = renamed [Replace "Super Tall"] 
            $ lessBorders Screen
            $ avoidStruts 
            $ windowNavigation 
            $ addTabs shrinkText myTheme
            $ subLayout [] (Simplest ||| Circle)
            $ gaps 3 $ Tall 1 (3/100) (1/2) 


circle      = avoidStruts
            $ Circle

myTheme :: Theme
myTheme = def { activeColor         = myFocusedBorderColor
              , activeBorderColor   = myFocusedBorderColor 
              , inactiveColor       = myNormalBorderColor
              , inactiveBorderColor = myNormalBorderColor
              , fontName            = "xft:JetBrainsMono:pixelsize=10"
              }

------------------------------------------------------------------------
-- Window rules:

-- Execute arbitrary actions and WindowSet manipulations when managing
-- a new window. You can use this to, for example, always float a
-- particular program, or have a client always appear on a particular
-- workspace.
--
-- To find the property name associated with a program, use
-- > xprop | grep WM_CLASS
-- and click on the client you're interested in.
--
-- To match on the WM_NAME, you can use 'title' in the same way that
-- 'className' and 'resource' are used below.
--
myManageHook = composeAll
    [ className =? "MPlayer"        --> doFloat
    , className =? "Gimp"           --> doFloat
    , className =? "Steam"          --> doFloat
    , className =? "Mumble"         --> doFloat
    , className =? "Terraria.bin.x86_64" --> hasBorder False
    , className =? "firefox"        --> hasBorder False
    , className =? "mpdevil"        --> hasBorder False
    , resource  =? "desktop_window" --> doIgnore
    , resource  =? "kdesktop"       --> doIgnore ]


------------------------------------------------------------------------
-- Event handling

-- * EwmhDesktops users should change this to ewmhDesktopsEventHook
--
-- Defines a custom handler function for X Events. The function should
-- return (All True) if the default handler is to be run afterwards. To
-- combine event hooks use mappend or mconcat from Data.Monoid.
--
myEventHook = mempty

------------------------------------------------------------------------
-- Status bars and logging

-- Perform an arbitrary action on each internal state change or X event.
-- See the 'XMonad.Hooks.DynamicLog' extension for examples.

myxmobarPP = def {     
           ppCurrent = currentPP
         , ppVisible = visiblePP
         , ppHiddenNoWindows = xmobarColor "darkgreen" ""
         , ppHidden  = xmobarColor "orange" "" . wrap "" ""
         , ppLayout  = xmobarColor "purple"  "" . wrap "" ""
         , ppTitle   = xmobarColor "green" "" . shorten 40 . wrap "" ""
         , ppUrgent  = xmobarColor "red" ""
         , ppSep     = "   "
         } 
    where
        currentPP :: WorkspaceId -> String
        currentPP x = xmobarColor "#44bcd8" "" 
                    $ "<box type=Bottom width=2 color=#44bcd8><fn=3>ﬦ</fn>" ++ x ++"</box>"
        visiblePP :: WorkspaceId -> String
        visiblePP x = xmobarColor "yellow" "" 
                    $ "<box type=Bottom width=2 color=#FFFF00><fn=3>\xf52a</fn>" ++ x ++ "</box>"

myLogHook = do 
    updatePointer (0.5, 0.5) (0,0) 
    a <- getNamedPipe "xmobarmain"
    dynamicLogWithPP $ myxmobarPP {ppOutput = maybe (\x -> return()) hPutStrLn a}
    b <- getNamedPipe "xmobarsec"
    dynamicLogWithPP $ myxmobarPP { ppOutput = maybe (\x -> return()) hPutStrLn b
                                  , ppOrder = id
                                  } 


------------------------------------------------------------------------
-- Startup hook

-- Perform an arbitrary action each time xmonad starts or is restarted
-- with mod-q.  Used by, e.g., XMonad.Layout.PerWorkspace to initialize
-- per-workspace layout choices.
--
-- By default, do nothing.
myStartupHook = do 
    spawnNamedPipe "xmobar ~/.xmonad/xmobars/xmobar1" "xmobarmain"
    spawnNamedPipe "xmobar ~/.xmonad/xmobars/xmobar2" "xmobarsec"
    spawn "/usr/bin/trayer --edge top --align left --SetDockType true --SetPartialStrut true --expand true --widthtype request --transparent true --alpha 50 --tint 000000 --height 19 --monitor primary"
    spawn "xss-lock ~/.scripts/lock.sh"






------------------------------------------------------------------------
-- Now run xmonad with all the defaults we set up.

-- Run xmonad with the settings you specify. No need to modify this.
--
main = xmonad $ ewmh $ docks $ withUrgencyHook NoUrgencyHook $ defaults

-- A structure containing your configuration settings, overriding
-- fields in the default config. Any you don't override, will
-- use the defaults defined in xmonad/XMonad/Config.hs
--
-- No need to modify this.

defaults = def {
      -- simple stuff
        terminal           = myTerminal,
        focusFollowsMouse  = myFocusFollowsMouse,
        clickJustFocuses   = myClickJustFocuses,
        borderWidth        = myBorderWidth,
        modMask            = myModMask,
        workspaces         = myWorkspaces,
        normalBorderColor  = myNormalBorderColor,
        focusedBorderColor = myFocusedBorderColor,

      -- key bindings
        keys               = myKeys,
        mouseBindings      = myMouseBindings,

      -- hooks, layouts
        layoutHook         = myLayout,
        manageHook         = myManageHook,
        handleEventHook    = myEventHook,
        logHook            = myLogHook, 
        startupHook        = myStartupHook
    } 


-- | Finally, a copy of the default bindings in simple textual pabular format.
help :: String
help = unlines ["The default modifier key is 'alt'. Default keybindings:",
    "",
    "-- launching and killing programs",
    "mod-Shift-Enter  Launch xterminal",
    "mod-p            Launch dmenu",
    "mod-Shift-p      Launch gmrun",
    "mod-Shift-c      Close/kill the focused window",
    "mod-Space        Rotate through the available layout algorithms",
    "mod-Shift-Space  Reset the layouts on the current workSpace to default",
    "mod-n            Resize/refresh viewed windows to the correct size",
    "",
    "-- move focus up or down the window stack",
    "mod-Tab        Move focus to the next window",
    "mod-Shift-Tab  Move focus to the previous window",
    "mod-j          Move focus to the next window",
    "mod-k          Move focus to the previous window",
    "mod-m          Move focus to the master window",
    "",
    "-- modifying the window order",
    "mod-Return   Swap the focused window and the master window",
    "mod-Shift-j  Swap the focused window with the next window",
    "mod-Shift-k  Swap the focused window with the previous window",
    "",
    "-- resizing the master/slave ratio",
    "mod-h  Shrink the master area",
    "mod-l  Expand the master area",
    "",
    "-- floating layer support",
    "mod-t  Push window back into tiling; unfloat and re-tile it",
    "",
    "-- increase or decrease number of windows in the master area",
    "mod-comma  (mod-,)   Increment the number of windows in the master area",
    "mod-period (mod-.)   Deincrement the number of windows in the master area",
    "",
    "-- quit, or restart",
    "mod-Shift-q  Quit xmonad",
    "mod-q        Restart xmonad",
    "mod-[1..9]   Switch to workSpace N",
    "",
    "-- Workspaces & screens",
    "mod-Shift-[1..9]   Move client to workspace N",
    "mod-{w,e,r}        Switch to physical/Xinerama screens 1, 2, or 3",
    "mod-Shift-{w,e,r}  Move client to screen 1, 2, or 3",
    "",
    "-- Mouse bindings: default actions bound to mouse events",
    "mod-button1  Set the window to floating mode and move by dragging",
    "mod-button2  Raise the window to the top of the stack",
    "mod-button3  Set the window to floating mode and resize by dragging"]


