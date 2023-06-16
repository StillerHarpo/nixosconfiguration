{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}

import Bookmarks (bookmarks, workBookmarks)
import Data.List (last)
import qualified Data.Map as M
import qualified Data.Text as T 
import qualified Data.Text.Lazy as T (toStrict)
import Data.Word (Word32)
import Protolude
import Text.Pretty.Simple
import Utils
import XMonad hiding (ifM)
import XMonad.Actions.CycleWS (nextScreen, swapNextScreen)
import XMonad.Actions.FindEmptyWorkspace (viewEmptyWorkspace)
import XMonad.Actions.MessageFeedback (sendMessageB)
import qualified XMonad.Actions.Search as T
import XMonad.Actions.SpawnOn (manageSpawn)
import XMonad.Actions.UpdatePointer (updatePointer)
import XMonad.Hooks.EwmhDesktops (ewmh, ewmhFullscreen, fullscreenEventHook)
import XMonad.Hooks.SetWMName (setWMName)
import XMonad.Hooks.UrgencyHook
  ( UrgencyHook (..),
    withUrgencyHook,
  )
import XMonad.Layout.Column (Column (Column))
import XMonad.Layout.Gaps (Direction2D (L, R), gaps)
import qualified XMonad.Layout.Groups as G
import qualified XMonad.Layout.Groups.Helpers as GH
import XMonad.Layout.NoBorders (noBorders, smartBorders)
import qualified XMonad.StackSet as W
import XMonad.Util.DebugWindow (debugWindow)
import XMonad.Util.Dmenu (menuArgs)
import XMonad.Util.Dzen
  ( addArgs,
    center,
    dzenConfig,
    font,
    onCurr,
    timeout,
  )
import XMonad.Util.EZConfig (additionalKeys, additionalKeysP)
import qualified XMonad.Util.ExtensibleState as XS
import XMonad.Util.NamedWindows (getName)
import XMonad.Util.Run (runProcessWithInput, safeSpawn)
import XMonad.Util.Stack (Zipper, insertUpZ, onFocusedZ)
import Xrandr

data LibNotifyUrgencyHook = LibNotifyUrgencyHook deriving (Read, Show)

newtype ListStorage = ListStorage [Window] deriving (Typeable)

instance ExtensionClass ListStorage where
  initialValue = ListStorage []

termCommand :: IsString a => a
termCommand = "alacritty"

main :: IO ()
main =
  xmonad $
    withUrgencyHook LibNotifyUrgencyHook $
      ewmhFullscreen
        def
          { terminal = termCommand,
            modMask = mod4Mask,
            workspaces = map show [1 .. 20],
            layoutHook = smartBorders $ noBorders myLayout,
            logHook = updatePointer (0.5, 0.5) (0, 0),
            manageHook = myManageHooks,
            startupHook = do
              setWMName "LG3D"
              spawn "feh --bg-scale ~/black.png"
          }
        `additionalKeys` ( [ ((0, 0x1008ff13), volume Plus),
                             ((0, 0x1008ff11), volume Minus),
                             ((0, 0x1008ff12), mute),
                             ((mod4Mask, 0x63), clock),
                             ((mod4Mask, xK_x), spawn toggleRedshift),
                             ((mod4Mask, xK_y), toggleMonitor),
                             ((mod4Mask, xK_Return), spawn termCommand),
                             ((mod4Mask .|. shiftMask, xK_Return), spawnOnEmpty termCommand),
                             ((mod4Mask, xK_v), spawn emacs),
                             ((mod4Mask, xK_b), printDebug),
                             ((mod4Mask .|. shiftMask, xK_v), spawnOnEmpty emacs),
                             ( (mod4Mask, xK_n),
                               runProcess "networkmanager_dmenu" . ("-font" :) . (: []) =<< getFontFromScreenWidth
                             ),
                             ((mod4Mask, xK_m), spawn "systemctl suspend -i && XSECURELOCK_COMPOSITE_OBSCURER=0 xsecurelock"),
                             ((mod4Mask, xK_u), spawn "sleep 2 && XSECURELOCK_COMPOSITE_OBSCURER=0 xsecurelock"),
                             ( (mod4Mask, xK_f),
                               composeAll
                                 [ runOrShift,
                                   saveFocus
                                 ]
                             ),
                             ( (mod4Mask .|. shiftMask, xK_f),
                               composeAll
                                 [ runOrRaise,
                                   saveFocus
                                 ]
                             ),
                             ((mod4Mask, xK_g), goToNotify),
                             ((mod4Mask, xK_j), GH.focusUp),
                             -- TODO Debug why this sometimes don't work
                             ((mod4Mask, xK_k), GH.focusDown),
                             ((mod4Mask, xK_h), shrinkMasterGroups),
                             ((mod4Mask, xK_l), expandMasterGroups),
                             ((mod4Mask .|. shiftMask, xK_j), GH.swapUp),
                             ((mod4Mask .|. shiftMask, xK_k), GH.swapDown),
                             ((mod4Mask, xK_s), GH.focusGroupMaster),
                             ((mod4Mask, xK_a), GH.focusGroupUp),
                             ((mod4Mask, xK_d), GH.focusGroupDown),
                             ((mod4Mask .|. shiftMask, xK_a), GH.moveToGroupUp False),
                             ((mod4Mask .|. shiftMask, xK_d), GH.moveToGroupDown False),
                             ((mod4Mask, xK_p), playPause),
                             ((mod4Mask, xK_i), playPrevious),
                             ((mod4Mask, xK_o), playNext),
                             -- rematch workspace keys to take workspaces in to account
                             ( (mod4Mask, xK_w),
                               composeAll
                                 [ nextScreen,
                                   saveFocus
                                 ]
                             ),
                             ( (mod4Mask, xK_e),
                               composeAll
                                 [ swapNextScreen,
                                   saveFocus
                                 ]
                             )
                           ]
                             ++ [ ((mod4Mask, k), saveView i)
                                  | (i, k) <- zip [1 .. 9] [xK_1 .. xK_9]
                                ]
                         )
        `additionalKeysP` [ ("<XF86MonBrightnessUp>", backlight Plus),
                            ("<XF86MonBrightnessDown>", backlight Minus),
                            ("<XF86AudioPlay>", playPause),
                            ("<XF86AudioPrev>", playPrevious),
                            ("<XF86AudioNext>", playNext)
                          ]
  where
    spawnOnEmpty prog = composeAll [viewEmptyWorkspace, spawn prog, saveFocus]
    saveView i = composeAll [windows . W.greedyView $ show i, saveFocus]
    emacs = "emacsclient -c ~/Dokumente/init.org"
    dbus = "dbus-send --print-reply "
    dest = "--dest=org.mpris.MediaPlayer2.spotify "
    org = "/org/mpris/MediaPlayer2 org.mpris.MediaPlayer2.Player."
    playPause = spawn $ dbus ++ dest ++ org ++ "PlayPause"
    playNext = spawn $ dbus ++ dest ++ org ++ "Next"
    playPrevious = spawn $ dbus ++ dest ++ org ++ "Previous"
    toggleRedshift =
      "systemctl --user is-active redshift.service"
        ++ " && systemctl --user stop redshift.service"
        ++ " || systemctl --user start redshift.service"

toggleMonitor :: MonadIO m => m ()
toggleMonitor =
  findM (fmap isConnected . monitorStatus) ["DP-1", "DP-2", "HDMI-A-1", "HDMI-A-2"]
    >>= maybe
      (pure ())
      ( \extMonitor ->
          ifM
            (isEnabled <$> monitorStatus "eDP-1")
            ( runProcess
                "xrandr"
                [ "--output",
                  sysToRandr extMonitor,
                  "--auto",
                  "--output",
                  "eDP-1",
                  "--off"
                ]
            )
            (runProcess "xrandr" (toEnableOption extMonitor))
      )

-- Key scripts --
myDzenConfig :: Int -> Text -> X ()
myDzenConfig len = dzenConfig (timeout 1 >=> centered) . T.unpack
  where
    centered =
      onCurr (center len 66)
        >=> font "-adobe-helvetica-*-*-*-*-24-*-*-*-*-*-*-*"
        >=> addArgs ["-fg", "#10bb20"]

clockText :: X Text
clockText = do
  date <- runProcessWithInput "date" ["+%H:%M:%S%n%A, %d. %B %Y"] []
  battery <- io $ readFile "/sys/class/power_supply/BAT0/capacity"
  pure . T.replace "\n" " " $ T.pack date <> "     Battery: " <> T.init battery <> "%"

clock :: X ()
clock = clockText >>= myDzenConfig 700

mute :: X ()
mute = script >> liftIO output >>= myDzenConfig 300
  where
    script = runProcessWithInput "pamixer" ["-t"] ""
    output =
      (\(T.pack -> res) -> if T.isInfixOf "true" res then "Muted" else "Unmuted")
        <$> catch @IOException
          (runProcessWithInput "pamixer" ["--get-mute"] "")
          (const $ pure "")

data Direction = Plus | Minus

volume :: Direction -> X ()
volume vol = script >> T.pack <$> liftIO output >>= myDzenConfig 300
  where
    script = runProcessWithInput "pamixer" [volChar vol ++ "5"] ""
    volChar Plus = "-i"
    volChar Minus = "-d"
    output =
      catch @IOException
        (runProcessWithInput "pamixer" ["--get-volume", "sinks"] "")
        (const $ pure "Command failed")

backlight :: Direction -> X ()
backlight dir = void $ runProcessWithInput "light" [dirOp dir, "10"] ""
  where
    dirOp Plus = "-A"
    dirOp Minus = "-U"

-- Dont work use instead m,n
-- 0x1008ff02 = XFMonBrightnessUp
-- 0x1008ff03 = XFMonBrightnessDown

-- | Shrink the master area
shrinkMasterGroups :: X ()
shrinkMasterGroups = sendMessage $ G.ToEnclosing $ SomeMessage Shrink

-- | Expand the master area
expandMasterGroups :: X ()
expandMasterGroups = sendMessage $ G.ToEnclosing $ SomeMessage Expand

myManageHooks =
  composeAll
    [ manageSpawn <+> manageHook def,
      doSink
    ]

doSink :: ManageHook
doSink = ask >>= \w -> liftX (reveal w) >> doF (W.sink w)

myLayout = Full ||| shrinked ||| tabbed
  where
    shrinked = gaps [(L, 300), (R, 300)] Full
    -- tabbed   = G.group T.simpleTabbed $ Mirror $ Column 1
    tabbed = G.group Full $ Mirror $ Column 1

-- | programms that can be started from rofi
programms ::
  -- | (name in dmenu, executable)
  [(Text, Text)]
programms =
  [ ("firefox", "firefox"),
    ("chromium", "chromium"),
    ("anki", "anki"),
    ("kodi", "kodi"),
    ("signal", "signal-desktop"),
    ("element", "element-desktop"),
    ("steam", "steam"),
    ("mu4e", emacs "mu4e"),
    ("elfeed", emacs "elfeed"),
    ("tor", "tor-browser"),
    ("alacritty", termCommand),
    ("pavucontrol", "pavucontrol"),
    ("arandr", "arandr"),
    ("blueman-manager", "blueman-manager"),
    ("mullvad-gui", "mullvad-gui"),
    ("windows-vnc", "vncviewer 192.168.178.24:5900"),
    ("linux-vnc", "vncviewer localhost:9900"),
    ("remmina", "remmina"),
    ("slack", "slack")
  ]
    ++ map (second browser) bookmarks
    ++ map (second (browserWithProfile "work")) workBookmarks
  where
    startTerm s =
      termCommand
        <> " -t \""
        <> s
        <> "\" -e \""
        <> s
        <> "\""
    emacs s =
      "emacsclient -c -e \"("
        <> s
        <> ")\""
        <> maximizeEmacs
    maximizeEmacs = " -e \"(spacemacs/toggle-maximize-buffer)\""

-- | save the current window, so you can later come back with goToNotify
saveFocus :: X ()
saveFocus = do
  workspace <- W.stack . W.workspace . W.current <$> gets windowset
  let window = W.focus <$> workspace
  whenJust
    window
    (XS.modify . changeHead)

changeHead :: Window -> ListStorage -> ListStorage
changeHead _ l@(ListStorage []) = l
changeHead w (ListStorage (_ : ws)) = ListStorage $ w : ws

-- | cycle through the windows that triggered urgency and go back to the window,
--   you came from
goToNotify :: X ()
goToNotify = do
  w <- lastStorage <$> XS.get
  XS.modify deleteN
  windows $ greedyFocusWindow w

lastStorage :: ListStorage -> Window
lastStorage (ListStorage xs) = last xs

deleteN :: ListStorage -> ListStorage
deleteN l@(ListStorage [_]) = l
deleteN (ListStorage xs) = ListStorage $ initSafe xs

-- | if a window triggers a urgency, save the workspace on which it is
--   and show a notification
instance UrgencyHook LibNotifyUrgencyHook where
  urgencyHook LibNotifyUrgencyHook w = do
    name <- getName w
    unless (match $ show name) $ do
      Just idx <- W.findTag w <$> gets windowset
      safeSpawn "notify-send" ["-t 3000", show name, "workspace " ++ idx]
      XS.modify $ addCont w
    where
      unwantedNotifcations = ["qutebrowser"]
      match n = any (matchEnd n) unwantedNotifcations
      matchEnd n uN = take (length uN) (reverse n) == reverse uN

-- | add the workspace at the end of the list, if it isn't already in it
addCont :: Window -> ListStorage -> ListStorage
addCont w l@(ListStorage ws)
  | w `elem` ws = l
  | otherwise = ListStorage $ ws ++ [w]

-- | open url in browser with specified profile
browserWithProfile :: Text -> Text -> Text
browserWithProfile profile url = "firefox -P " <> profile <> " -new-window \"" <> urlWithSearch <> "\""
  where
    urlWithSearch =
      if (T.any (== '.') url || T.isInfixOf "localhost" url) && not (T.any (== ' ') url)
        then url
        else "duckduckgo.com\\?q=" <> replaceSpaces url

-- | open url in browser
browser :: Text -> Text
browser = browserWithProfile "default"

getFontFromScreenWidth :: X Text
getFontFromScreenWidth = do
  getScreenWidth
    <&> \case
      2560 -> "mono 20"
      _ -> "mono 16"

-- | Calls dmenuMap to grab the appropriate Window or program, and hands it off to action
--   if found .
actionMenu ::
  -- | the action
  (Window -> X ()) ->
  -- | should the program be open on a new workspace
  Bool ->
  X ()
actionMenu action viewEmpty = do
  time <- clockText
  font <- getFontFromScreenWidth
  let menuMapFunction =
        menuMapArgsDefault
          "rofi"
          [ "-dmenu",
            "-font",
            font,
            "-i",
            "-p",
            "window",
            "-mesg",
            time
          ]
          defaultAction
  join (windowMap >>= menuMapFunction)
  where
    defaultAction url =
      when (T.length url > 0) $
        composeAll $
          [viewEmptyWorkspace | viewEmpty]
            ++ [spawn . T.unpack $ browser url]
    windowMap :: X (M.Map Text (X ()))
    windowMap = do
      ws <- gets $ W.workspaces . windowset
      M.fromList . (++ openProgramms) . toAction . concat
        <$> mapM keyValuePairs ws
      where
        keyValuePairs ws = mapM (keyValuePair ws) $ W.integrate' (W.stack ws)
        keyValuePair ws w = (,w) . T.toTitle . T.dropWhile (not . isAlpha) <$> decorateName ws w
        toAction = map $ second action
        openProgramms =
          map
            ( \(name, prog) ->
                ( name,
                  composeAll
                    ( [viewEmptyWorkspace | viewEmpty]
                        ++ [spawn (T.unpack prog), saveFocus]
                    )
                )
            )
            programms

runOrRaise :: X ()
runOrRaise = actionMenu (windows . greedyFocusWindow) True

runOrShift :: X ()
runOrShift = actionMenu action False
  where
    action w = do
      idx <- W.currentTag <$> gets windowset
      windows $ delWin idx w
      windows $ W.insertUp w
      insertUp w

getScreenWidth :: X Word32
getScreenWidth = rect_width . screenRect . W.screenDetail . W.current . windowset <$> get

-- | Returns the window name as will be listed in dmenu.
--   Tagged with the workspace ID, to guarantee uniqueness, and to let
--   the user know where they going.
decorateName :: WindowSpace -> Window -> X Text
decorateName ws w = do
  name <- resClass <$> withDisplay (\d -> io $ getClassHint d w)
  displayName <- show <$> getName w
  return $
    T.pack name
      <> " - "
      <> displayName
      <> " ["
      <> T.pack (W.tag ws)
      <> "]"

-- | temporally deletes a window on a workspace
delWin :: (Ord a, Eq s, Eq i) => i -> a -> W.StackSet i l a s sd -> W.StackSet i l a s sd
delWin n w s = case W.findTag w s of
  Just from | n `W.tagMember` s -> go from s
  _ -> s
  where
    go from = onWorkspace from (W.delete' w)

-- | Like 'menuMapArgs' but also takes a default command
menuMapArgsDefault ::
  MonadIO m =>
  Text ->
  [Text] ->
  (Text -> a) ->
  M.Map Text a ->
  m a
menuMapArgsDefault menuCmd args def selectionMap = do
  selection <- menuFunction . map T.unpack $ M.keys selectionMap
  return $ fromMaybe (def selection) $ M.lookup selection selectionMap
  where
    menuFunction = fmap T.pack . menuArgs (T.unpack menuCmd) (map T.unpack args)

-- | is not exported from Stackset
onWorkspace ::
  (Eq i, Eq s) =>
  i ->
  (W.StackSet i l a s sd -> W.StackSet i l a s sd) ->
  (W.StackSet i l a s sd -> W.StackSet i l a s sd)
onWorkspace n f s = W.view (W.currentTag s) . f . W.view n $ s

-- | inserts a window over the currently focused one
insertUp w = alt (onFocused $ insertUpZ w) (W.insertUp w)

-- | Group helpers
onFocused :: (Zipper Window -> Zipper Window) -> G.ModifySpec
onFocused f _ = onFocusedZ (G.onZipper f)

alt :: G.ModifySpec -> (WindowSet -> WindowSet) -> X ()
alt f g = alt2 (G.Modify f) $ windows g

alt2 :: G.GroupsMessage -> X () -> X ()
alt2 m x = do
  b <- sendMessageB m
  unless b x

printDebug :: X ()
printDebug = do
  x <- get
  liftIO $ writeFile "/home/florian/debugXmonad.hs" (T.toStrict $ pShow (windowset x))

replaceSpaces :: Text -> Text
replaceSpaces =
  T.map
    ( \case
        ' ' -> '+'
        c -> c
    )
    . T.strip

greedyFocusWindow :: (Eq s, Eq a, Eq i) => a -> W.StackSet i l a s sd -> W.StackSet i l a s sd
greedyFocusWindow w s
  | Just w == W.peek s = s
  | otherwise = fromMaybe s $ do
      n <- W.findTag w s
      return $ until ((Just w ==) . W.peek) W.focusUp (W.greedyView n s)
