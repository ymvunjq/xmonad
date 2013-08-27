{-# OPTIONS_GHC -XNoMonomorphismRestriction #-}
{-# LANGUAGE DeriveDataTypeable #-}

-------------------------------------------------------------------------------
-- |
-- Module : DynamicActivity.hs
-- Author : ymvunjq@gmail.com
--
-- Add dynamic set of workspaces called activity
-------------------------------------------------------------------------------


module DynamicActivity
(
	  print_activities
	, debugActivity
	, dropActivityName
	, filterWorkspaces
	, gotoActivity
	, prevActivity
	, nextActivity
	, gotoActivityWorkspace
	, gotoNextActivityWorkspace
	, gotoPrevActivityWorkspace
	, shifttoActivityWorkspace
	, shiftToNextActivityWorkspace
	, shiftToPrevActivityWorkspace
	, delCurrentActivityWorkspace
	, delCurrentActivity
	, promptActivityAdd
	, promptAddActivityWorkspace
        , promptRenameCurrentActivityWorkspace
) where

import Control.Monad
import Control.Applicative

import Data.List
import Data.Maybe (isNothing,isJust,fromJust)
import Codec.Binary.UTF8.String (encodeString)

import XMonad hiding (workspaces)
import qualified XMonad.StackSet as S
import XMonad.Prompt
import XMonad.Hooks.DynamicLog
import qualified XMonad.Util.ExtensibleState as XS
import XMonad.Actions.DynamicWorkspaces
import XMonad.Actions.CycleWS
import XMonad.Actions.DynamicWorkspaceOrder

import XMonad.Actions.OnScreen (viewOnScreen)
import XMonad.Actions.WithAll (withAll')
import XMonad.Hooks.UrgencyHook (readUrgents)

-- Should use renameWorkspaceByName from DynamicWorkspace, but not available in xmonad 0.10
import XMonad.Prompt.Workspace ( workspacePrompt )

active_activity_color = "#FFA500"
inactive_activity_color = "#585858"
urgent_activity_color = "#FF0000"
background_activity_color = "#161616"
no_activity = "*"
debug_activity = "Debug"
activity_workspace_separator = '-'
trash_workspace = "trash"

type ActivityId = String
type ActivityIndex = Int
type WorkspaceIndex = Int
type WorkspacePosition = Maybe (ScreenId,WorkspaceId)
type ScreenInfo = S.Screen WorkspaceId (Layout Window) Window ScreenId ScreenDetail

data Activity = Activity { name :: ActivityId
                         , current :: WorkspacePosition
                           -- OK... should be [WorkspacePosition] if we have more than 2 screens
                         , visible :: WorkspacePosition
                         } deriving (Typeable, Show, Read)

data ActivityStorage = AS [Activity] deriving (Typeable, Read, Show)
data CurrentActivity = CS Int deriving (Typeable, Read, Show)

instance ExtensionClass ActivityStorage where
  initialValue = AS $ []
  extensionType = PersistentExtension

instance ExtensionClass CurrentActivity where
  initialValue = CS $ 0
  extensionType = PersistentExtension


debugActivity :: X ()
debugActivity = do
  AS activities <- XS.get
  let str = intercalate "\n" . map (\x -> (name x) ++ " => Current:" ++ (showPosition . current $ x) ++ " Visible:" ++ (showPosition . visible $ x)) $ activities
  urgents <- readUrgents
  sst <- gets (windowset)
  let urg = show .length . map (fromJust) . filter (isJust) . map (\win -> S.findTag win sst) $ urgents
  spawn . concat $ ["xmessage '",str,"\nUrgent:",urg,"'"]
  where
    showPosition pos = case pos of
      Just x -> "(Screen:"++(show . screen $ x) ++ ",Wid:" ++ (workspace x) ++ ")"
      Nothing -> "Nothing"

-- Helpers
screen = fst
workspace = snd

-- | Return True if trash workspace exists
isTrashWorkspace :: X Bool
isTrashWorkspace = do
  ws <- gets (map S.tag . S.workspaces . windowset)
  return $ elem trash_workspace ws

-- | Output a list of strings, ignoring empty ones and separating the
--   rest with the given separator.
sepBy :: String   -- ^ separator
      -> [String] -- ^ fields to output
      -> String
sepBy sep = concat . intersperse sep

-- | Return current activity
getCurrentActivity :: X Activity
getCurrentActivity = do
  CS current <- XS.get
  AS activities <- XS.get
  return $ activityById current activities

-- | Return visible screen if it exists, Nothing otherwise (in mono screen case for example)
getVisible :: X (Maybe ScreenInfo)
getVisible = do
  vis <- gets (S.visible . windowset)
  return $ if (length vis) == 0 then Nothing else Just . head $ vis

-- | Return true if activity shall be tagged as urgent
isUrgentActivity :: ActivityId -> X Bool
isUrgentActivity actid = do
  urgents <- readUrgents
  sst <- gets (windowset)
  let ws = map (fromJust) . filter (isJust) . map (\win -> S.findTag win sst) $ urgents
  return $ any (\x -> belongToActivity x actid) ws

dzen_active = dzenColor active_activity_color background_activity_color
dzen_inactive = dzenColor inactive_activity_color background_activity_color
dzen_urgent = dzenColor urgent_activity_color background_activity_color

color :: Activity -> X String
color activity = do
  let n = (name activity)
  CS current <- XS.get
  AS activities <- XS.get
  urgent <- isUrgentActivity n
  if urgent then return (dzen_urgent n)
  else if n == (name (activityById current activities)) then return (dzen_active n)
       else return (dzen_inactive n)

colorize_activities :: [Activity] -> X String
colorize_activities as = fmap (sepBy " ") (mapM color as)

activityById :: ActivityIndex -> [Activity] -> Activity
activityById index list = list !! index

-- | Remove activity name from workspace name
dropActivityName :: String -> String
dropActivityName ws = do
  case (elemIndex activity_workspace_separator ws) of
    Just x -> drop (x+1) ws
    Nothing -> ws

-- | Return True if Workspace belongs to an activity
belongToAnActivity :: WorkspaceId -> Bool
belongToAnActivity ws = case (elemIndex activity_workspace_separator ws) of
  Just x -> True
  Nothing -> False

-- | Return True if ws belongs to activity act
belongToActivity :: WorkspaceId -> ActivityId -> Bool
belongToActivity ws act
  | act == debug_activity = True
  | act == no_activity = not. belongToAnActivity $ ws
  | otherwise = take (length act) ws == act

-- | Activate current workspace of activity if any
setActivityCurrentWS :: X ()
setActivityCurrentWS = do
  act <- getCurrentActivity
  case current act of
    Just x -> windows $ viewOnScreen (screen x) (workspace x)
    Nothing -> return ()

-- | Activate visible workspace of activity
setActivityVisibleWS :: X ()
setActivityVisibleWS = do
  act <- getCurrentActivity
  case visible act of
    Just x -> windows $ viewOnScreen (screen x) (workspace x)
    Nothing -> return ()

-- | Store Current activity context
storeCurrentActivityContext :: X ()
storeCurrentActivityContext = do
  AS activities <- XS.get
  CS act_index <- XS.get
  let act = activityById act_index activities
  ctag <- gets (S.current . windowset)
  vis <- getVisible
  case vis of
    Just v -> modifyStoredActivity act_index act{current=getTag (S.tag $ S.workspace ctag) (name act) (S.screen ctag, S.tag $ S.workspace ctag)
                                                ,visible=getTag (S.tag $ S.workspace v) (name act) ((S.screen v),(S.tag $ S.workspace v))}
    Nothing -> modifyStoredActivity act_index act{current=getTag (S.tag $ S.workspace ctag) (name act) (S.screen ctag, S.tag $ S.workspace ctag)
                                                 ,visible=Nothing}
    where
      getTag ws act val = if belongToActivity ws act then Just val else Nothing

-- | Restore Current Activity Context
restoreCurrentActivityContext :: X ()
restoreCurrentActivityContext = do
  setActivityVisibleWS
  setActivityCurrentWS


-- | Direct Access to activity
gotoActivity :: ActivityIndex -> X ()
gotoActivity index = do
  AS activities <- XS.get
  if (index + 1) > length activities then return ()
  else do
    storeCurrentActivityContext
    XS.put (CS $ index)
    restoreCurrentActivityContext

switchActivity :: Int -> X ()
switchActivity shift = do
  AS activities <- XS.get
  CS current <- XS.get
  gotoActivity ((current+shift) `mod` (length activities))

-- | Goto next Activity
nextActivity :: X ()
nextActivity = switchActivity 1

-- | Goto prev Activity
prevActivity :: X ()
prevActivity = switchActivity (-1)

-- | Generic function
dotoDirectionActivityWorkspace :: (WorkspaceId -> X ()) -> Direction1D -> X ()
dotoDirectionActivityWorkspace action direction = do
  doTo direction (WSIs wsExist) getSortByOrder action
  where
    wsExist = do
      act <- getCurrentActivity
      ws <- getActivityWorkspaces . name $ act
      return $ \x -> elem (S.tag x) ws


gotoDirectionActivityWorkspace :: Direction1D -> X ()
gotoDirectionActivityWorkspace = dotoDirectionActivityWorkspace (windows . S.greedyView)

shiftToDirectionActivityWorkspace :: Direction1D -> X ()
shiftToDirectionActivityWorkspace = dotoDirectionActivityWorkspace (windows . S.shift)

-- | Goto next Workspace belonging to activity
gotoNextActivityWorkspace :: X ()
gotoNextActivityWorkspace = gotoDirectionActivityWorkspace Next

-- | Goto prev Workspace belonging to activity
gotoPrevActivityWorkspace :: X ()
gotoPrevActivityWorkspace = gotoDirectionActivityWorkspace Prev

-- | Shift focused window to next activity workspace
shiftToNextActivityWorkspace :: X ()
shiftToNextActivityWorkspace = shiftToDirectionActivityWorkspace Next

-- | Shift focused window to previous activity workspace
shiftToPrevActivityWorkspace :: X ()
shiftToPrevActivityWorkspace = shiftToDirectionActivityWorkspace Prev

-- | Generic function
runActivityWorkspace :: WorkspaceIndex -> (WorkspaceId -> WindowSet -> WindowSet) -> X ()
runActivityWorkspace index func = do
  act <- getCurrentActivity
  ws <- getActivityWorkspaces . name $ act
  if length ws < (index+1) then return ()
  else windows (func (ws !! index))

-- | Goto Activity Workspace
gotoActivityWorkspace :: WorkspaceIndex -> X ()
gotoActivityWorkspace index = runActivityWorkspace index S.greedyView

-- | Shift Window to another activity workspace
shifttoActivityWorkspace :: WorkspaceIndex -> X ()
shifttoActivityWorkspace index = runActivityWorkspace index S.shift

-- | Output activities for ppExtra
print_activities = do
  AS m <- XS.get
  fmap Just (colorize_activities m)

-- | Return Workspaces corresponding to Activity
getActivityWorkspaces :: ActivityId -> X [WorkspaceId]
getActivityWorkspaces id = do
  wssort <- getSortByOrder
  ws <- gets (map S.tag . wssort . S.workspaces . windowset)
  if id == debug_activity then
    -- TODO: hack something better should be used
    return $ filter (\x -> 1==1) ws
    else
    if id == no_activity then
      return $ filter (\x -> case (elemIndex activity_workspace_separator x) of {Just y -> False; Nothing -> True}) ws
    else
      return $ filter (\x -> (take (length id) x) == id) ws

-- | Return function that returns only workspaces corresponding to Activity
filterWorkspaces :: X ([WindowSpace] -> [WindowSpace])
filterWorkspaces = do
	act <- getCurrentActivity
	ws <- getActivityWorkspaces . name $ act
	return $ filter (\y -> elem (S.tag y) ws)

-- | Add an activity
addActivity :: ActivityId -> X ()
addActivity n = do
  storeCurrentActivityContext
  AS m <- XS.get
  addWorkspace (n ++ [activity_workspace_separator] ++ "1")

  vis <- getVisible
  when (isJust vis) $ do
    let vis_wsname = n ++ [activity_workspace_separator] ++ "2"
    addHiddenWorkspace vis_wsname
    windows $ viewOnScreen (S.screen . fromJust $ vis) vis_wsname

  -- Yeah current and visible should not be nothing, but it will be updated when activity will change
  -- Anyway a weird behavior will happen if you delete a workspace and then create a new one without having changed activity
  -- Visible screen won't be updated because I was to lazy to change those Nothing...
  XS.put (AS $ m ++ [Activity {name=n,current=Nothing,visible=Nothing}])
  XS.put (CS $ (length m))

-- | Del current activity
delCurrentActivity :: X ()
delCurrentActivity = do
  AS activities <- XS.get
  CS current <- XS.get
  let act = activityById current activities

  -- If activity to be deleted is not Debug or NoActivity then we delete also ws
  when ((name act) /= debug_activity && (name act) /= no_activity) $ do
    ws <- getActivityWorkspaces . name $ act
    mapM_ (\wsid -> delActivityWorkspace wsid) ws

  XS.put (AS $ filter ((/= (name act)) . name) $ activities)
  when (current == ((length activities)-1)) $ XS.put (CS $ (current-1))

  -- A new activity id displayed so we have to restore its context
  restoreCurrentActivityContext

modifyStoredActivity :: ActivityIndex -> Activity -> X()
modifyStoredActivity index activity = do
  AS activities <- XS.get
  if index == 0 then
    XS.put (AS $ [activity] ++ (tail activities))
  else
    if index == (length activities)-1 then
	XS.put (AS $ (init activities) ++ [activity])
    else
	XS.put (AS $ ((take index activities) ++ [activity] ++ (drop (index+1) activities)))

-- Add a workspace to the current activity
addActivityWorkspace :: WorkspaceId -> X ()
addActivityWorkspace wsname = do
  act <- getCurrentActivity
  ws <- getActivityWorkspaces . name $ act

  vis <- getVisible
  ctag <- gets (S.currentTag . windowset)

  addWorkspace ((name act) ++ [activity_workspace_separator] ++ wsname)

  -- Set a visible workspace if there was no
  when ((length ws > 0) && (isNothing . visible $ act) && (not . isNothing $ vis)) $ do
    windows $ viewOnScreen (S.screen . fromJust $ vis) ctag


delActivityWorkspace :: WorkspaceId -> X ()
delActivityWorkspace wsid = do
  -- removeWorkspace delete current workspace, so we have to switch wsid to current
  windows (S.greedyView wsid)

  when (wsid /= trash_workspace) $ do
    exist <- isTrashWorkspace
    wins <- gets (S.integrate' . S.stack . S.workspace . S.current . windowset)

    when (not exist && (length wins) > 0) $ do
      addHiddenWorkspace trash_workspace

    withAll' $ S.shiftWin trash_workspace

  removeWorkspace

  -- Set a new current workspace for this activity if possible
  act <- getCurrentActivity
  ws <- getActivityWorkspaces . name $ act
  let eligible_ws = filter (not . belongToVisible act) ws
  if length eligible_ws > 0 then
    windows (S.greedyView . head $ eligible_ws)
  else
    return ()
  where
      belongToVisible act wsid = case (visible act) of
        Just x -> (workspace x) == wsid
        Nothing -> False


-- | Del current activity Workspace
delCurrentActivityWorkspace :: X ()
delCurrentActivityWorkspace = do
  ctag <- gets (S.currentTag . windowset)
  delActivityWorkspace ctag


-- | PROMPT MANAGEMENT

data ActivityPrompt = ActivityPrompt String

instance XPrompt ActivityPrompt where
  showXPrompt (ActivityPrompt s) = s

-- | Prompt for a name for the current activity.
promptActivityAdd :: XPConfig -> String -> X ()
promptActivityAdd xp s =
   mkXPrompt (ActivityPrompt s) xp (const $ return []) addActivity

-- | Prompt for a name for the current activity.
promptAddActivityWorkspace :: XPConfig -> String -> X ()
promptAddActivityWorkspace xp s =
   mkXPrompt (ActivityPrompt s) xp (const $ return []) addActivityWorkspace

-- | Taken from DynamicWorkspace 0.11. Should be removed when I will be using xmonad 0.11
removeWorkspace' :: (Eq i) => i -> S.StackSet i l a sid sd -> S.StackSet i l a sid sd
removeWorkspace' torem s@(S.StackSet { S.current = scr@(S.Screen { S.workspace = wc })
                                   , S.hidden = (w:ws) })
    | S.tag w == torem = s { S.current = scr { S.workspace = wc { S.stack = meld (S.stack w) (S.stack wc) } }
                         , S.hidden = ws }
   where meld Nothing Nothing = Nothing
         meld x Nothing = x
         meld Nothing x = x
         meld (Just x) (Just y) = S.differentiate (S.integrate x ++ S.integrate y)
removeWorkspace' _ s = s

-- | Should be changed when using xmonad 0.11
promptRenameCurrentActivityWorkspace :: XPConfig -> String -> X ()
promptRenameCurrentActivityWorkspace xp s = do
  act <- getCurrentActivity
  mkXPrompt (ActivityPrompt s) xp (const $ return []) $ \w -> windows $ \wset -> let sett wk = wk { S.tag = ((name act) ++ [activity_workspace_separator] ++ w) }
                                                                                     setscr scr = scr { S.workspace = sett $ S.workspace scr }
                                                                                     sets q = q { S.current = setscr $ S.current q }
                                                                                 in sets $ removeWorkspace' w wset
