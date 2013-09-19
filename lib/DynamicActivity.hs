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
          ActivityConfig(..), defaultActivityConfig
	, print_activities
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
import XMonad.Hooks.UrgencyHook (readUrgents)

-- Should use renameWorkspaceByName from DynamicWorkspace, but not available in xmonad 0.10
import XMonad.Prompt.Workspace ( workspacePrompt )

-- To cleanly remove workspaces
import CleanRemove (cleanRemoveWS)

type ActivityId = String
type ActivityIndex = Int
type WorkspaceIndex = Int
type WorkspacePosition = Maybe (ScreenId,WorkspaceId)
type ScreenInfo = S.Screen WorkspaceId (Layout Window) Window ScreenId ScreenDetail

data ActivityConfig = ActivityConfig { no_activity_name             :: String,
                                       debug_activity_name          :: String,
                                       activity_separator           :: String,
                                       ws_separator                 :: Char,
                                       ppActivityActive             :: ActivityId -> String,
                                       ppActivityInactive           :: ActivityId -> String,
                                       ppActivityUrgent             :: ActivityId -> String
                                     }

defaultActivityConfig :: ActivityConfig
defaultActivityConfig = ActivityConfig { no_activity_name             = "*",
                                         debug_activity_name          = "Debug",
                                         activity_separator           = " ",
                                         ws_separator                 = '-',
                                         ppActivityActive             = id,
                                         ppActivityInactive           = id,
                                         ppActivityUrgent             = id
                                       }


data Activity = Activity { name :: ActivityId
                         , current :: WorkspacePosition
                           -- OK... should be [WorkspacePosition] if we have more than 2 screens
                         , visible :: WorkspacePosition
                         } deriving (Typeable, Show, Read)

data ActivityStorage = AS [Activity] deriving (Typeable, Read, Show)
data CurrentActivity = CS Int deriving (Typeable, Read, Show)

instance ExtensionClass ActivityStorage where
  -- TODO: Bug if no_activity name is not *
  initialValue = AS $ [Activity{name="*",current=Nothing,visible=Nothing}]
  extensionType = PersistentExtension

instance ExtensionClass CurrentActivity where
  initialValue = CS $ 0
  extensionType = PersistentExtension

----------------------------------------------------------------------------------------
-- ACTIVITY NAME MANAGEMENT
----------------------------------------------------------------------------------------

-- | Get name of workspace depending on activity name
getActivityWorkspaceName :: ActivityConfig -> String -> String -> String
getActivityWorkspaceName conf activity ws = activity ++ [(ws_separator conf)] ++ ws

-- | Remove activity name from workspace name
--   Ex: Activity1-ws1 -> ws1
dropActivityName :: ActivityConfig -> String -> String
dropActivityName conf ws = do
  case (elemIndex  (ws_separator conf) ws) of
    Just x -> drop (x+1) ws
    Nothing -> ws

-- | Return true if a workspace belongs to an activity (no matter of the activity)
belongToAnActivity :: ActivityConfig -> WorkspaceId -> Bool
belongToAnActivity conf ws = case (elemIndex (ws_separator conf)  ws) of
  Just x -> True
  Nothing -> False

-- | Return True if ws belongs to activity act
belongToActivity :: ActivityConfig -> WorkspaceId -> ActivityId -> Bool
belongToActivity conf ws act
  | act == (debug_activity_name conf) = True
  | act == (no_activity_name conf) = not . belongToAnActivity conf $ ws
  | otherwise = take (length act) ws == act

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
isUrgentActivity :: ActivityConfig -> ActivityId -> X Bool
isUrgentActivity conf actid = do
  urgents <- readUrgents
  sst <- gets (windowset)
  let ws = map (fromJust) . filter (isJust) . map (\win -> S.findTag win sst) $ urgents
  return $ any (\x -> belongToActivity conf x actid) ws

color :: ActivityConfig -> Activity -> X String
color conf activity = do
  let n = (name activity)
  CS current <- XS.get
  AS activities <- XS.get
  urgent <- isUrgentActivity conf n
  if urgent then return (ppActivityUrgent conf $ n)
  else if n == (name (activityById current activities)) then return (ppActivityActive conf $ n)
       else return (ppActivityInactive conf $ n)

colorize_activities :: ActivityConfig -> [Activity] -> X String
colorize_activities conf as = fmap (sepBy (activity_separator conf)) (mapM (color conf) as)

activityById :: ActivityIndex -> [Activity] -> Activity
activityById index list = list !! index

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
storeCurrentActivityContext :: ActivityConfig -> X ()
storeCurrentActivityContext conf = do
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
      getTag ws act val = if belongToActivity conf ws act then Just val else Nothing

-- | Restore Current Activity Context
restoreCurrentActivityContext :: X ()
restoreCurrentActivityContext = do
  setActivityVisibleWS
  setActivityCurrentWS


-- | Direct Access to activity
gotoActivity :: ActivityConfig -> ActivityIndex -> X ()
gotoActivity conf index = do
  AS activities <- XS.get
  if (index + 1) > length activities then return ()
  else do
    storeCurrentActivityContext conf
    XS.put (CS $ index)
    restoreCurrentActivityContext

switchActivity :: ActivityConfig -> Int -> X ()
switchActivity conf shift = do
  AS activities <- XS.get
  CS current <- XS.get
  gotoActivity conf ((current+shift) `mod` (length activities))

-- | Goto next Activity
nextActivity :: ActivityConfig -> X ()
nextActivity conf = switchActivity conf 1

-- | Goto prev Activity
prevActivity :: ActivityConfig -> X ()
prevActivity conf = switchActivity conf (-1)

-- | Generic function
dotoDirectionActivityWorkspace :: ActivityConfig -> (WorkspaceId -> X ()) -> Direction1D -> X ()
dotoDirectionActivityWorkspace conf action direction = do
  doTo direction (WSIs wsExist) getSortByOrder action
  where
    wsExist = do
      act <- getCurrentActivity
      ws <- getActivityWorkspaces conf . name $ act
      return $ \x -> elem (S.tag x) ws


gotoDirectionActivityWorkspace :: ActivityConfig -> Direction1D -> X ()
gotoDirectionActivityWorkspace conf = dotoDirectionActivityWorkspace conf (windows . S.greedyView)

shiftToDirectionActivityWorkspace :: ActivityConfig -> Direction1D -> X ()
shiftToDirectionActivityWorkspace conf = dotoDirectionActivityWorkspace conf (windows . S.shift)

-- | Goto next Workspace belonging to activity
gotoNextActivityWorkspace :: ActivityConfig -> X ()
gotoNextActivityWorkspace conf = gotoDirectionActivityWorkspace conf Next

-- | Goto prev Workspace belonging to activity
gotoPrevActivityWorkspace :: ActivityConfig -> X ()
gotoPrevActivityWorkspace conf = gotoDirectionActivityWorkspace conf Prev

-- | Shift focused window to next activity workspace
shiftToNextActivityWorkspace :: ActivityConfig -> X ()
shiftToNextActivityWorkspace conf = shiftToDirectionActivityWorkspace conf Next

-- | Shift focused window to previous activity workspace
shiftToPrevActivityWorkspace :: ActivityConfig -> X ()
shiftToPrevActivityWorkspace conf = shiftToDirectionActivityWorkspace conf Prev

-- | Generic function
runActivityWorkspace :: ActivityConfig -> WorkspaceIndex -> (WorkspaceId -> WindowSet -> WindowSet) -> X ()
runActivityWorkspace conf index func = do
  act <- getCurrentActivity
  ws <- getActivityWorkspaces conf . name $ act
  if length ws < (index+1) then return ()
  else windows (func (ws !! index))

-- | Goto Activity Workspace
gotoActivityWorkspace :: ActivityConfig -> WorkspaceIndex -> X ()
gotoActivityWorkspace conf index = runActivityWorkspace conf index S.greedyView

-- | Shift Window to another activity workspace
shifttoActivityWorkspace :: ActivityConfig -> WorkspaceIndex -> X ()
shifttoActivityWorkspace conf index = runActivityWorkspace conf index S.shift

-- | Output activities for ppExtra
print_activities conf = do
  AS m <- XS.get
  fmap Just (colorize_activities conf m)

-- | Return Workspaces corresponding to Activity
getActivityWorkspaces :: ActivityConfig -> ActivityId -> X [WorkspaceId]
getActivityWorkspaces conf id = do
  wssort <- getSortByOrder
  ws <- gets (map S.tag . wssort . S.workspaces . windowset)
  if id == (debug_activity_name conf) then
    -- TODO: hack something better should be used
    return $ filter (\x -> 1==1) ws
    else
    if id == (no_activity_name conf) then
      return $ filter (\x -> case (elemIndex (ws_separator conf) x) of {Just y -> False; Nothing -> True}) ws
    else
      return $ filter (\x -> (take (length id) x) == id) ws

-- | Return function that returns only workspaces corresponding to Activity
filterWorkspaces :: ActivityConfig -> X ([WindowSpace] -> [WindowSpace])
filterWorkspaces conf = do
	act <- getCurrentActivity
	ws <- getActivityWorkspaces conf . name $ act
	return $ filter (\y -> elem (S.tag y) ws)

-- | Add an activity
addActivity :: ActivityConfig -> ActivityId -> X ()
addActivity conf n = do
  storeCurrentActivityContext conf
  AS m <- XS.get
  addWorkspace $ getActivityWorkspaceName conf n "1"

  vis <- getVisible
  when (isJust vis) $ do
    let vis_wsname = getActivityWorkspaceName conf n "2"
    addHiddenWorkspace vis_wsname
    windows $ viewOnScreen (S.screen . fromJust $ vis) vis_wsname

  -- Yeah current and visible should not be nothing, but it will be updated when activity will change
  -- Anyway a weird behavior will happen if you delete a workspace and then create a new one without having changed activity
  -- Visible screen won't be updated because I was to lazy to change those Nothing...
  XS.put (AS $ m ++ [Activity {name=n,current=Nothing,visible=Nothing}])
  XS.put (CS $ (length m))

-- | Del current activity
delCurrentActivity :: ActivityConfig -> X ()
delCurrentActivity conf = do
  AS activities <- XS.get
  CS current <- XS.get
  let act = activityById current activities

  -- If activity to be deleted is not Debug or NoActivity then we delete also ws
  when ((name act) /= (debug_activity_name conf) && (name act) /= (no_activity_name conf)) $ do
    ws <- getActivityWorkspaces conf $ name act
    mapM_ (\wsid -> delActivityWorkspace conf wsid) ws

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
addActivityWorkspace :: ActivityConfig -> WorkspaceId -> X ()
addActivityWorkspace conf wsname = do
  act <- getCurrentActivity
  ws <- getActivityWorkspaces conf $ name act

  vis <- getVisible
  ctag <- gets (S.currentTag . windowset)

  addWorkspace $ getActivityWorkspaceName conf (name act) wsname

  -- Set a visible workspace if there was no
  when ((length ws > 0) && (isNothing . visible $ act) && (not . isNothing $ vis)) $ do
    windows $ viewOnScreen (S.screen . fromJust $ vis) ctag


delActivityWorkspace :: ActivityConfig -> WorkspaceId -> X ()
delActivityWorkspace conf wsid = do
  -- cleanRemoveWS delete current workspace, so we have to switch wsid to current
  windows (S.greedyView wsid)
  cleanRemoveWS

  -- Set a new current workspace for this activity if possible
  act <- getCurrentActivity
  ws <- getActivityWorkspaces conf $ name act
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
delCurrentActivityWorkspace :: ActivityConfig -> X ()
delCurrentActivityWorkspace conf = do
  ctag <- gets (S.currentTag . windowset)
  delActivityWorkspace conf ctag


-- | PROMPT MANAGEMENT

data ActivityPrompt = ActivityPrompt String

instance XPrompt ActivityPrompt where
  showXPrompt (ActivityPrompt s) = s

-- | Prompt for a name for the current activity.
promptActivityAdd :: XPConfig -> ActivityConfig -> String -> X ()
promptActivityAdd xp conf s =
   mkXPrompt (ActivityPrompt s) xp (const $ return []) (addActivity conf)

-- | Prompt for a name for the current activity.
promptAddActivityWorkspace :: XPConfig -> ActivityConfig -> String -> X ()
promptAddActivityWorkspace xp conf s =
   mkXPrompt (ActivityPrompt s) xp (const $ return []) (addActivityWorkspace conf)

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
promptRenameCurrentActivityWorkspace :: XPConfig -> ActivityConfig -> String -> X ()
promptRenameCurrentActivityWorkspace xp conf s = do
  act <- getCurrentActivity
  mkXPrompt (ActivityPrompt s) xp (const $ return []) $ \w -> windows $ \wset -> let sett wk = wk { S.tag = (getActivityWorkspaceName conf (name act) w) }
                                                                                     setscr scr = scr { S.workspace = sett $ S.workspace scr }
                                                                                     sets q = q { S.current = setscr $ S.current q }
                                                                                 in sets $ removeWorkspace' w wset
