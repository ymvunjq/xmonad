{-# OPTIONS_GHC -XNoMonomorphismRestriction #-}
{-# LANGUAGE DeriveDataTypeable #-}

------------------------------------------------------------------------------------------------------
-- |
-- Module : CleanRemove.hs
-- Author : ymvunjq@gmail.com
--
-- Allow deleting workspace moving all windows of that workspace to a trash workspace before
-----------------------------------------------------------------------------------------------------


module CleanRemove
(
	  cleanRemoveWS
) where

import Control.Monad
import Control.Applicative

import XMonad
import qualified XMonad.StackSet as S

-- To add trash workspace if it does not exist
import XMonad.Actions.DynamicWorkspaces (addHiddenWorkspace,removeWorkspace)

import XMonad.Actions.WithAll (withAll')

trash_workspace = "trash"

-- | Return True if trash workspace exists
isTrashWorkspace :: X Bool
isTrashWorkspace = do
  ws <- gets (map S.tag . S.workspaces . windowset)
  return $ elem trash_workspace ws


-- | Cleanly remove current workspace, moving all its windows to a trash workspace
cleanRemoveWS :: X ()
cleanRemoveWS = do
  ws <- gets (S.currentTag . windowset)
  when (ws /= trash_workspace) $ do
    exist <- isTrashWorkspace
    wins <- gets (S.integrate' . S.stack . S.workspace . S.current . windowset)

    when (not exist && (length wins) > 0) $ do
      addHiddenWorkspace trash_workspace

    withAll' $ S.shiftWin trash_workspace

  removeWorkspace
