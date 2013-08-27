{-# OPTIONS_GHC -XNoMonomorphismRestriction #-}

module Host
(
	myHostKeys
) where

import XMonad hiding ( (|||) )

myHostKeys = [
			-- XF86ScreenSaver
			((0, 0xff14), spawn "gnome-screensaver-command -l")
			]
