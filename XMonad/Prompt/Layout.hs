{-# LANGUAGE ExistentialQuantification, FlexibleInstances, MultiParamTypeClasses, RankNTypes, AllowAmbiguousTypes, KindSignatures, FlexibleContexts #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Prompt.Layout
-- Copyright   :  (C) 2017 Ivan Malison
-- License     :  BSD3
--
-- Maintainer  :
-- Stability   :  unstable
-- Portability :  unportable
--
-- A layout-selection prompt for XMonad
--
-----------------------------------------------------------------------------

module XMonad.Prompt.Layout
  ( joinLayouts
  , selectLayoutDmenu
  , layoutPrompt
  , layoutNames
  , LayoutInfo(..)
  , (|||!)
  ) where

import           XMonad.Core (LayoutClass(..), X, Layout(..))
import           XMonad.Layout.LayoutCombinators
import           XMonad.Operations
import           XMonad.Prompt
import           XMonad.Prompt.Workspace
import qualified XMonad.Util.Dmenu as DM

data LayoutInfo a = forall l. (LayoutClass l a, Read (l a)) =>
  LayoutInfo { hook :: l a
             , layouts :: [Layout a]
             }

layoutNames :: LayoutClass Layout a => LayoutInfo a -> [String]
layoutNames info = [description layout | layout <- layouts info]

layoutPrompt :: LayoutClass Layout a => XPConfig -> LayoutInfo a -> X ()
layoutPrompt c info =
  mkXPrompt
    (Wor "")
    c
    (mkComplFunFromList' (layoutNames info))
    (sendMessage . JumpToLayout)

selectLayoutDmenu :: LayoutClass Layout a => LayoutInfo a -> X String
selectLayoutDmenu info =
  DM.menuArgs "rofi" ["-dmenu", "-i"] $ layoutNames info

data LayoutCombinator a =
  LayoutCombinator (forall l1 l2. ( LayoutClass l1 a
                                  , Read (l1 a)
                                  , LayoutClass l2 a
                                  , Read (l2 a)
                                  ) => l1 a -> l2 a -> Layout a)

class InfoJoinable l1 l2 a where
  joinLayouts :: LayoutCombinator a -> l1 a -> l2 a -> LayoutInfo a

instance InfoJoinable LayoutInfo LayoutInfo a where
  joinLayouts (LayoutCombinator op)
              LayoutInfo {hook = h1, layouts = l1}
              LayoutInfo {hook = h2, layouts = l2} =
                case h1 `op` h2 of
                  Layout l -> LayoutInfo {hook = l, layouts = l1 ++ l2}

instance (LayoutClass l a, Read (l a)) =>
         InfoJoinable LayoutInfo l a where
  joinLayouts (LayoutCombinator op) LayoutInfo { hook = theHook
                                               , layouts = theLayouts
                                               } newLayout =
    case theHook `op` newLayout of
      Layout l -> LayoutInfo { hook = l
                             , layouts = theLayouts ++ [Layout newLayout]
                             }

instance (LayoutClass l a, Read (l a)) =>
         InfoJoinable l LayoutInfo a where
  joinLayouts (LayoutCombinator op) newLayout
              LayoutInfo { hook = theHook , layouts = theLayouts} =
    case theHook `op` newLayout of
      Layout l -> LayoutInfo { hook = l
                             , layouts = theLayouts ++ [Layout newLayout]
                             }

instance (LayoutClass l a, Read (l a), LayoutClass l2 a, Read (l2 a)) =>
         InfoJoinable l l2 a where
  joinLayouts (LayoutCombinator op) l1 l2 =
    case l1 `op` l2 of
      Layout l -> LayoutInfo {hook = l, layouts = [Layout l1, Layout l2]}

(|||!)
  :: forall (l1 :: * -> *) (l2 :: * -> *) a.
     InfoJoinable l1 l2 a
  => l1 a -> l2 a -> LayoutInfo a
l1 |||! l2 = joinLayouts (LayoutCombinator (\a b -> Layout (a ||| b))) l1 l2
