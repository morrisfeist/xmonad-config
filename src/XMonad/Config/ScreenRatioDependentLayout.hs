module XMonad.Config.ScreenRatioDependentLayout
  ( SRDL (..)
  , mkSRDL
  ) where

import           Control.Arrow   (second)
import           Data.Maybe      (fromMaybe)
import           XMonad          (LayoutClass (description, handleMessage, runLayout),
                                  Rectangle (Rectangle), SomeMessage,
                                  WorkspaceId, X)
import           XMonad.StackSet (Workspace (Workspace))

-- Chooses which layout to use based on screen ratio.
data SRDL lt gt a = SRDL Rational SRDLActive (lt a) (gt a) deriving (Read, Show)
data SRDLActive = ActiveLT | ActiveGT deriving (Read, Show)

mkSRDL :: Rational -> lt a -> gt a -> SRDL lt gt a
mkSRDL r = SRDL r ActiveLT

mkLT :: SRDL lt gt a -> Maybe (lt a) -> Maybe (SRDL lt gt a)
mkLT (SRDL r ActiveLT _ gt) lt   = (\l -> SRDL r ActiveLT l gt) <$> lt
mkLT (SRDL r ActiveGT lt gt) lt' = Just $ SRDL r ActiveLT (fromMaybe lt lt') gt

mkGT :: SRDL lt gt a -> Maybe (gt a) -> Maybe (SRDL lt gt a)
mkGT (SRDL r ActiveLT lt gt) gt' = Just $ SRDL r ActiveGT lt (fromMaybe gt gt')
mkGT (SRDL r ActiveGT lt _) gt   = SRDL r ActiveGT lt <$> gt

instance (LayoutClass lt a, LayoutClass gt a) => LayoutClass (SRDL lt gt) a where
  runLayout :: Workspace WorkspaceId (SRDL lt gt a) a -> Rectangle -> X ([(a, Rectangle)], Maybe (SRDL lt gt a))
  runLayout (Workspace i srdl@(SRDL r _ lt gt) ms) rect@(Rectangle _ _ w h) =
    if fromIntegral w / fromIntegral h <= r
      then second (mkLT srdl) <$> runLayout (Workspace i lt ms) rect
      else second (mkGT srdl) <$> runLayout (Workspace i gt ms) rect

  handleMessage :: SRDL lt gt a -> SomeMessage -> X (Maybe (SRDL lt gt a))
  handleMessage (SRDL r ActiveLT lt gt) m = fmap (\l -> SRDL r ActiveLT l gt) <$> handleMessage lt m
  handleMessage (SRDL r ActiveGT lt gt) m = fmap (SRDL r ActiveGT lt) <$> handleMessage gt m

  description :: SRDL lt gt a -> String
  description (SRDL _ ActiveLT lt _) = description lt
  description (SRDL _ ActiveGT _ gt) = description gt
