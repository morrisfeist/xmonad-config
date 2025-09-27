module XMonad.Config.ShiftFirstHalf
  ( ShiftFirstHalf (..)
  ) where
import           Control.Arrow   (second)
import           Data.Tuple      (swap)
import           XMonad          (LayoutClass (description, handleMessage, runLayout))
import           XMonad.StackSet (Stack (Stack, focus), Workspace (Workspace),
                                  differentiate, integrate')

newtype ShiftFirstHalf l a = ShiftFirstHalf { unShiftFirstHalf :: l a } deriving (Read, Show)

instance (Eq a, LayoutClass l a) => LayoutClass (ShiftFirstHalf l) a where
  runLayout (Workspace i (ShiftFirstHalf l) ms) rect =
    let ms' = createStack (focus <$> ms) (shiftFirstHalf $ integrate' ms)
    in second (fmap ShiftFirstHalf) <$> runLayout (Workspace i l ms') rect
    where
      shiftFirstHalf [] = []
      shiftFirstHalf xs = uncurry (++) . swap $ splitAt (length xs `div` 2 + 1) xs

  handleMessage l m = fmap ShiftFirstHalf <$> handleMessage (unShiftFirstHalf l) m
  description = description . unShiftFirstHalf

createStack :: Eq a => Maybe a -> [a] -> Maybe (Stack a)
createStack Nothing as = differentiate as
createStack (Just a) as = case span (/= a) as of
  (_, [])    -> differentiate as
  (ls, r:rs) -> Just $ Stack r (reverse ls) rs
