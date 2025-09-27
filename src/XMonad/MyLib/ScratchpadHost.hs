{-# LANGUAGE LambdaCase   #-}
{-# LANGUAGE ViewPatterns #-}

module XMonad.MyLib.ScratchpadHost
  ( AppCommand
  , AppMatch (..)
  , ScratchpadApp (..)
  , ScratchpadHost (..)
  , ScratchpadMode (..)
  , ScratchpadMessage (..)
  , mkNamedScratchpadManageHook
  , scratchpadEventHook
  , doSetScratchpadMode
  ) where

import           Control.Monad               (filterM, unless, when)
import           Data.Bool                   (bool)
import           Data.Data                   (Typeable)
import qualified Data.Map                    as Map
import           Data.Maybe                  (fromMaybe, listToMaybe, mapMaybe)
import           Data.Monoid                 (All (All))
import           XMonad                      (Event (DestroyWindowEvent, ev_window),
                                              ExtensionClass (extensionType, initialValue),
                                              LayoutClass (handleMessage, runLayout),
                                              ManageHook, Message,
                                              MonadReader (ask),
                                              MonadState (get), Query,
                                              Rectangle, SomeMessage,
                                              StateExtension (PersistentExtension),
                                              Window, WorkspaceId, X,
                                              XState (windowset), appName,
                                              className, fromMessage, idHook,
                                              liftX, runQuery, sendMessage,
                                              spawn, title, (=?))
import           XMonad.StackSet             (Screen (workspace),
                                              Stack (Stack, focus),
                                              StackSet (current),
                                              Workspace (Workspace, stack),
                                              differentiate, integrate')
import qualified XMonad.Util.ExtensibleState as XS
import           XMonad.Util.NamedScratchpad (NamedScratchpad (NS, cmd, hook, name, query),
                                              namedScratchpadAction,
                                              namedScratchpadManageHook)


type AppName      = String
type AppTitle     = String
type AppClassName = String
type AppCommand   = String

data AppMatch
  = ClassMatch AppClassName -- AppClassName ~ second string returned by WM_CLASS
  | NameMatch AppName -- AppName ~ first string returned by WM_CLASS
  | TitleMatch AppTitle

data ScratchpadApp = ScratchpadApp
  { appMatch          :: AppMatch
  , appScratchpadMode :: ScratchpadMode
  , appCommand        :: AppCommand
  }

class IsInstance a where
  isInstance :: a -> Query Bool

instance IsInstance AppMatch where
  isInstance (ClassMatch c) = className =? c
  isInstance (NameMatch n)  = appName =? n
  isInstance (TitleMatch t) = title =? t

instance IsInstance ScratchpadApp where
  isInstance = isInstance . appMatch

getScratchpadAppName :: ScratchpadApp -> AppClassName
getScratchpadAppName app = case appMatch app of
  ClassMatch c -> c
  NameMatch n  -> n
  TitleMatch t -> t

toNamedScratchpad :: ScratchpadApp -> NamedScratchpad
toNamedScratchpad app = NS
  { name = getScratchpadAppName app
  , cmd = appCommand app
  , query = isInstance app
  , hook = doSetScratchpadMode $ appScratchpadMode app
  }


newtype ScratchpadState = ScratchpadState
  { unScratchpadState :: Map.Map Window ScratchpadMode
  } deriving (Read, Show, Typeable)

instance ExtensionClass ScratchpadState where
  initialValue = ScratchpadState Map.empty
  extensionType = PersistentExtension

setScratchpadMode :: ScratchpadMode -> Window -> X ()
setScratchpadMode mode a = do
  XS.modify $ ScratchpadState . Map.insert a mode . unScratchpadState
  spawn $ "xprop -id " ++ show a ++ " -f _SCRATCHPAD_STATUS 8s -set _SCRATCHPAD_STATUS " ++ show mode
  when (mode == Above) $ do
    sendMessage ShowScratchpad

cycleScratchpadMode :: Window -> X ()
cycleScratchpadMode a = do
  ScratchpadState as <- XS.get
  let mode = cycleMode (Map.lookup a as)
  XS.put $ ScratchpadState (Map.insert a mode as)
  spawn $ "xprop -id " ++ show a ++ " -f _SCRATCHPAD_STATUS 8s -set _SCRATCHPAD_STATUS " ++ show mode
  when (mode == Above) $ do
    sendMessage ShowScratchpad
  where
    cycleMode :: Maybe ScratchpadMode -> ScratchpadMode
    cycleMode (Just x)
      | x == maxBound = minBound
      | otherwise     = succ x
    cycleMode _            = Above

unsetScratchpadMode :: Window -> X ()
unsetScratchpadMode a = do
  XS.modify $ ScratchpadState . Map.delete a . unScratchpadState

doSetScratchpadMode :: ScratchpadMode -> ManageHook
doSetScratchpadMode mode = liftX . (>> idHook) . setScratchpadMode mode =<< ask

scratchpadEventHook :: Event -> X All
scratchpadEventHook ev@(DestroyWindowEvent {}) = All True <$ unsetScratchpadMode (ev_window ev)
scratchpadEventHook _ = pure (All True)

queryScratchpadModeByApp :: ScratchpadApp -> X ScratchpadMode
queryScratchpadModeByApp app = do
  ScratchpadState as <- XS.get
  fromMaybe Normal . listToMaybe . mapMaybe (`Map.lookup` as) <$> filterM (runQuery $ isInstance app) (Map.keys as)

class QueryScratchpadMode a where
  queryScratchpadMode :: a -> X ScratchpadMode

instance QueryScratchpadMode Window where
  queryScratchpadMode a = Map.findWithDefault Normal a . unScratchpadState <$> XS.get

data ScratchpadMode = Normal | Above deriving (Eq, Read, Show, Enum, Bounded)

data ScratchpadHost la lm a = ScratchpadHost Bool (la a) (lm a)
  deriving (Read, Show)

data ScratchpadMessage
  = SetScratchpadMode ScratchpadMode Window
  | CycleScratchpadMode Window
  | ToggleScratchpad
  | ShowScratchpad
  | HideScratchpad
  | RunScratchpadApp [ScratchpadApp] ScratchpadApp
instance Message ScratchpadMessage

instance (Eq a, LayoutClass la a, LayoutClass lm a, QueryScratchpadMode a, Read (la a)) => LayoutClass (ScratchpadHost la lm) a where

  runLayout :: Workspace WorkspaceId (ScratchpadHost la lm a) a -> Rectangle -> X ([(a, Rectangle)], Maybe (ScratchpadHost la lm a))
  runLayout (Workspace i (ScratchpadHost v la lm) stackAll) rect = do
    -- Determine which window belongs in the scratchpad
    (sa, sm) <- partitionStack queryScratchpadMode stackAll

    -- runLayout for scratchpad and main windows
    (aa, la') <- runLayout (Workspace i la sa) rect
    (am, lm') <- runLayout (Workspace i lm sm) rect

    pure
      ( bool [] aa v <> am
      , case (la', lm') of
          (Nothing, Nothing) ->  Nothing
          _ -> Just $ ScratchpadHost v
            (fromMaybe la la')
            (fromMaybe lm lm')
      )

  handleMessage :: ScratchpadHost la lm a -> SomeMessage -> X (Maybe (ScratchpadHost la lm a))
  handleMessage l (fromMessage -> Just (SetScratchpadMode mode w)) = Just l <$ setScratchpadMode mode w
  handleMessage l (fromMessage -> Just (CycleScratchpadMode w)) = Just l <$ cycleScratchpadMode w
  handleMessage (ScratchpadHost v la lm) (fromMessage -> Just ToggleScratchpad) = pure $ Just $ ScratchpadHost (not v) la lm
  handleMessage (ScratchpadHost _ la lm) (fromMessage -> Just ShowScratchpad) = pure $ Just $ ScratchpadHost True la lm
  handleMessage (ScratchpadHost _ la lm) (fromMessage -> Just HideScratchpad) = pure $ Just $ ScratchpadHost False la lm
  handleMessage (ScratchpadHost False la lm) (fromMessage -> Just (RunScratchpadApp as a)) = do
    let namedScratchpads = toNamedScratchpad <$> as
    queryScratchpadModeByApp a >>= \case
      Above -> do
        onCurrentWorkspace <- isOnCurrentWorkspace a
        unless onCurrentWorkspace $ do
          namedScratchpadAction namedScratchpads $ getScratchpadAppName a
        pure $ Just $ ScratchpadHost True la lm
      _ -> do
        namedScratchpadAction namedScratchpads $ getScratchpadAppName a
        pure Nothing
  handleMessage (ScratchpadHost True _ _) (fromMessage -> Just (RunScratchpadApp as a)) = do
    let namedScratchpads = toNamedScratchpad <$> as
    namedScratchpadAction namedScratchpads $ getScratchpadAppName a
    pure Nothing
  handleMessage (ScratchpadHost v la lm) m = (fmap . fmap) (ScratchpadHost v la) (handleMessage lm m)


isOnCurrentWorkspace :: ScratchpadApp -> X Bool
isOnCurrentWorkspace app = do
  ws <- integrate' . stack . workspace . current . windowset <$> get
  not . null <$> filterM (runQuery $ isInstance app) ws

partitionStack :: (Eq a, Monad m) => (a -> m ScratchpadMode) -> Maybe (Stack a) -> m (Maybe (Stack a), Maybe (Stack a))
partitionStack p stackAll = do
  let windowsAll = integrate' stackAll
  labeledWindows <- zip windowsAll <$> mapM p windowsAll
  let restack tag = createStack (focus <$> stackAll) . map fst . filter ((== tag) . snd)
  pure
    ( restack Above labeledWindows
    , restack Normal labeledWindows
    )

createStack :: Eq a => Maybe a -> [a] -> Maybe (Stack a)
createStack Nothing as = differentiate as
createStack (Just a) as = case span (/= a) as of
  (_, [])    -> differentiate as
  (ls, r:rs) -> Just $ Stack r (reverse ls) rs

mkNamedScratchpadManageHook :: [ScratchpadApp] -> ManageHook
mkNamedScratchpadManageHook = namedScratchpadManageHook . fmap toNamedScratchpad
