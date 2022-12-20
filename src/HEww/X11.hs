{-# LANGUAGE ExistentialQuantification #-}
module HEww.X11 (getX11DataSources, windowTitle) where
    
import Graphics.X11.Xlib.Event
import Graphics.X11.Xlib
import Graphics.X11.Xlib.Extras

import Pipes

import Control.Concurrent.STM

import HEww.Core

import Data.Text (Text)
import Data.String (fromString)
import Data.Map (Map)
import qualified Data.Map as Map

import Control.Monad.Reader
import Control.Concurrent (forkIO)
import Control.Monad.State
import Data.Bits

import System.IO
import Data.Maybe (fromMaybe)

data XEnv = XEnv {display :: Display, root :: Window}

type NeededWindowEvents = Map Window (EventMask)
data X11DataSourceState s = X11DataSourceState {dataSourceState :: s, eventsState :: NeededWindowEvents}

type X11DataSourceM s = StateT (X11DataSourceState s) (ReaderT XEnv IO)

data X11DataSource a = forall s. X11DataSource (Event -> X11DataSourceM s (Maybe (UpdateM a ()))) (X11DataSourceM s (Maybe UpdateM a ())) s EventMask


onEventState :: (NeededWindowEvents -> NeededWindowEvents) -> X11DataSourceM s ()
onEventState f = modify $ \s -> s {eventsState = f $ eventsState s}

removeDependency :: Window -> X11DataSourceM s ()
removeDependency w = onEventState $ Map.delete w

addEventDependency :: Window -> EventMask -> X11DataSourceM s ()
addEventDependency w ev = onEventState $ Map.alter (\o -> Just (fromMaybe noEventMask o .|. ev)) w

getDataState :: X11DataSourceM s s
getDataState = gets dataSourceState

putDataState :: s -> X11DataSourceM s ()
putDataState p = modify $ \s -> s {dataSourceState = p}

getAtomNamed :: MonadIO m => String -> Display -> m Atom
getAtomNamed n dis = liftIO $ internAtom dis n False

getActiveWindowAtom :: MonadIO m => Display -> m Atom
getActiveWindowAtom = getAtomNamed "_NET_ACTIVE_WINDOW"

getNetWmNameAtom :: MonadIO m => Display -> m Atom
getNetWmNameAtom = getAtomNamed "_NET_WM_NAME"

getWMNameAtom :: MonadIO m => Display -> m Atom
getWMNameAtom = getAtomNamed "WM_NAME"

getCurrentWindow :: MonadIO m => Display -> Window -> Atom -> m (Maybe Window)
getCurrentWindow dis rt act = fmap head <$> (liftIO $ rawGetWindowProperty 32 dis act rt)

changedTitle :: XID -> Display -> X11DataSourceM (Maybe XID) (Maybe (Maybe String))
changedTitle xid dis = do n <- liftIO $ fetchName dis xid
                          case n of
                              Nothing -> putDataState (Just xid) >> return Nothing
                              Just name -> putDataState (Just xid) >> return (Just $ Just $ name)

activeWindowChanged :: X11DataSourceM (Maybe XID) (Maybe (Maybe String))
activeWindowChanged = do
  dis <- asks display
  rt <- asks root
  act <- getActiveWindowAtom dis
  activeWin <- getCurrentWindow dis rt act
  lx <- getDataState
  case (lx,activeWin) of
      (Just a,Just ac) -> when (a /= ac) $ liftIO $ selectInput dis a noEventMask
      _ -> pure ()
  case activeWin of
      Just 0 -> putDataState Nothing >> pure Nothing
      Just n -> do liftIO $ selectInput dis n propertyChangeMask
                   changedTitle n dis
      Nothing -> do liftIO $ hPutStrLn stderr "could not get active window"
                    return Nothing

handlePropertyEvent :: Window -> Atom -> X11DataSourceM (Maybe XID) (Maybe (Maybe String))
handlePropertyEvent win atom =
  do dis <- asks display
     lx <- getDataState
     n1 <- getNetWmNameAtom dis
     n2 <- getWMNameAtom dis
     act <- getActiveWindowAtom dis
     if atom == act then activeWindowChanged
     else if atom == n1 || atom == n2 then
       case lx of
         Nothing -> return Nothing
         Just lw -> if lw == win then
                       changedTitle lw dis
                    else return Nothing
     else
       return Nothing

windowTitle :: (Maybe Text -> UpdateM a ()) -> X11DataSource a
windowTitle f =
  X11DataSource (\ev ->
    do dis <- asks display
       r <- case ev of
              PropertyEvent _ _ _ _ win atom _ _ -> handlePropertyEvent win atom
              _ -> return Nothing
       liftIO $ flush dis
       pure $ rawDataToUpdate f r
    )
    (pure ())
    Nothing
    propertyChangeMask
 where rawDataToUpdate :: (Maybe Text -> UpdateM a ()) -> Maybe (Maybe String) -> Maybe (UpdateM a ())
       rawDataToUpdate update = fmap (update . fmap fromString)


data WorkspaceStatus = Open | Occupied | Empty
data WorkspaceInfo = WorkspaceInfo {name :: Text, status :: WorkspaceStatus}

-- xworkspacesPropertyChange :: Window -> Atom -> X11DataSourceM () [WorkspaceInfo]

-- workspaces :: ([WorkspaceInfo] -> UpdateM a ()) -> X11DataSource a
-- workspaces f =
--   X11DataSource (\ev ->
--       (f <$> case ev of
--                PropertyEvent _ _ _ _ win atom _ _ -> Just <$> xworkspacesPropertyChange win atom
--                _ -> return Nothing)
--        <*
--        liftIO $ flush dis
--     )
--     ()
--     propertyChangeMask

-- data DataSourceCurrentState = DataSourceCurrentState {}

eventLoop :: Display -> Window -> [X11DataSource a] -> TQueue (UpdateM a ()) -> IO ()
eventLoop dis rt dsources q =
       allocaXEvent $ \ev -> do
            nextEvent dis ev
            ev' <- getEvent ev
            dn <- forM dsources $ \(X11DataSource d s em) ->
             do (up,s') <- runReaderT (runStateT (d ev') s) (XEnv dis rt)
                case up of
                    Just up'-> pushInputToTQueue q up'
                    Nothing -> return ()
                return $ X11DataSource d s' em
            eventLoop dis rt dn q

runEventLoop :: Display -> Window -> [X11DataSource a] -> TQueue (UpdateM a ()) -> IO ()

getX11DataSources :: [X11DataSource a] -> DataSourceExtension a
getX11DataSources x11data = addDataSource $
    do tq <- newTQueueIO
       dis <- openDisplay ":0"
       let rt = defaultRootWindow dis
           mask = foldl (\o (X11DataSource _ _ m) -> m .|. o) noEventMask x11data
       selectInput dis rt (mask)
       flush dis
       void $ forkIO $ eventLoop dis rt x11data tq
       return $ DataSource (tQueueToProducer tq) $ pure ()

