{-# LANGUAGE ExistentialQuantification,TupleSections,ScopedTypeVariables #-}
module HEww.X11 (getX11DataSources,
                 windowTitle,
                 workspaces,
                 X11Handle,
                 changeWorkspace,
                 WorkspaceInfo(..),
                 WorkspaceStatus(..),
                 X11DataSource(..),
                 Event(..),
                 X11DataSourceM(..),
                 addEventDependency,
                 removeDependency
                 ) where
    
import Graphics.X11.Xlib.Event
import Graphics.X11.Xlib
import Graphics.X11.Xlib.Extras

import Pipes

import Control.Concurrent.STM

import HEww.Core

import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8With)
import Data.String (fromString)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.List (foldl',sort)
import Data.Set (Set)
import Data.Foldable (fold)
import qualified Data.Set as Set
import Control.Monad.State
import Data.Bits

import Control.Monad.Reader
import Control.Concurrent (forkIO)

import Data.ByteString (split,pack)

import System.IO

-- import Data.Bits.Extras (w64)

data XEnv = XEnv {display :: Display, root :: Window}

type NeededWindowEvents = Map Window (EventMask)
data X11DataSourceState s = X11DataSourceState {dataSourceState :: s, eventsState :: NeededWindowEvents}

type X11DataSourceM s = StateT (X11DataSourceState s) (ReaderT XEnv IO)

data X11DataSource a = forall s. X11DataSource (Event -> X11DataSourceM s (Maybe (UpdateM a ()))) (X11DataSourceM s (Maybe (UpdateM a ()))) s


onEventState :: (NeededWindowEvents -> NeededWindowEvents) -> X11DataSourceM s ()
onEventState f = modify $ \s -> s {eventsState = f $ eventsState s}

removeDependency :: Window -> X11DataSourceM s ()
removeDependency w = onEventState $ Map.delete w

addEventDependency :: Window -> EventMask -> X11DataSourceM s ()
addEventDependency w ev = onEventState $ Map.alter (\o -> Just (fromMaybe noEventMask o .|. ev)) w

getDataState :: X11DataSourceM s s
getDataState = gets dataSourceState

getsData :: (s -> a) -> X11DataSourceM s a
getsData a = gets $ a . dataSourceState

putDataState :: s -> X11DataSourceM s ()
putDataState p = modify $ \s -> s {dataSourceState = p}

modifyData :: (s -> s) -> X11DataSourceM s ()
modifyData f = modify $ \s -> s {dataSourceState = f $ dataSourceState s}

getAtomNamed :: MonadIO m => String -> Display -> m Atom
getAtomNamed n dis = liftIO $ internAtom dis n False

getActiveWindowAtom :: MonadIO m => Display -> m Atom
getActiveWindowAtom = getAtomNamed "_NET_ACTIVE_WINDOW"

getNetWmNameAtom :: MonadIO m => Display -> m Atom
getNetWmNameAtom = getAtomNamed "_NET_WM_NAME"

getWMNameAtom :: MonadIO m => Display -> m Atom
getWMNameAtom = getAtomNamed "WM_NAME"

getClientListAtom :: MonadIO m => Display -> m Atom
getClientListAtom = getAtomNamed "_NET_CLIENT_LIST"

getWMDESKTOPAtom :: MonadIO m => Display -> m Atom
getWMDESKTOPAtom = getAtomNamed "_NET_WM_DESKTOP"

getDesktopNamesAtom :: MonadIO m => Display -> m Atom
getDesktopNamesAtom = getAtomNamed "_NET_DESKTOP_NAMES"

getNumberOfDesktopsAtom :: MonadIO m => Display -> m Atom
getNumberOfDesktopsAtom = getAtomNamed "_NET_NUMBER_OF_DESKTOPS"

getDesktopViewportAtom :: MonadIO m => Display -> m Atom
getDesktopViewportAtom = getAtomNamed "_NET_DESKTOP_VIEWPORT"

getCurrentDesktopAtom :: MonadIO m => Display -> m Atom
getCurrentDesktopAtom = getAtomNamed "_NET_CURRENT_DESKTOP"

getWMHintsAtom :: MonadIO m => Display -> m Atom
getWMHintsAtom = getAtomNamed "WM_HINTS"

getCurrentWindow :: MonadIO m => Display -> Window -> Atom -> m (Maybe Window)
getCurrentWindow dis rt act = fmap head <$> (liftIO $ rawGetWindowProperty 32 dis act rt)

changedTitle :: XID -> Display -> X11DataSourceM (Maybe XID) (Maybe (Maybe String))
changedTitle xid dis = do n <- liftIO $ fetchName dis xid
                          case n of
                              Nothing -> putDataState (Just xid) >> return Nothing
                              Just winName -> putDataState (Just xid) >> return (Just $ Just $ winName)

activeWindowChanged :: X11DataSourceM (Maybe XID) (Maybe (Maybe String))
activeWindowChanged = do
  dis <- asks display
  rt <- asks root
  act <- getActiveWindowAtom dis
  activeWin <- getCurrentWindow dis rt act
  lx <- getDataState
  case (lx,activeWin) of
      (Just a,Just ac) -> when (a /= ac) $ removeDependency a
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
    (asks root >>= \rt -> addEventDependency rt propertyChangeMask >> pure Nothing) -- update this later to get the current window and send the event
    Nothing
 where rawDataToUpdate :: (Maybe Text -> UpdateM a ()) -> Maybe (Maybe String) -> Maybe (UpdateM a ())
       rawDataToUpdate update = fmap (update . fmap fromString)


data WorkspaceStatus = Open | Occupied | Empty | Urgent deriving (Show,Eq)
data WorkspaceInfo = WorkspaceInfo {name :: Text, status :: WorkspaceStatus} deriving (Show,Eq)

xworkspacesPropertyChange :: Atom -> X11DataSourceM XWorkspaceState (Maybe [WorkspaceInfo])
xworkspacesPropertyChange atom =
  do dis <- asks display
     clientList <- getClientListAtom dis
     wMDESKTOP <- getWMDESKTOPAtom dis
     desktopNamesAtom <- getDesktopNamesAtom dis
     numberOfDesktopsAtom <- getNumberOfDesktopsAtom dis
     desktopViewport <- getDesktopViewportAtom dis
     currentDesktop <- getCurrentDesktopAtom dis
     hints <- getWMHintsAtom dis
     if atom == clientList || atom == wMDESKTOP then do
         rebuildClientList
         buildDesktopList
     else if atom == desktopNamesAtom || atom == numberOfDesktopsAtom || atom == desktopViewport then do
         names <- getDesktopNames
         modifyData $ \s -> s {desktopNames = names}
         rebuildDesktops
         rebuildClientList
         buildDesktopList
     else if atom == currentDesktop then do
         updateCurrentDesktop
         buildDesktopList
     else if atom == hints then do
         rebuildUrgentHints
         buildDesktopList
     else pure Nothing

-- getWMHints :: MonadIO m => Display -> m Atom
-- getWMHints = getAtomNamed "WM_HINTS"


data XWorkspaceState = XWorkspaceState {
    -- monitors :: [XineramaScreenInfo],
    desktopNames :: [Text],
    urgentDesktops :: Set Int,
    current :: Int,
    clients :: Map Window Int, -- where the client is
    numOfWindows :: Map Int Int,
    numberOfDesktops :: Int} -- desktop number number of windows

-- this is going to be a blantent copy of the polybar module (xworkspaces) for this but in haskell
-- well maybe without the stuff that they do with multimonitors because I don't know why it's needed
--
-- getMonitors :: X11DataSourceM s [XineramaScreenInfo]
-- getMonitors = do dis <- asks display
--                  liftIO $ getScreenInfo


getNumberOfDesktops :: X11DataSourceM XWorkspaceState Int
getNumberOfDesktops = do dis <- asks display
                         number <- getNumberOfDesktopsAtom dis
                         rt <- asks root
                         numberOf <- liftIO $ getWindowProperty32 dis number rt
                         case numberOf of
                             Just (x:_) -> return $ fromEnum x
                             _ -> do liftIO $ hPutStrLn stderr "faild to get number of desktops"
                                     pure 0

getDesktopNames :: X11DataSourceM XWorkspaceState [Text]
getDesktopNames = do dis <- asks display
                     rt <- asks root
                     n <- getNumberOfDesktops
                     names <- getDesktopNamesAtom dis
                     modifyData $ \s -> s {numberOfDesktops = n}
                     wins <- liftIO $ getWindowProperty8 dis names rt
                     case wins of
                         Just a -> pure $ map (decodeUtf8With (const $ const $ Just '�')) . split 0 . pack . map (toEnum . fromEnum) $ a
                         Nothing -> liftIO $ hPutStrLn stderr "failed to get names of desktop" >> pure []

getCurrentDesktop :: X11DataSourceM XWorkspaceState Int
getCurrentDesktop = do (XEnv dis rt) <- ask
                       act <- getCurrentDesktopAtom dis
                       desk <- liftIO $ getWindowProperty32 dis act rt
                       case desk of
                         Just (a:_) -> pure $ fromEnum a
                         _ -> do liftIO $ hPutStrLn stderr "failed to get current desktop"
                                 return 0

updateCurrentDesktop :: X11DataSourceM XWorkspaceState ()
updateCurrentDesktop = do d <- getCurrentDesktop
                          modifyData $ \s -> s {current = d}

getClientListIO :: Display -> Window -> IO [Window]
getClientListIO dis rt =
  do cla <- getClientListAtom dis
     clist <- getWindowProperty32 dis cla rt
     case clist of
       Nothing -> do hPutStrLn stderr "failed to get client list"
                     return []
       Just a -> pure $ map (toEnum . fromEnum) a

getClientList :: X11DataSourceM XWorkspaceState [Window]
getClientList = do (XEnv dis rt) <- ask
                   liftIO $ getClientListIO dis rt

getDesktopOf :: Window -> Atom -> X11DataSourceM XWorkspaceState Int
getDesktopOf win atom = do dis <- asks display
                           d <- liftIO $ getWindowProperty32 dis atom win
                           case d of
                               Just (desk:_) -> return $ fromEnum desk
                               _ -> do liftIO $ hPutStrLn stderr "failed to get desktop of window"
                                       return 0

rebuildClientList :: X11DataSourceM XWorkspaceState ()
rebuildClientList = do clientList <- sort <$> getClientList
                       cmap <- getsData clients
                       dis <- asks display
                       a <- getWMDESKTOPAtom dis
                       forM_ clientList $ \c -> unless (Map.member c cmap) $ addEventDependency c propertyChangeMask
                       (withDesk :: [(Window,Int)]) <- forM clientList $ \c -> (c,) <$> getDesktopOf c a
                       let wins = foldl' (\m (_,d) -> Map.insertWith (+) d 1 m) Map.empty withDesk
                           nclients= foldl' (\m (w,d) -> Map.insert w d m) Map.empty withDesk
                       modifyData $ \s -> s {clients = nclients, numOfWindows = wins}

                       rebuildUrgentHints

isUrgent :: Window -> Atom -> X11DataSourceM XWorkspaceState Bool
isUrgent win a = do dis <- asks display
                    p <- liftIO $ getWMHints dis win
                    pure $ (/=0) $ bit urgencyHintBit .&. wmh_flags p


rebuildUrgentHints :: X11DataSourceM XWorkspaceState ()
rebuildUrgentHints = do dis <- asks display
                        wins <- Map.toList <$> getsData clients
                        dnumber <- getsData numberOfDesktops
                        h <- getWMHintsAtom dis
                        (urgent :: Set Int) <- fold <$> traverse (\(win,d) -> if d > dnumber then pure Set.empty
                                                     else do u <- isUrgent win h
                                                             pure $ if u then Set.singleton d else Set.empty) wins
                        modifyData $ \s -> s {urgentDesktops = urgent}

buildDesktopList :: X11DataSourceM XWorkspaceState (Maybe [WorkspaceInfo])
buildDesktopList = do urgent <- getsData urgentDesktops
                      active <- getsData current
                      desks <- Map.toList <$> getsData numOfWindows
                      names <- getsData desktopNames
                      
                      let occupied_desks = foldMap Set.singleton $ map fst $ filter ((/=0) . snd) desks
                      pure $ Just $ map (\(i,t) -> if i == active then WorkspaceInfo t Open else
                                                   if Set.member i urgent then WorkspaceInfo t Urgent else
                                                   if Set.member i occupied_desks then WorkspaceInfo t Occupied else
                                                   WorkspaceInfo t Empty) $ zip [0..] names

rebuildDesktops :: X11DataSourceM XWorkspaceState ()
rebuildDesktops = do d <- getDesktopNames
                     modifyData $ \s -> s {desktopNames = d}

workspaces :: ([WorkspaceInfo] -> UpdateM a ()) -> X11DataSource a
workspaces f =
  X11DataSource (\ev -> do
      dis <- asks display

      r <- case ev of
               PropertyEvent _ _ _ _ _ atom _ _ -> xworkspacesPropertyChange atom
               _ -> return $ Nothing
      liftIO $ flush dis
      pure $ (r >>= Just . f)

    )
    (do
        -- mons <- getMonitors
        names <- getDesktopNames
        modifyData $ \s -> s {desktopNames = names}
        rebuildDesktops
        -- modify $ \s -> s {
        --         -- monitors = mons,
        --         desktopNames = names}
        updateCurrentDesktop
        rebuildDesktops
        rebuildClientList
        fmap f <$> buildDesktopList
        )
    (XWorkspaceState [] Set.empty 0 Map.empty Map.empty 0)

updateCurrentEventsSub :: Display -> Window -> NeededWindowEvents -> [DataSourceCurrentState a] -> IO NeededWindowEvents
updateCurrentEventsSub dis rt onev ds = do
  clist <- Map.fromList . map (,()) <$> getClientListIO dis rt
  let nev = foldl' (Map.unionWith (.|.)) (Map.empty) (map (\(DataSourceCurrentState _ (X11DataSourceState _ e)) -> e) ds)
               `Map.difference` clist -- remove old values
      intersection = Map.intersectionWith (,) nev onev
      added = Map.difference nev onev
      removed = Map.difference onev nev
  forM_ (Map.toList intersection) $ \(w,(ol,n)) ->
      if ol == n then
          return ()
      else selectInput dis w n
  forM_ (Map.toList added) $ \(w,n) -> selectInput dis w n
  forM_ (Map.toList removed) $ \(w,_) -> selectInput dis w noEventMask
  pure nev

data DataSourceCurrentState a = forall s. DataSourceCurrentState {updateF :: (Event -> X11DataSourceM s (Maybe (UpdateM a ()))),
                                                                  currentState :: X11DataSourceState s}

eventLoop :: Display -> Window -> [DataSourceCurrentState a] -> NeededWindowEvents -> TQueue (UpdateM a ()) -> IO ()
eventLoop dis rt dsources currentSub q =
       allocaXEvent $ \ev -> do
            nextEvent dis ev
            ev' <- getEvent ev
            dn <- forM dsources $ \(DataSourceCurrentState d s) ->
             do (up,s') <- runReaderT (runStateT (d ev') s) (XEnv dis rt)
                case up of
                    Just up'-> pushInputToTQueue q up'
                    Nothing -> return ()
                return $ DataSourceCurrentState d s'

            newSub <- updateCurrentEventsSub dis rt currentSub dn

            eventLoop dis rt dn newSub q

runEventLoop :: Display -> Window -> [X11DataSource a] -> TQueue (UpdateM a ()) -> IO ()
runEventLoop dis rt ds tq = do started <- mapM (startDataSource dis rt tq) ds
                               newSub <- updateCurrentEventsSub dis rt (Map.empty) started
                               eventLoop dis rt started newSub tq

startDataSource :: Display -> Window -> TQueue (UpdateM a ()) -> X11DataSource a -> IO (DataSourceCurrentState a)
startDataSource dis rt q (X11DataSource f first s) =
    do (up,s') <- (runReaderT (runStateT first (X11DataSourceState s Map.empty)) (XEnv dis rt))
       case up of
         Just up' -> pushInputToTQueue q up'
         Nothing -> return ()
       return $ DataSourceCurrentState f s'

data X11Handle = X11Handle Display Window

changeWorkspace :: X11Handle -> Int -> UpdateM a ()
changeWorkspace (X11Handle dis rt) ws = liftIO $ allocaXEvent $ \ev -> do
    setEventType ev clientMessage
    atom <- getCurrentDesktopAtom dis
    setClientMessageEvent' ev rt atom 32 [toEnum ws,0,0,0,0]
    sendEvent dis rt False (substructureRedirectMask + substructureNotifyMask) ev
    flush dis

-- the X11 datasources need to check for themselves weather they want the event that they recieved
-- currently it is not implemented yet to automatically filter out the events that aren't needed
-- I may implement that in the future
getX11DataSources :: [X11DataSource a] -> DataSourceExtensionWithHandle X11Handle a
getX11DataSources x11data = addDataSourceWithHandle $
    do tq <- newTQueueIO
       dis <- openDisplay ":0"
       let rt = defaultRootWindow dis
       flush dis
       void $ forkIO $ runEventLoop dis rt x11data tq
       return $ (DataSource (tQueueToProducer tq) $ pure (), X11Handle dis rt)

