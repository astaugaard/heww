module HEww.X11 where

import Graphics.X11.Xlib.Event
import Graphics.X11.Xlib
import Graphics.X11.Xlib.Extras
import Pipes
import Control.Concurrent.STM
import Control.Concurrent.Async
import HEww.Core
import Data.Text (Text)
import Data.String (fromString)
import Control.Monad.Reader
import Control.Concurrent (forkIO, runInBoundThread, forkOS)
import System.IO
import Data.Bits ((.|.))

data XEnv = XEnv {display :: Display, root :: Window}

type X11DataSource a = ReaderT XEnv (Pipe Event (UpdateM a ()) IO) ()

windowTitle :: (Maybe Text -> UpdateM a ()) -> X11DataSource a
windowTitle f = do dis <- asks display
                   act <- liftIO $ internAtom dis "_NET_ACTIVE_WINDOW" False
                   n1 <- liftIO $ internAtom dis "_NET_WM_NAME" False
                   n2 <- liftIO $ internAtom dis "WM_NAME" False
                   windowTitleLoop (Nothing,Nothing) act n1 n2 f
                   liftIO $ putStrLn "end?"

iterateM_ :: Monad m => (a -> m a) -> a -> m b
iterateM_ mf s = do r <- mf s
                    iterateM_ mf r

windowTitleLoop :: (Maybe XID,Maybe String) -> Atom -> Atom -> Atom -> (Maybe Text -> UpdateM a ()) -> X11DataSource a
windowTitleLoop lv act n1 n2 f =
  iterateM_ (\s@(lx,_) ->
    do ev <- lift $ await
       rt <- asks root
       dis <- asks display
       (r,sendRet) <- liftIO $ runInBoundThread $
        do r <-
            case ev of
                PropertyEvent _ _ _ _ win atom _ _ ->
                   do if atom == act then do
                        activeWin <- fmap head <$> (liftIO $ rawGetWindowProperty 32 dis act rt) -- did I do this right?
                        case activeWin of
                            Just 0 -> case lx of
                                        Just a -> do selectInput dis a noEventMask
                                                     flush dis
                                                     return ((Nothing,Nothing),(Just Nothing))

                                        Nothing -> return ((Nothing,Nothing),Nothing)

                            Just n -> do selectInput dis n propertyChangeMask
                                         flush dis
                                         case lx of
                                            Just a -> if a == n then
                                                        return ((Just a,Nothing),Nothing)
                                                      else do selectInput dis a noEventMask
                                                              flush dis
                                                              changedTitle f n dis
                                            Nothing -> changedTitle f n dis
                            Nothing -> do hPutStrLn stderr "could not get active window"
                                          return (s,Nothing)
                      else if atom == n1 || atom == n2 then
                        case lx of
                          Nothing -> return (s,Nothing)
                          Just lw -> if lw == win then
                                        changedTitle f lw dis
                                     else return (s,Nothing)
                      else
                        return (s,Nothing)
                _ -> return (s,Nothing)
           liftIO $ flush dis
           return r
       case sendRet of
           Nothing -> return ()
           Just a -> lift $ yield $ f $ fmap fromString a
       return r) lv

changedTitle :: (Maybe Text -> UpdateM a ()) -> XID -> Display -> IO ((Maybe XID, Maybe String),Maybe (Maybe String))
changedTitle f xid dis = do n <- fetchName dis xid
                            flush dis
                            case n of
                              Nothing -> return ((Just xid,Nothing),Nothing)
                              Just name -> return ((Just xid,Just name), Just $ Just $ name)

eventLoop :: Display -> TChan (Event) -> IO ()
eventLoop dis chan = forever $ allocaXEvent $ \ev -> do
       nextEvent dis ev
       ev' <- getEvent ev
       atomically $ writeTChan chan ev'

x11DataSources :: [X11DataSource a] -> ([DataSource a] -> IO a) -> [DataSource a] ->  IO a
x11DataSources x11data = addDataSource $
    do tq <- newTQueueIO
       itc <- newBroadcastTChanIO
       dis <- openDisplay ":0"
       let rt = defaultRootWindow dis
       selectInput dis rt (propertyChangeMask)
       flush dis
       void $ forkOS $ eventLoop dis itc
       forConcurrently_ x11data $ \p ->
         do ttc <- atomically $ dupTChan itc
            forkIO $ runEffect $ tChanToProducer ttc >->
                                 runReaderT p (XEnv dis rt) >->
                                 forever (await >>= (lift . pushInputToTQueue tq))
       return $ DataSource (tQueueToProducer tq) $ pure ()

