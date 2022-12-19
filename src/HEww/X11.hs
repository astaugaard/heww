{-# LANGUAGE ExistentialQuantification #-}
module HEww.X11 where
    
import Graphics.X11.Xlib.Event
import Graphics.X11.Xlib
import Graphics.X11.Xlib.Extras

import Pipes

import Control.Concurrent.STM

import HEww.Core

import Data.Text (Text)
import Data.String (fromString)

import Control.Monad.Reader
import Control.Concurrent (forkIO)
import Control.Monad.State

import System.IO

data XEnv = XEnv {display :: Display, root :: Window}

data X11DataSource a = forall s. X11DataSource (Event -> StateT s (ReaderT XEnv IO) (Maybe (UpdateM a ()))) s

windowTitle :: (Maybe Text -> UpdateM a ()) -> X11DataSource a
windowTitle f =
  X11DataSource (\ev ->
    do dis <- asks display
       act <- liftIO $ internAtom dis "_NET_ACTIVE_WINDOW" False
       n1 <- liftIO $ internAtom dis "_NET_WM_NAME" False
       n2 <- liftIO $ internAtom dis "WM_NAME" False
       rt <- asks root
       s@(lx,_) <- get
       (r,sendRet) <- liftIO $
        do r <-
            case ev of
                PropertyEvent _ _ _ _ win atom _ _ ->
                   do if atom == act then do
                        activeWin <- fmap head <$> (liftIO $ rawGetWindowProperty 32 dis act rt) -- did I do this right?
                        case activeWin of
                            Just 0 -> case lx of
                                        Just a -> do selectInput dis a noEventMask
                                                     return ((Nothing,Nothing),(Just Nothing))
                                        Nothing -> return ((Nothing,Nothing),Nothing)
                            Just n -> do selectInput dis n propertyChangeMask
                                         case lx of
                                            Just a -> if a == n then
                                                        return ((Just a,Nothing),Nothing)
                                                      else do selectInput dis a noEventMask
                                                              changedTitle n dis
                                            Nothing -> changedTitle n dis
                            Nothing -> do hPutStrLn stderr "could not get active window"
                                          return (s,Nothing)
                      else if atom == n1 || atom == n2 then
                        case lx of
                          Nothing -> return (s,Nothing)
                          Just lw -> if lw == win then
                                        changedTitle lw dis
                                     else return (s,Nothing)
                      else
                        return (s,Nothing)
                _ -> return (s,Nothing)
           liftIO $ flush dis
           return r
       put r
       case sendRet of
           Nothing -> return Nothing
           Just a -> return $ Just $ f $ fmap fromString a)
    (Nothing,Nothing)

changedTitle :: XID -> Display -> IO ((Maybe XID, Maybe String),Maybe (Maybe String))
changedTitle xid dis = do n <- fetchName dis xid
                          case n of
                              Nothing -> return ((Just xid,Nothing),Nothing)
                              Just name -> return ((Just xid,Just name), Just $ Just $ name)

eventLoop :: Display -> Window -> [X11DataSource a] -> TQueue (UpdateM a ()) -> IO ()
eventLoop dis rt dsources q =
       allocaXEvent $ \ev -> do
            nextEvent dis ev
            ev' <- getEvent ev
            dn <- forM dsources $ \(X11DataSource d s) ->
             do (up,s') <- runReaderT (runStateT (d ev') s) (XEnv dis rt)
                case up of
                    Just up'-> pushInputToTQueue q up'
                    Nothing -> return ()
                return $ X11DataSource d s'
            eventLoop dis rt dn q
       -- atomically $ writeTChan chan ev'

x11DataSources :: [X11DataSource a] -> ([DataSource a] -> IO a) -> [DataSource a] ->  IO a
x11DataSources x11data = addDataSource $
    do tq <- newTQueueIO
       dis <- openDisplay ":0"
       let rt = defaultRootWindow dis
       selectInput dis rt (propertyChangeMask)
       flush dis
       void $ forkIO $ eventLoop dis rt x11data tq
       return $ DataSource (tQueueToProducer tq) $ pure ()

