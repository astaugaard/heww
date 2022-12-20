module HEww.Core (DataSource(..), addDataSource, pushInputToTQueue, tQueueToProducer, UpdateM,addDataSourceWithHandle, tChanToProducer, DataSourceExtension, tQueueOuputToProducer, DataSourceExtensionWithHandle) where

import Pipes
import Control.Concurrent.STM.TQueue
import Control.Concurrent.STM.TChan
import Control.Concurrent.STM
import Control.Concurrent (forkIO)

import GI.Gtk.Declarative.App.State

data DataSource a = DataSource {producer :: Producer (UpdateM a ()) IO (), cleanUp :: IO ()}

type DataSourceExtension a = ([DataSource a] -> IO a) -> [DataSource a] -> IO a

type DataSourceExtensionWithHandle h a = (h -> [DataSource a] -> IO a) -> [DataSource a] -> IO a

addDataSource :: IO (DataSource a) -> DataSourceExtension a
addDataSource ds f dss = do ds' <- ds
                            f $ ds' : dss

addDataSourceWithHandle :: IO (DataSource a,h) -> DataSourceExtensionWithHandle h a
addDataSourceWithHandle dat f dss = do (ds,v) <- dat
                                       f v (ds:dss)

pushInputToTQueue :: TQueue a -> a -> IO ()
pushInputToTQueue q i = atomically $ writeTQueue q i

tQueueToProducer :: TQueue a -> Producer a IO ()
tQueueToProducer tq = do v <- liftIO $ atomically $ readTQueue tq
                         yield v
                         tQueueToProducer tq

tChanToProducer :: TChan a -> Producer a IO ()
tChanToProducer tc = do v <- liftIO $ atomically $ readTChan tc
                        yield v
                        tChanToProducer tc

tQueueOuputToProducer :: (TQueue a -> IO ()) -> Producer a IO ()
tQueueOuputToProducer f = do tq <- liftIO $ newTQueueIO
                             void $ liftIO $ forkIO $ f tq
                             tQueueToProducer tq
