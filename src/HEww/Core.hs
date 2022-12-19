module HEww.Core (DataSource(..), addDataSource, pushInputToTQueue, tQueueToProducer, UpdateM,addDataSourceWithHandle, tChanToProducer) where

import Pipes
import Control.Concurrent.STM.TQueue
import Control.Concurrent.STM.TChan
import Control.Concurrent.STM

import GI.Gtk.Declarative.App.State

data DataSource a = DataSource {producer :: Producer (UpdateM a ()) IO (), cleanUp :: IO ()}

addDataSource :: IO (DataSource a) -> ([DataSource a] -> IO a) -> [DataSource a] -> IO a
addDataSource ds f dss = do ds' <- ds
                            f $ ds' : dss

addDataSourceWithHandle :: IO (DataSource a,b) -> (b -> [DataSource a] -> IO a) -> [DataSource a] -> IO a
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
