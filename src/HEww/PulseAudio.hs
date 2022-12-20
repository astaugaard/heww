-- {-# LANGUAGE CApiFFI #-}
{-# LANGUAGE ForeignFunctionInterface #-}
module HEww.PulseAudio (VolumeHandle, getCurrentVolume, changeVolumeBy) where

import Foreign.Ptr
import HEww.Core
import Control.Concurrent.STM.TQueue
import Control.Concurrent.STM
-- import Pipes

data VolumeHandlePtrType


data VolumeHandle = VolumeHandle (FunPtr (Bool -> Float -> IO ())) (Ptr VolumeHandlePtrType)

foreign import ccall "wrapper" createVolumeHandle :: (Bool -> Float -> IO ()) -> IO (FunPtr (Bool -> Float -> IO ()))

foreign import ccall safe "run_volume_handle" runVolumeHandle' :: FunPtr (Bool -> Float -> IO ()) -> IO (Ptr VolumeHandlePtrType)
foreign import ccall safe "close_handle" closeHandle' :: (Ptr VolumeHandlePtrType) -> IO ()
foreign import ccall safe "changeVolumeBy" changeVolumeBy' :: (Ptr VolumeHandlePtrType) -> Float -> IO ()

runVolumeHandle :: (Bool -> Float -> IO ()) -> IO VolumeHandle
runVolumeHandle f = do fw <- createVolumeHandle f
                       hp <- runVolumeHandle' fw
                       return $ VolumeHandle fw hp

closeHandle :: VolumeHandle -> IO ()
closeHandle (VolumeHandle fw hp) = do closeHandle' hp
                                      freeHaskellFunPtr fw

getCurrentVolume :: (Bool -> Float -> UpdateM a ()) -> DataSourceExtensionWithHandle VolumeHandle a
getCurrentVolume f = addDataSourceWithHandle $
    do tq <- atomically $ newTQueue
       vh <- runVolumeHandle (\muted volume -> pushInputToTQueue tq (f muted volume))
       return $ (DataSource (tQueueToProducer tq) (closeHandle vh),vh)

changeVolumeBy :: VolumeHandle -> Float -> IO ()
changeVolumeBy (VolumeHandle _ ptr) f = changeVolumeBy' ptr f
