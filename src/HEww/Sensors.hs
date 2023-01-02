module HEww.Sensors (pollSensors,SensorPoll(..)) where

import HEww.Core
import Control.Concurrent.STM.TSem

import Data.List (foldl')
import qualified Data.Map as Map
import Data.Maybe

import Foreign.Marshal.Utils
import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.C.String
import qualified Foreign.Concurrent as C (newForeignPtr)

import Control.Concurrent.STM
import Control.Concurrent
import Control.Monad
import Control.Monad.IO.Class

import System.IO
import Foreign.Storable
import Foreign.Marshal


-- foreign import ccall "wrapper" createVolumeHandle :: (Bool -> Float -> IO ()) -> IO (FunPtr (Bool -> Float -> IO ()))


data FilePtr
data ChipName
data Feature
data SubFeature

foreign import ccall "wrapper" createFinalizer :: (Ptr a -> IO ()) -> IO (FunPtr (Ptr a -> IO ()))

foreign import ccall unsafe "fopen" fopen' :: CString -> CString -> IO (Ptr FilePtr) -- I don't know what this is in haskell so I am just going to use the c
foreign import ccall unsafe "fclose" fclose :: Ptr FilePtr -> IO ()
-- foreign import ccall unsafe "free" freePtr :: Ptr a -> IO ()

foreign import ccall unsafe "sensors_init" sensorsInit :: Ptr FilePtr -> IO Int
foreign import ccall unsafe "sensors_cleanup" sensorsCleanup :: IO ()
foreign import ccall unsafe "sensors_get_value" getSensorValue :: Ptr ChipName -> Int -> Ptr Double -> IO Int

foreign import ccall unsafe "sensors_get_detected_chips" getDetectedChips' :: Ptr ChipName -> Ptr Int -> IO (Ptr ChipName)
foreign import ccall unsafe "sensors_get_features" getFeatures' :: Ptr ChipName -> Ptr Int -> IO (Ptr Feature)
foreign import ccall unsafe "sensors_get_all_subfeatures" getSubFeatures' :: Ptr ChipName -> Ptr Feature -> Ptr Int -> IO (Ptr SubFeature)
foreign import ccall unsafe "sensors_parse_chip_name" parseChipName' :: CString -> Ptr ChipName -> IO Int
foreign import ccall unsafe "sensors_free_chip_name" freeChipName :: Ptr ChipName -> IO ()

foreign import ccall unsafe "sensors_get_subfeature_name" getSubFeatureName' :: Ptr SubFeature -> IO CString
foreign import ccall unsafe "sensors_get_subfeature_number" getSubFeatureNumber :: Ptr SubFeature -> IO Int
foreign import ccall unsafe "sensors_get_feature_name" getFeatureName' :: Ptr Feature -> IO CString
foreign import ccall unsafe "size_of_chipname" sizeOfChipName :: Int

getSubFeatureName :: Ptr SubFeature -> IO String
getSubFeatureName p = getSubFeatureName' p >>= peekCAString

getFeatureName :: Ptr Feature -> IO String
getFeatureName p = getFeatureName' p >>= peekCAString

loopWhileNotNull :: (Ptr Int -> IO (Ptr b)) -> IO [Ptr b]
loopWhileNotNull f = with 0 $ \iptr -> go f iptr
    where go :: (Ptr Int -> IO (Ptr b)) -> Ptr Int -> IO [Ptr b]
          go f' iptr = do v <- f' iptr
                          if v == nullPtr then
                             pure []
                          else (v:) <$> go f' iptr

getDetectedChips :: ForeignPtr ChipName -> IO [Ptr ChipName]
getDetectedChips cn = withForeignPtr cn $ \chipName' ->
    loopWhileNotNull $ \iptr -> getDetectedChips' chipName' iptr

getFeatures :: Ptr ChipName -> IO [Ptr Feature]
getFeatures cn = loopWhileNotNull $ \iptr -> getFeatures' cn iptr

getSubFeatures :: Ptr ChipName -> Ptr Feature -> IO [Ptr SubFeature]
getSubFeatures cn f = loopWhileNotNull $ \iptr -> getSubFeatures' cn f iptr

parseChipName :: String -> IO (Maybe (ForeignPtr ChipName))
parseChipName name = do cname <- newCAString name
                        putStrLn "hello world3"
                        chipName' <- mallocBytes 24
                        putStrLn "hello world4"
                        err <- parseChipName' cname chipName'
                        putStrLn "hello world5"
                        if err == 0 then do
                            free cname
                            -- f <- createFinalizer (\c -> freeChipName c >> free c)
                            Just <$> C.newForeignPtr chipName' (freeChipName chipName' >> free chipName')
                        else do
                            free cname
                            pure Nothing

getChipName :: String -> IO (Maybe (Ptr ChipName))
getChipName name = do n <- parseChipName name
                      case n of
                          Nothing -> pure Nothing
                          Just a -> listToMaybe <$> getDetectedChips a

fopen :: String -> String -> IO (ForeignPtr FilePtr)
fopen name per = withCString name $ \n -> withCString per $ \p ->
                 do ptr <- fopen' n p
                    -- f <- createFinalizer (\pt -> fclose pt >> free pt)
                    C.newForeignPtr ptr (fclose ptr >> free ptr)



data SensorPoll a = SensorPoll {action :: Double -> UpdateM a (),
                                chipName :: String,
                                featureName :: String,
                                subFeatureName :: String,
                                pollEvery :: Int}

runPoll :: Int -> IO (UpdateM a ()) -> TQueue (UpdateM a ()) -> IO ()
runPoll time action' queue = do update <- action'
                                atomically $ writeTQueue queue update
                                liftIO $ threadDelay (time * 1000000)
                                runPoll time action' queue

runPollSensor :: Int -> (Double -> UpdateM a ()) -> Ptr ChipName -> Int -> TSem -> TQueue (UpdateM a ()) -> IO ()
runPollSensor time f chipName' subFeature mutex tq = runPoll time
    (do atomically $ waitTSem mutex
        v <- f <$> (alloca $ \dptr ->
                getSensorValue chipName' subFeature dptr *> peek dptr)
        atomically $ signalTSem mutex
        pure v) tq

pollSensors :: [SensorPoll a] -> Maybe String -> DataSourceExtension a
pollSensors polls conf = addDataSource $
  do configFile <- case conf of
       Nothing -> do newForeignPtr_ nullPtr
       Just a -> fopen a "r"

     err <- withForeignPtr configFile $ sensorsInit

     when (err /= 0) $ error "could not init sensors"

     let mapOfPolls = map (\p -> (chipName p,Map.singleton (featureName p, subFeatureName p) [(action p, pollEvery p)])) polls
         -- merged :: [(String,Map (String,String) [(Double -> UpdateM a (),Int)])]
         merged = Map.toList $ foldl' (\m (k,v) -> Map.insertWith (Map.unionWith (++)) k v m) Map.empty mapOfPolls

     putStrLn "hello world"

     mutex <- atomically $ newTSem 1
     tq <- newTQueueIO

     forM_ merged $ \(chip,listers) -> do
         putStrLn "hello world 1"
         cn <- getChipName chip
         putStrLn "hello world2"
         case cn of
             Nothing -> hPutStrLn stderr $ "could not find chip: " ++ chip
             Just chipName' ->
               do features <- getFeatures chipName'
                  forM_ features $ \feature -> do
                    fname <- getFeatureName feature
                    subFeatures <- getSubFeatures chipName' feature
                    forM_ subFeatures $ \subFeature -> do
                        sname <- getSubFeatureName subFeature
                        snumber <- getSubFeatureNumber subFeature
                        case Map.lookup (fname,sname) listers of
                            Nothing -> hPutStrLn stderr $ "no such feature->subFeature: " ++ fname ++ "->" ++ sname
                            Just ls -> forM_ ls $ \(a,e) -> forkIO $ runPollSensor e a chipName' snumber mutex tq

     putStrLn "hello world"
     pure $ DataSource (tQueueToProducer tq) $ atomically (waitTSem mutex) >> sensorsCleanup


