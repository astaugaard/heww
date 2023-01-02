{-# LANGUAGE BangPatterns #-}
module HEww.Polls (pollCpu,pollTime,pollComand,pollRegularly,runPoll) where
    
import HEww.Core
import System.Process hiding (runCommand)
import Control.Concurrent (threadDelay)
import Pipes
import Data.Time.Clock
import System.IO (hGetContents, hClose)

runPoll :: UpdateM a () -> Int -> Producer (UpdateM a ()) IO ()
runPoll u seconds = do yield u
                       liftIO $ threadDelay (seconds * 1000000)
                       runPoll u seconds

pollRegularly :: UpdateM a () -> Int -> DataSourceExtension a
pollRegularly action seconds = addDataSource $ pure $ DataSource (runPoll action seconds) (pure ())

runCommand :: String -> IO String
runCommand c = do (i,_,_,_) <- runInteractiveCommand c
                  s <- hGetContents i
                  hClose i
                  return s

pollComand :: String -> (String -> UpdateM a ()) -> Int -> DataSourceExtension a
pollComand action toUpdate = pollRegularly (liftIO (runCommand action) >>= toUpdate)

pollTime :: (UTCTime -> UpdateM a ()) -> Int -> DataSourceExtension a
pollTime f = pollRegularly (liftIO getCurrentTime >>= f)

getCpuPercent :: IO Int
getCpuPercent = do !before <- map read . tail . words . head . lines <$> readFile "/proc/stat"
                   threadDelay 200000
                   after <- map read . tail . words . head . lines <$> readFile "/proc/stat"
                   let dif = zipWith (-) after before :: [Float]
                       total = sum dif
                       idle = dif !! 3 + dif !! 4
                   return $ floor $ (1 - (idle / total)) * 100

pollCpu :: (Int -> UpdateM a ()) -> Int -> DataSourceExtension a
pollCpu f = pollRegularly (liftIO getCpuPercent >>= f)

