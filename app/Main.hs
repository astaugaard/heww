{-# LANGUAGE OverloadedStrings, OverloadedLabels, OverloadedLists, LambdaCase #-}
module Main (main) where

import GI.Gtk.Declarative.App.State
import GI.Gtk.Declarative.Widget
import GI.Gtk.Declarative
import qualified GI.Gtk as Gtk
import Graphics.UI.GIGtkStrut

import Data.Text (Text)
import Data.String (fromString)
import Data.Vector ()
import GHC.Int ()

import Control.Monad.State

import HEww.PulseAudio
import HEww
import HEww.X11
import HEww.Polls

data AppState = AppState {soundHandle :: VolumeHandle, soundLevel :: Float, currentWindowName :: Text, cpuUsage :: Int}

aview :: AppState -> Widget (UpdateM AppState ())
aview v = container Gtk.Box []
            ([ BoxChild defaultBoxChildProperties $ widget Gtk.Button [ #label := "-", on #clicked (liftIO $ changeVolumeBy (soundHandle v) $ (-0.05))]
            , BoxChild defaultBoxChildProperties $ widget Gtk.Label [ #label := fromString (show $ soundLevel v)]
            , BoxChild defaultBoxChildProperties $ widget Gtk.Button [ #label := "+", on #clicked (liftIO $ changeVolumeBy (soundHandle v) $ 0.05)]
            , BoxChild defaultBoxChildProperties $ widget Gtk.Label [ #label := currentWindowName v]
            , BoxChild defaultBoxChildProperties $ widget Gtk.Label [ #label := fromString ("  " ++ (show $ cpuUsage v) ++ "%")]
            ])

main :: IO ()
main = void $
  runHEww $
  getCurrentVolume (\_ v -> modify $ \s -> s {soundLevel = v}) $ \vh ->
  getX11DataSources [windowTitle (\case Nothing -> modify (\s -> s {currentWindowName = ""}); Just n -> modify (\s -> s {currentWindowName = n}))] $
  pollCpu (\i -> modify $ \s -> s {cpuUsage = i}) 10 $
  startApp "bar" (defaultStrutConfig {strutHeight = ExactSize 20}) aview (AppState vh 0 "not set yet" 0)
