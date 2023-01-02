{-# LANGUAGE OverloadedStrings, OverloadedLabels, OverloadedLists, LambdaCase, FlexibleContexts #-}
module Main (main) where

import GI.Gtk.Declarative.App.State
import GI.Gtk.Declarative.Widget
import GI.Gtk.Declarative
import GI.Gtk.Declarative.Container.Grid

import qualified GI.Gtk as Gtk

import Graphics.UI.GIGtkStrut

import Data.Text (Text)
import Data.String (fromString)
import Data.Vector (fromList,Vector)
import Data.Ratio
import GHC.Int ()

import Control.Monad.State

import HEww.PulseAudio
import HEww
import HEww.X11
import HEww.Polls
import HEww.Sensors
import HEww.Widgets.Scale
import Data.Time.Format

data AppState = AppState {soundHandle :: VolumeHandle,
                          x11Handle :: X11Handle,
                          soundLevel :: Float,
                          currentWindowName :: Text,
                          cpuUsage :: Int,
                          currentWorkspaces :: [WorkspaceInfo],
                          timeString :: Text,
                          temperature :: Double,
                          soundExpanded :: Bool}

renderWorkspace :: X11Handle -> (WorkspaceInfo,Int) -> Widget (UpdateM AppState ())
renderWorkspace h ((WorkspaceInfo name status),n) = widget Gtk.Button [#label := icon, on #clicked (changeWorkspace h n), classes (["icon"] ++ maybeActiveClass)]
  where icon = case status of
                  Open -> "\xf111"
                  Empty -> "\xf10c"
                  Occupied -> "\xf192"
                  Urgent -> "\xf192"
        maybeActiveClass = case status of
            Open -> ["active"]
            _ -> []

leftSide :: AppState -> Widget (UpdateM AppState ())
leftSide v = container Gtk.Box [classes ["island", "workspaces"], #orientation := Gtk.OrientationHorizontal, #halign := Gtk.AlignStart, #spacing := 2]
          $ fromList $ map (BoxChild defaultBoxChildProperties . renderWorkspace (x11Handle v)) $ zip (currentWorkspaces v) [0..]

displayInfo :: Text -> Text -> Text -> Widget (UpdateM AppState ())
displayInfo c icon text = container Gtk.Box [classes [c], #halign := Gtk.AlignCenter, #spacing := 5]
                      [ widget Gtk.Label [#label := icon, classes ["icon"]]
                      , widget Gtk.Label [#label := text]
                      ]

temp v = displayInfo "temp" "\xf2c9" $ fromString $ (show (round $ temperature v))
cpu v = displayInfo "cpu" "\xf109" $ fromString $ (show (cpuUsage v) ++ "%")
time v = displayInfo "time" "\xf073" $ timeString v

soundSlider :: AppState -> Widget (UpdateM AppState ())
soundSlider v = (\f -> liftIO $ setVolume (soundHandle v) $ realToFrac f) <$> (scale [#widthRequest := 100] $ ScaleSettings 100 0 (realToFrac $ soundLevel v) Gtk.OrientationHorizontal)

updateEvent :: UpdateM AppState ()
updateEvent = do liftIO $ putStrLn "hello world"
                 modify $ \s -> s {soundExpanded = True}

sound v = container Gtk.Box [classes ["sound"],
                             #orientation := Gtk.OrientationHorizontal,
                             #spacing := 5,
                             on #enterNotifyEvent (const (False,updateEvent)),
                             on #leaveNotifyEvent (const (False,modify $ \s -> s {soundExpanded = False}))]
            [ BoxChild defaultBoxChildProperties $ widget Gtk.Label [classes ["icon"] , #label := "\xfa7d"]
            -- , BoxChild defaultBoxChildProperties $ bin Gtk.Revealer [ #transitionType := Gtk.RevealerTransitionTypeSlideLeft
            --                                                         , #transitionDuration := 350
            --                                                         , #revealChild := soundExpanded v] $ soundSlider v
            , BoxChild defaultBoxChildProperties $ widget Gtk.Label [#label := fromString (show (round $ soundLevel v) ++ "%")]
            ]

rightSide :: AppState -> Widget (UpdateM AppState ())
rightSide v = container Gtk.Box [classes ["island"], #orientation := Gtk.OrientationHorizontal, #halign := Gtk.AlignEnd, #spacing := 15]
            [ BoxChild defaultBoxChildProperties $ time v
            , BoxChild defaultBoxChildProperties $ cpu v
            , BoxChild defaultBoxChildProperties $ temp v
            , BoxChild defaultBoxChildProperties $ sound v
            ]

middle :: AppState -> Widget (UpdateM AppState ())
middle v = container Gtk.Box [classes ["island", "windowname"], #halign := Gtk.AlignCenter, #hexpand := True, #spacing := 5]
                             [ BoxChild defaultBoxChildProperties $ widget Gtk.Label [#label := "\xf2d0", classes ["icon"]]
                             , BoxChild defaultBoxChildProperties $ widget Gtk.Label [#label := currentWindowName v]]

aview :: AppState -> Widget (UpdateM AppState ())
aview v = container Gtk.Grid [classes ["bar"], #columnHomogeneous := True]
            ([ GridChild (defaultGridChildProperties {leftAttach = 0}) $ leftSide v
             , GridChild (defaultGridChildProperties {leftAttach = 1}) $ middle v
             , GridChild (defaultGridChildProperties {leftAttach = 2}) $ rightSide v
            ])

main :: IO ()
main = void $
  runHEww $
  pollSensors [SensorPoll (\t -> modify $ \s -> s {temperature = t}) "k10temp-pci-00c3" "temp1" "temp1_input" 10] Nothing $
  getCurrentVolume (\_ v -> modify $ \s -> s {soundLevel = v}) $ \vh ->
  getX11DataSources [windowTitle (\case Nothing -> modify (\s -> s {currentWindowName = ""}); Just n -> modify (\s -> s {currentWindowName = n})),
                     workspaces (\w -> modify $ \s -> s {currentWorkspaces = w} ) ] $ \xh ->
  pollCpu (\i -> modify $ \s -> s {cpuUsage = i}) 10 $
  pollTime (\t -> modify $ \s -> s {timeString = fromString $ formatTime defaultTimeLocale "%m-%d-%y %R" t}) 15 $
  startApp "bar" (defaultStrutConfig {strutHeight = ExactSize 40, strutWidth = ScreenRatio (99 % 100), strutAlignment = Center}) (Just "/home/a/Dropbox/haskell/bar/test.css") aview (AppState vh xh 0 "not set yet" 0 [] "" 0 False)
