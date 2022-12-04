{-# LANGUAGE OverloadedStrings, OverloadedLabels #-}
module Main (main) where

import qualified GI.Gtk as Gtk
-- import qualified GI.Gdk as Gdk
import Data.GI.Base
import Text.Printf
import Control.Monad (void)
-- import Data.Maybe (fromJust)
import Graphics.UI.GIGtkStrut
import GHC.Int (Int32)
-- import qualified GI.GdkX11.Objects.X11Window as XWin
-- import qualified Graphics.X11.Xlib.Extras as Xlib

barSize :: GHC.Int.Int32
barSize = 20

main :: IO ()
main = do void $ Gtk.init Nothing
          mv <- Gtk.getMajorVersion
          minv <- Gtk.getMinorVersion
          micv <- Gtk.getMicroVersion
          printf "Gtk %d.%d.%d" mv minv micv

          window <- buildStrutWindow (defaultStrutConfig {strutHeight = ExactSize barSize}) -- new Gtk.Window [#name := "bar", #typeHint := Gdk.WindowTypeHintDock, #decorated := False]
          void $ on window #destroy Gtk.mainQuit
          #showAll window
          Gtk.main


          -- scr <- #getScreen window
          -- w <- #getWidth scr

          -- dis <- #getDisplay scr
          -- nmons <- #getNMonitors dis
          -- monitors <- forM [0..nmons-1] $ \m ->
          --   do mg <- (#getMonitor dis m >>= #getGeometry . fromJust :: IO Gdk.Rectangle)
          --      w <- get mg #width
          --      h <- get mg #height
          --      putStrLn $ "monitor " ++ show m ++ ": " ++ show w ++ " x " ++ show h
          --      return mg
          -- act <- fromJust <$> #getActiveWindow scr
          -- curmon <- (Gdk.displayGetMonitorAtWindow dis act :: IO (Gdk.Monitor))
          -- cgeo <- get curmon #geometry
          -- case cgeo of
          --     Nothing -> putStrLn "no monitor found"
          --     Just cgeo -> do
          --         x <- get cgeo #x
          --         y <- get cgeo #y
          --         width <- get cgeo #width
          --         height <- get cgeo #height
          --         printf "monitor: %s x %s (current, offset %s)" (show width) (show height) (show x)
          --         printf "bar: start=%d end=%d" x (x+width-1)
          --         #move window x y
          --         #resize window width barSize

          --         -- Xlib.changeProperty32 dis xwin (#internAtom dis "_NET_WM_STRUT") (#internAtom dis "_CARDINAL") Xlib.propModeReplace [0, 0, barSize, 0]
          --         -- setStrut window $ zeroStrutSettings {top = barSize, _top_start_x = x, }
          --         -- setupStrutWindow  window

