{-# LANGUAGE OverloadedStrings, OverloadedLabels #-}
module Main (main) where

import qualified GI.Gtk as Gtk
import Data.GI.Base
import Text.Printf
import Control.Monad (void)
import Graphics.UI.GIGtkStrut
import GHC.Int (Int32)

barSize :: GHC.Int.Int32
barSize = 20

-- widget    state -> IO (Widget stateUpdate)
-- side functions
-- liftState bring a widget with one state and state change to another state
-- liftEvent
--
-- pros
-- easier to implement (alot (I think))
--
-- cons
-- more ad-hoc and explict state heavy
--
-- vs
--
-- widget   =   behavior (Widget Event)
-- pros
-- more elegant
--
-- cons
-- would have to overide every constructor in gi-gtk-declarative to support it
-- not sure how to write 100%
--

main :: IO ()
main = do void $ Gtk.init Nothing
          mv <- Gtk.getMajorVersion
          minv <- Gtk.getMinorVersion
          micv <- Gtk.getMicroVersion
          printf "Gtk %d.%d.%d" mv minv micv

          window <- buildStrutWindow (defaultStrutConfig {strutHeight = ExactSize barSize})
          window `set` [#name := "bar", #decorated := False]
          void $ on window #destroy Gtk.mainQuit

          #showAll window
          Gtk.main

