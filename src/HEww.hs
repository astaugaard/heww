{-# LANGUAGE OverloadedLabels #-}
module HEww ( StrutConfig(..)
            , StrutAlignment(..)
            , StrutSize(..)
            , StrutPosition(..)
            , runHEww
            , DataSource(..)
            , startApp
            , defaultStrutConfig
            ) where
import Graphics.UI.GIGtkStrut
import GI.Gtk.Declarative.App.State
-- import GI.Gtk.Declarative.Widget
import GI.Gtk.Declarative
import qualified GI.Gdk as Gdk

import Data.Text (Text,unpack)
import qualified Data.ByteString as B

import System.Posix.Signals
import HEww.Core
import qualified GI.Gtk as Gtk
import qualified Control.Concurrent.Async as Async
import Control.Exception (finally)
import Control.Monad (void)


runHEww :: ([DataSource a] -> IO a)
       -> IO a
runHEww f = f []


startApp :: Text -> StrutConfig -> Maybe Text -> (a -> Widget (UpdateM a ())) -> a -> [DataSource a] -> IO a
startApp name struts css aview startState dataSources =
  do let cleanUpAll = (traverse cleanUp dataSources >> Gtk.mainQuit)

     void $ Gtk.init Nothing



     void $ Async.async Gtk.main

     window <- buildStrutWindow struts
     window `Gtk.set` [#name Gtk.:= name, #decorated Gtk.:= False]
     void $ Gtk.on window #destroy cleanUpAll


     case css of
         Just file -> do screen <- maybe (fail "No screen?!") return =<< Gdk.screenGetDefault
                         vis <- #getRgbaVisual screen
                         #setVisual window vis
                         styles <- B.readFile $ unpack file
                         p <- Gtk.cssProviderNew
                         Gtk.cssProviderLoadFromData p styles
                         Gtk.styleContextAddProviderForScreen screen p (fromIntegral Gtk.STYLE_PROVIDER_PRIORITY_USER)
         Nothing -> pure ()

     #showAll window
                                                            -- don't know a better way of doing this
     void $ installHandler sigINT (CatchOnce $ cleanUpAll >> raiseSignal sigINT) Nothing -- cleanup if killed with ^C

     (runLoop (App {view = aview,
                    initialState = startState,
                    inputs = (map producer dataSources)})
              window)
           `finally` cleanUpAll


