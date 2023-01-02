{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
module HEww.Widgets.Scale where
    
import qualified GI.Gtk as Gtk
import qualified GI.Gtk.Objects.Widget as Gtk
import qualified GI.GObject as GI
import GI.Gtk.Declarative
import Data.Vector (Vector)
import GI.Gtk.Declarative.EventSource ( fromCancellation, Subscription )


data ScaleSettings = ScaleSettings {maxV :: Double, minV :: Double, current :: Double, orientation :: Gtk.Orientation} deriving Eq

scale :: Vector (Attribute Gtk.Scale Double)
         -> ScaleSettings
         -> Widget Double
scale customAttributes customParams = Widget (
    CustomWidget { customWidget
                 , customCreate
                 , customPatch
                 , customSubscribe
                 , customAttributes
                 , customParams
                 })
  where
    customWidget = Gtk.Scale

    customCreate :: ScaleSettings -> IO (Gtk.Scale, ())
    customCreate props =
     do ad <- Gtk.adjustmentNew (current props) (minV props) (maxV props) 0 0 0
        scale <- Gtk.scaleNew (orientation props) $ Just ad
        return (scale,())

    customPatch :: ScaleSettings -> ScaleSettings -> () -> CustomPatch Gtk.Scale ()
    customPatch old new _
      | old == new = CustomKeep
      | otherwise = CustomModify $ \scale -> do
          ad <- Gtk.adjustmentNew (current new) (minV new) (maxV new) 0 0 0
          scale `Gtk.set` [#orientation Gtk.:= orientation new, #adjustment Gtk.:= ad]
          pure ()

    customSubscribe :: ScaleSettings -> () -> Gtk.Scale -> (Double -> IO ()) -> IO Subscription
    customSubscribe params () widget f = do
        h <- GI.on widget #changeValue $ \_ v -> f v >> pure True
        return $ fromCancellation $ GI.signalHandlerDisconnect widget h



