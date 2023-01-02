-- {-# LANGUAGE OverloadedLabels #-}
-- {-# LANGUAGE NamedFieldPuns #-}
module HEww.Widgets.Hover where
    
-- import qualified GI.Gtk as Gtk
-- import qualified GI.Gtk.Objects.Widget as Gtk
-- import qualified GI.GObject as GI
-- import GI.Gtk.Declarative
-- import GI.Gtk.Declarative.State
-- import GI.Gtk.Declarative.EventSource (fromCancellation,cancel,subscribe,Subscription)
-- import Data.Vector (Vector)


-- data HoverBoxSettings a = HoverBoxSettings {onEnterEvent :: a, onLeaveEvent :: a, childWidget :: (Widget a)}

-- hoverBox :: Vector (Attribute Gtk.Box a)
--          -> HoverBoxSettings a
--          -> Widget a
-- hoverBox customAttributes customParams = Widget (
--     CustomWidget { customWidget
--                  , customCreate
--                  , customPatch
--                  , customSubscribe
--                  , customAttributes
--                  , customParams
--                  })
--   where
--     customWidget = Gtk.Box

--     customCreate :: HoverBoxSettings a -> IO (Gtk.Box, (SomeState, Gtk.EventControllerMotion))
--     customCreate props =
--      do box <- Gtk.new Gtk.Box []
--         firstState <- create $ childWidget props
--         subWidget <- someStateWidget firstState
--         #add box subWidget
--         motion <- #new Gtk.EventControllerMotion []
--         GI.set motion [#widget GI.:= box]
--         return (box,(firstState,motion))

--     customPatch :: HoverBoxSettings a -> HoverBoxSettings a -> (SomeState,Gtk.EventControllerMotion) -> CustomPatch Gtk.Box (SomeState,Gtk.EventControllerMotion)
--     customPatch old new (someState,motion) =
--         case patch old new someState of
--             Replace f -> CustomModify $ \box -> do
--                 widget <- someStateWidget someState
--                 #remove box widget
--                 newState <- f
--                 newWidget <- someStateWidget newState
--                 #add box newWidget
--                 return (newState,motion)
--             Modify f -> CustomModify $ \box -> do
--                 newState <- f
--                 return (newState,motion)
--             Keep -> CustomModify $ \box -> do -- this is just to cause it to refresh signal handlers
--                 return (someState,motion)
--     customSubscribe :: HoverBoxSettings a -> (SomeState,Gtk.EventControllerMotion) -> Gtk.Box -> (a -> IO ()) -> IO Subscription
--     customSubscribe params (someState,motion) widget f = do
--         c <- subscribe widget someState f
--         h <- GI.on motion #enter $ \_ _ -> f $ onEnterEvent params
--         h' <- GI.on motion #leave $ \_ _ -> f $ onLeaveEvent params
--         return $ fromCancellation $ (cancel c >> GI.signalHandlerDisconnect motion h >> GI.signalHandlerDisconnect motion h')



