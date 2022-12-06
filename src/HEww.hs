
module HEww where
import qualified GI.Gtk.Declarative.App.Simple as A
import qualified GI.Gtk.Declarative as G

type UpdateM state a = StateT state Maybe a

data App state = App {widgets :: state -> G.Widget (UpdateM ()), initialState :: state, inputs :: [Producer (UpdateM ()) IO ()]}

run :: (G.Widget a -> Bin window a) -> App state -> IO state
run b (App widgets initialState inputs) = A.run $ A.App
    {
        update = \s e -> let ((_,w),s') = runState s $ runWriterT e
                         in if getAny w then Exit else Transition s' (return Nothing),
        view = \s -> b $ widgets s,
        inputs = inputs,
        initialState = initialState
    }
