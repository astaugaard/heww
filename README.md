# bar
A small library for writing bars/desktop widgets in haskell using gi-gtk-declarative.

## Installing library

If you are using stack you can add to your stack.yaml. 
```
extra-deps:
- git: https://github.com:astaugaard/heww.git
  commits: <insert current commit hash>
```
and add it to the dependencies in the packages.yaml.

You could also clone this repo and modify the app/Main.hs

## Using this library

The two main functions for using this library are the startApp and runHEww functions. The first function takes in some basic configuration of the application, and returns a function from the datasources to the main IO action that runs the app and returns the final state of the application when it is closed. The runHEww function takes that function and passes it in a empty list of data sources.
```haskell
main = runHEww $
       startApp <name> <strutConfig> <cssFile> <view function> <initialState>
```
The strutConfig perameter takes the configuration for the space that the bar reserves. It's type is the type StrutConfig from the the library https://github.com/taffybar/gtk-strut.git, which this library uses to allocate the window. The cssFile paramenter is either ```Just <filePath> ``` or if you aren't using css you can just leave it as `Nothing`. Then the view parameter is function that takes the current state and returns the current view for that state, with a event type of `UpdateM a ()`, which underthe hood is `StateT s (MaybeT IO) a`, and you can close the application with a call to fail, or update the current state with the normal state monad functions. The final parameter initialState is the state of your bar when it is started. 

To recieve data from polls/sensors etc, you wrap the call to startApp with one of the functions supplied by this library (or one of your own), these functions take any wrappers above thems datasources and then pass them into the next function the down the chain until they reach the startApp function which takes them and uses those datasources. These functions usualy take a function that will convert any info about the event that they are listening to and returns a UpdateM action that is used to update the state. Sometimes these polls will pass a handle to be used to interface with what they are listening to, to the rest of the other listeners and startApp function.

```haskell 
main = runHEww $
       pollCpu (\cpuUsage -> <something modifing the state of the application>) 10 $
       getCurrentVolume (\muted volume -> <another action>) $ \volumeHandle -> 
       startApp <name> <strutConfig> <cssFile> <view function> <initialState>
```
Also some datasources are grouped together because they are interfacing with the same thing and in this case the function that is used to listen to them takes a list specifing what needs to be polled in that group.
```haskell
main = runHEww $
       getX11DataSources [ windowTitle (\currentWindowTitleInfo -> <updateAction>)
                         , workspaces (\workspaceInfo -> <updateAction>) -- these are currently the only two inforamation polls for x11
                         ] $ \x11handle ->
       startApp ...
```

## Polls
found in HEww.Polls
the Int parameter of the functions is for how often to run the poll
```haskell
pollCpu :: (Int -> UpdateM a ()) -> Int -> DataSourceExtension a 
-- polls the current cpu usage

pollTime :: (UTCTime -> UpdateM a ()) -> Int -> DataSourceExtension a 
-- the time data structure is from the time library

pollCommand :: String -> (String -> UpdateM a ()) -> Int -> DataSourceExtension a 
-- the first parameter is the command to poll, and the update action is passed in the output of the command

pollRegulary :: UpdateM a () -> Int -> DataSourceExtension a
-- runs a updateM action regularly this is used to implement all of the other commands in this module
```

## PulseAudio
found in HEww.PulseAudio
```haskell
getCurrentVolume :: (Bool -> Float -> UpdateM a ()) -> DataSourceExtensionWithHandle VolumeHandle a
-- the action is passed in wether the volume is muted and the current volume

changeVolumeBy :: VolumeHandle -> Float -> UpdateM a ()
-- takes the volume handle from getCurrentVolume and changes the current volume by the passed in float.

setVolume :: VolumeHandle -> Float -> UpdateM a ()
-- sets the volume to the passed in Float
```

## Sensors
found in HEww.Sensors
```haskell
pollSensors :: [SensorPoll a] -> Maybe String -> DataSourceExtension a
-- the Maybe String parameter is the config file that you are using for libsensors (or Nothing)

data SensorsPoll a = SensorsPoll {
  action :: Double -> UpdateM a (),
  chipName :: String, 
  -- the chipName featureName subFeatureName fields are the names that are used to select the sensors using libsensor
  -- can be found with sensors -u
  featureName :: String,
  subFeatureName :: String,
  pollEvery :: Int}
```

## X11
found in HEww.X11
```haskell
getX11DataSources :: [X11DataSource a] -> DataSourceExtensionWithHandle X11Handle a


data X11DataSource a = forall s. X11DataSource (Event -> X11DataSourceM s (Maybe (UpdateM a ()))) 
                                               (X11DataSourceM s (Maybe (UpdateM a ()))) s
-- you only need to know this if you are creating your own X11DataSources

windowTitle :: (Maybe Text -> UpdateM a ()) -> X11DataSource a

workspaces :: ([WorkspaceInfo] -> UpdateM a ()) -> X11DataSource a

data WorkspaceInfo = WorkspaceInfo 
     { name :: Text
     , status :: WorkspaceStatus
     }
     
data WorkspaceInfo = Open | Occupied | Empty | Urgent
-- I am not sure if the urgent flag works (because I don't know how to test it)
-- Also this hasn't been tested with multiple monitors.


changeWorkspace :: X11Handle a -> Int -> UpdateM a ()
-- change to the workspace specified

addEventDependency :: Window -> EventMask -> X11DataSourceM s ()
-- EventMask from x11 library  
-- this tells the x11 group that the dataSource being implemented in is interested in a event

removeDependency :: Window -> X11DataSourceM s ()
-- says that you are no longer interested in events from window
```

## Internal Stuff
found in HEww.Core, this is mainly for implementing your own data sources
```haskell
data DataSource a = DataSource 
  { producer :: Producer (UpdateM a ()) IO () -- producer from the pipes library of the updateEvents
  , cleanUp :: IO () -- called when the application is terminated
  }

addDataSource :: IO (DataSource a) -> DataSourceExtension a 
-- helper function to make it so you don't have to worry about other data sources.

addDataSourceWithHandle :: IO (DataSource a,h) -> DataSourceExtensionWithHandle h a
-- the h parameter is the handle that you are returning.

type DataSourceExtension a = ([DataSource a] -> IO a) -> [DataSource a] -> IO a

type DataSourceExtensionWithHandle h a = (h -> [DataSource a] -> IO a) -> [DataSource a] -> IO a

-- some helper functions that I used TQueue and TChan are from the STM library
pushInputToTQueue :: TQueue a -> a -> IO ()
tQueueToProducer :: TQueue a -> Producer a IO ()
tChanToProducer :: TChan a -> Producer a IO ()
tQueueOuputToProducer :: (TQueue a -> IO ()) -> Producer a IO ()
```
