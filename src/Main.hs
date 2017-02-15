import Control.Concurrent.MVar
import qualified System.FilePath.Glob as Glob
import qualified Data.Map.Strict as Map

import System.Directory
import System.Environment
import System.IO

import System.INotify

type State = Map.Map String String
type EventTypeHandler = State -> Event -> IO State
type EventFileHandler = State -> String -> IO State

patternFilter :: Glob.Pattern -> String -> State -> (EventFileHandler) -> IO State
patternFilter pattern filePath s handler =
  if (Glob.match pattern filePath) then
    handler s filePath
  else
    return s

handleNewFile :: EventFileHandler
handleNewFile oldState path = do
    let newState = Map.insert path "fileHandleHere" oldState
    -- cat file here, and update length of file
    -- need to change state to Map.Map String (FileHandle length)
    return newState

handleUpdatedFile :: EventFileHandler
handleUpdatedFile oldState path = do
    let newState = oldState
    -- find position from the old state seek to it, and resume reading to EOF
    -- update state to new length
    return newState

eventHandler :: Glob.Pattern -> EventTypeHandler
eventHandler pattern s e@(Created isDirectory filePath) =
  patternFilter pattern filePath s $ \s fp -> return $ Map.insert fp "fileHandleHere" s
eventHandler pattern s e@(Modified isDirectory maybeFilePath) =
  case maybeFilePath of
    Nothing -> return s
    Just filePath -> patternFilter pattern filePath s $ \s fp -> return $ Map.insert fp "modified" s
eventHandler p s e = return s

inotifyCallback :: MVar State -> INotify -> EventTypeHandler -> Event -> IO()
inotifyCallback mvar inotify eventTypeHandler e = do
  oldState <- takeMVar mvar
  newState <- eventTypeHandler oldState e
  putMVar mvar newState

main = do
  args <- getArgs
  if length args /= 2 then
    -- TODO: Make this not silly.
    fail "Usage: tailpattern DIRECTORY PATTERN"
  else
    -- Not what one would expect in something like Python.
    return ()
  let watchDirectory = args !! 0
  let patternGlob = args !! 1
  let pattern = Glob.compile patternGlob
  inotify <- initINotify
  mvar <- newMVar Map.empty
  wd <- addWatch
          inotify
          [Open, Close, Access, Modify, Move, Create]
          watchDirectory
          (inotifyCallback mvar inotify (eventHandler pattern))
  getLine
  files <- takeMVar mvar
  putStrLn $ show $ Map.keys files
  removeWatch wd
