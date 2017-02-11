import Control.Concurrent.MVar
import qualified System.FilePath.Glob as Glob
import qualified Data.Map.Strict as Map

import System.Directory
import System.Environment
import System.IO

import System.INotify

type State = Map.Map String String
type EventTypeHandler = State -> Event -> State
type EventFileHandler = State -> String -> State

patternFilter :: Glob.Pattern -> String -> State -> (EventFileHandler) -> State
patternFilter pattern filePath s handler =
  if (Glob.match pattern filePath) then
    handler s filePath
  else
    s

eventHandler :: Glob.Pattern -> EventTypeHandler
eventHandler pattern s e@(Created isDirectory filePath) =
  patternFilter pattern filePath s $ \s fp -> Map.insert fp "fileHandleHere" s
eventHandler pattern s e@(Modified isDirectory maybeFilePath) =
  case maybeFilePath of
    Nothing -> s
    Just filePath -> patternFilter pattern filePath s $ \s fp -> Map.insert fp "modified" s
eventHandler p s e = s

inotifyCallback :: MVar State -> INotify -> EventTypeHandler -> Event -> IO()
inotifyCallback mvar inotify eventTypeHandler e = do
  state <- takeMVar mvar
  print e
  let newState = eventTypeHandler state e
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
