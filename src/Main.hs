import Control.Concurrent.MVar
import qualified System.FilePath.Glob as Glob
import qualified Data.Map.Strict as Map

import Data.Either
import System.Directory
import System.Environment
import System.IO
import System.IO.Error

import System.INotify

data FileState = FS Integer

data State = State String (Map.Map String FileState)
type EventTypeHandler = State -> Event -> IO State
type EventFileHandler = State -> String -> IO State

patternFilter :: Glob.Pattern -> String -> State -> (EventFileHandler) -> IO State
patternFilter pattern filePath s handler =
  if (Glob.match pattern filePath) then
    handler s filePath
  else
    return s

fullPath root path = root ++ "/" ++ path

advanceFileState (State root stateMap) fileName = do
  let path = fullPath root fileName
  let (FS oldPos) = Map.findWithDefault (FS 0) path stateMap
  fileHandle <- openFile path ReadMode
  -- What if it was just a timestamp update? What if it's shorter now?
  hSeek fileHandle AbsoluteSeek (oldPos) -- HACK! Probably need to trycatch this.
  contents <- hGetContents fileHandle
  let len = toInteger $ length contents
  putStr contents
  hFlush stdout -- Otherwise it looks like it buffers to newline.
  let newState = (State root (Map.insert path (FS (len + oldPos)) stateMap))
  hClose fileHandle
  return newState

handleNewFile :: EventFileHandler
handleNewFile = advanceFileState

handleUpdatedFile :: EventFileHandler
handleUpdatedFile = advanceFileState

eventHandler :: Glob.Pattern -> EventTypeHandler
eventHandler pattern s e@(Created isDirectory filePath) =
  patternFilter pattern filePath s handleNewFile
eventHandler pattern s e@(Modified isDirectory maybeFileName) =
  case maybeFileName of
    Nothing -> return s
    Just fileName -> patternFilter pattern fileName s handleUpdatedFile
eventHandler p s e = return s

inotifyCallback :: MVar State -> INotify -> EventTypeHandler -> Event -> IO()
inotifyCallback mvar inotify eventTypeHandler e = do
  oldState <- takeMVar mvar
  result <- tryIOError (eventTypeHandler oldState e)
  newState <-
        case result of
            (Left error)  -> (putStrLn $ show error) >> return oldState
            (Right nextState) -> return nextState
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
  mvar <- newMVar (State watchDirectory Map.empty)
  wd <- addWatch
          inotify
          [Open, Close, Access, Modify, Move, Create]
          watchDirectory
          (inotifyCallback mvar inotify (eventHandler pattern))
  getLine
  (State root files) <- takeMVar mvar
  putStrLn $ show $ Map.keys files
  removeWatch wd
