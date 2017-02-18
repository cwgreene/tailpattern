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


advanceFileState path (FS oldPos) (State root stateMap) = do
 -- getContents closes the file. :(
 -- TODO: don't use getContents, and perform a tryCatch instead
 -- allowing us to keep the fileHandle around.
 -- TODO: Figure out if the above is worth it. What's the cost
 -- of doing hSeek? Do we get any benefit? Can we fstat to avoid
 -- catching a hGetChar execption? Or can we just block the thread
 -- instead? (This would require a mvar per file, so not in the
 -- current arch. Possible drawback: file handle exhaustion.
 -- Also, if the file is cached, then seeking is just a memory lookup.
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
handleNewFile state@(State root stateMap) fileName = do
  let path = fullPath root fileName
  newState <- advanceFileState path (FS 0) state
  return newState

handleUpdatedFile :: EventFileHandler
handleUpdatedFile state@(State root map) fileName = do
  let path = fullPath root fileName
  let fileState = Map.findWithDefault (FS 0) path map
  newState <- advanceFileState path fileState state
  return newState

eventHandler :: Glob.Pattern -> EventTypeHandler
eventHandler pattern s e@(Created isDirectory filePath) =
  patternFilter pattern filePath s handleNewFile
eventHandler pattern s e@(Modified isDirectory maybeFilePath) =
  case maybeFilePath of
    Nothing -> return s
    Just filePath -> patternFilter pattern filePath s handleUpdatedFile
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
