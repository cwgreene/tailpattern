import Control.Concurrent.MVar
import qualified System.FilePath.Glob as Glob
import qualified Data.Map.Strict as Map

import Data.Either
import System.Directory
import System.Environment
import System.IO
import System.IO.Error

import System.INotify

data FileState = FS Int (Handle)

data State = State String (Map.Map String FileState)
type EventTypeHandler = State -> Event -> IO State
type EventFileHandler = State -> String -> IO State

patternFilter :: Glob.Pattern -> String -> State -> (EventFileHandler) -> IO State
patternFilter pattern filePath s handler =
  if (Glob.match pattern filePath) then
    handler s filePath
  else
    return s

handleNewFile :: EventFileHandler
handleNewFile (State root stateMap) fileName = do
  let path = root ++ "/" ++ fileName
  putStrLn ("saw: " ++ path)
  fileHandle <- openFile path ReadMode
  contents <- hGetContents fileHandle
  let len = length contents
  putStr contents
  let newState = (State root (Map.insert path (FS len fileHandle) stateMap))
  return newState

handleUpdatedFile :: EventFileHandler
handleUpdatedFile oldState path = do
  let newState = oldState
  putStrLn $ "Updating file: " ++ path
  -- find position from the old state seek to it, and resume reading to EOF
  -- update state to new length
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
