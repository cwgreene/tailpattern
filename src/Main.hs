import Control.Concurrent.MVar
import qualified System.FilePath.Glob as Glob
import qualified Data.Map.Strict as Map

import System.Directory
import System.Environment
import System.IO

import System.INotify

type State = Map.Map String String
type EventTypeHandler = State -> Event -> State

patternFilter :: Glob.Pattern -> String -> State -> (State -> String -> State) -> State
patternFilter pattern filePath s handler =
  if (Glob.match pattern filePath) then
    handler s filePath
  else
    s

handler :: Glob.Pattern -> State -> Event -> State
handler pattern s e@(Created isDirectory filePath) =
  patternFilter pattern filePath s $ \s fp -> Map.insert fp "fileHandleHere" s
handler pattern s e@(Modified isDirectory maybeFilePath) =
  case maybeFilePath of
    Nothing -> s
    Just filePath -> patternFilter pattern filePath s $ \s fp -> Map.insert fp "modified" s
handler p s e = s

eventHandler :: MVar State -> INotify -> EventTypeHandler -> Event -> IO()
eventHandler mvar inotify handler e = do
  state <- takeMVar mvar
  print e
  let newState = handler state e
  putMVar mvar newState

main = do
  args <- getArgs
  if length args /= 2 then
    -- TODO: Make this not silly.
    fail "Invalid Number of Supplied Arguments"
  else
    -- Not what one would expect in something like Python.
    return ()
  let watchDirectory = args !! 0
  let patternGlob = args !! 1
  let pattern = Glob.compile patternGlob
  inotify <- initINotify
  print inotify
  mvar <- newMVar Map.empty
  wd <- addWatch
          inotify
          [Open, Close, Access, Modify, Move, Create]
          watchDirectory
          (eventHandler mvar inotify (handler pattern))
  print wd
  putStrLn "Listens"
  getLine
  files <- takeMVar mvar
  putStrLn $ show $ Map.keys files
  removeWatch wd
