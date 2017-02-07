import Control.Concurrent.MVar
import qualified System.FilePath.Glob as Glob
import qualified Data.Map.Strict as Map

import System.Directory
import System.IO

import System.INotify

type State = Map.Map String String

patternFilter :: Glob.Pattern -> String -> State -> (State -> String -> State) -> State
patternFilter pattern filePath s handler =
  if (Glob.match pattern filePath) then
    handler s filePath
  else
    s

pat = Glob.compile "doom*"

handler :: State -> Event -> State
handler s e@(Created isDirectory filePath) =
  patternFilter pat filePath s $ \s fp -> Map.insert fp "fileHandleHere" s
handler s e@(Modified isDirectory maybeFilePath) =
  case maybeFilePath of
    Nothing -> s
    Just filePath -> patternFilter pat filePath s $ \s fp -> Map.insert fp "modified" s
handler s e = s

eventHandler :: MVar State -> INotify -> Event -> IO()
eventHandler mvar inotify e = do
  state <- takeMVar mvar
  print e
  let newState = handler state e
  putMVar mvar newState

main = do
  inotify <- initINotify
  print inotify
  let watchDirectory = "/tmp/"
  mvar <- newMVar Map.empty
  wd <- addWatch
          inotify
          [Open, Close, Access, Modify, Move, Create]
          watchDirectory
          (eventHandler mvar inotify)
  print wd
  putStrLn "Listens"
  getLine
  files <- takeMVar mvar
  putStrLn $ show $ Map.keys files
  removeWatch wd
