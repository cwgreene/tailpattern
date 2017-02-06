import Control.Concurrent.MVar
import qualified Data.Map.Strict as Map

import System.Directory
import System.IO

import System.INotify

eventHandler :: MVar (Map.Map String String) -> INotify -> Event -> IO()
eventHandler mvar inotify e@(Created isDirectory filePath) = do
  state <- takeMVar mvar
  print e
  let newState = Map.insert filePath "fileHandleHere" state
  putMVar mvar newState
eventHandler mvar inotify e = return ()

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
