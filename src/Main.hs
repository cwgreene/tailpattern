import Control.Concurrent.MVar
import qualified Data.Map.Strict as Map

import System.Directory
import System.IO

import System.INotify

type State = Map.Map String String

handler :: State -> Event -> State
handler s e@(Created isDirectory filePath) = Map.insert filePath "fileHandleHere" s
handler s e = s

eventHandler :: MVar (Map.Map String String) -> INotify -> Event -> IO()
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
