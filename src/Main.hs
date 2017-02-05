import System.Directory
import System.IO

import System.INotify

eventHandler :: INotify -> [IO WatchDescriptor] -> Event -> IO()
eventHandler inotify e@(Created isDirectory filePath) = print e
eventHandler inotify e = return ()

main = do
  inotify <- initINotify
  print inotify
  let watchDirectory = "/tmp/"
  wd <- addWatch
          inotify
          [Open, Close, Access, Modify, Move, Create]
          watchDirectory
          (eventHandler inotify wds)
  print wd
  putStrLn "Listens"
  getLine
  removeWatch wd
