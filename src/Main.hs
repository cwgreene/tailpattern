import System.Directory
import System.IO

import System.INotify

eventHandler :: Event -> IO()
eventHandler e@(Created isDirectory filePath) = handleCreate e
eventHandler e = return ()

main = do
  inotify <- initINotify
  print inotify
  let watchDirectory = "/tmp/"
  wd <- addWatch
          inotify
          [Open, Close, Access, Modify, Move, Create]
          watchDirectory
          eventHandler
  print wd
  putStrLn "Listens"
  getLine
  removeWatch wd
