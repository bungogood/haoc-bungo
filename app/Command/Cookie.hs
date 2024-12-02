module Command.Cookie (writeCookie) where

import System.Directory (getHomeDirectory)
import System.FilePath ((</>))

writeCookie :: String -> IO ()
writeCookie cookie = do
  homeDir <- getHomeDirectory
  let cookieFile = homeDir </> ".adventofcode.session"
  writeFile cookieFile (cookie ++ "\n")
  putStrLn $ "Session cookie written to " ++ cookieFile
