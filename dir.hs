import System.Environment ( getArgs )
import System.Directory ( getDirectoryContents, doesDirectoryExist )
import Control.Monad ( mapM_, filterM )

main :: IO ()
main =
    getArgs
    >>=
    ( \args -> filterM doesDirectoryExist args )
    >>=
    ( \args' -> mapM_ listDir args' )

listDir :: String -> IO ()
listDir dir =
    getDirectoryContents dir
    >>=
    ( \ents -> mapM_ putStrLn ents )
