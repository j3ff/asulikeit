import System.Environment ( getArgs )
import System.Directory ( getDirectoryContents, doesDirectoryExist, doesFileExist )
import Control.Monad ( mapM_, filterM )
import Text.Regex.Posix
import Data.Tree
import System.IO
import Debug.Trace ( trace )

import MyTree


pattern = "\\s+public\\s+class\\s+(\\w+)\\s+extends\\s+(\\w+)"

main :: IO ()
main =
    getArgs
    >>=
    ( \args -> filterM doesDirectoryExist args )
    >>=
    ( \args' -> makeClassHierachy' ( head args' )  )


printDir :: String -> IO ()
printDir d = putStrLn d

makeClassHierachy' :: String -> IO ()
makeClassHierachy' dir =
    do
        ents <- getDirectoryContents dir
        filenames <-filterM doesFileExist ( map ( \s -> dir ++ "/" ++ s ) ents )
        handles <-mapM myOpenFile filenames
        contents <-mapM hGetContents handles
        contents' <- return ( filter containsDerivedClassDef contents )
        pairs <- return ( map getClassAndParent contents' )
        smallTrees <- return ( map makeTree pairs )
        forest <-return ( reduceForest smallTrees )
        putStrLn ( drawForest forest )
        mapM_ hClose handles



myOpenFile :: String -> IO Handle
myOpenFile filename = openFile filename ReadMode

containsDerivedClassDef :: String -> Bool
containsDerivedClassDef text =
    text =~ pattern :: Bool

getClassAndParent :: String -> ( String, String )
getClassAndParent s =
    let
        ( a, b, c, ( child : ( parent : rest ) )  ) = s =~ pattern :: ( String, String, String, [ String ] )
    in
        ( parent, child )
