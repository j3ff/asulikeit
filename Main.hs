module Main where

import System.Environment ( getArgs )
import System.Directory ( doesFileExist )
import Control.Monad ( mapM_, filterM )
import System.IO

import AS3Parser

main :: IO ()
main =
    do { args <- getArgs
       ; let as3_filename = head args
       ; hndl <- openFile as3_filename ReadMode
       ; content <- hGetContents hndl
       ; printAST ( parseAS3 content )
       ; hClose hndl }
