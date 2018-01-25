module Main where

import System.Environment
import System.FilePath
import Prelude hiding ((*>),(<*),(<$),($>))

import ParseLib.Abstract.Derived

import CSharpLex
import CSharpGram
import CSharpAlgebra
import CSharpFormat
import SSM
import CSharpCode

start :: Parser s a -> [s] -> a
start p = fst . head . filter (null . snd) . parse p

main :: IO ()
main = do
         -- get command line arguments
         args  <- getArgs
         -- compute a list of input an output files
         files <- case args of
                    []  ->  do
                              putStrLn "no argument given; assuming example.cs"
                              return [("../example.cs", "../example.ssm")]
                    xs  ->  return (map (\ f -> (f, addExtension (dropExtension f) "ssm")) xs)
         -- translate each of the files
         mapM_ processFile files
         mapM_ formatFile (map (\f -> (f,addExtension (dropExtension f ++ "formatted") "cs")) args)

formatFile :: (FilePath, FilePath) -> IO ()
formatFile (infile, outfile) =
  do
    xs <- readFile infile
    writeFile outfile (process xs)
    putStrLn (outfile ++ " written")
  where process = unlines . foldCSharp formatAlgebra . start (pClass <* eof) . start lexicalScanner

-- processFile compiles one file; it take the name of the input
-- file and the name of the output file as arguments
processFile :: (FilePath, FilePath) -> IO ()
processFile (infile, outfile) =
  do
    xs <- readFile infile
    writeFile outfile (process xs)
    putStrLn (outfile ++ " written")
  where process = formatCode
                . foldCSharp codeAlgebra
                . start (pClass <* eof)
                . start lexicalScanner
