module MP where

import System.Environment

type FileContents = String

type Keyword      = String
type KeywordValue = String
type KeywordDefs  = [(Keyword, KeywordValue)]

separators :: String
separators
  = " \n\t.,:;!\"\'()<>/\\"


lookUp :: String -> [(String, a)] -> [a]
lookUp a [] = []
lookUp a ((x,y) : xys) 
     |a == x = y : lookUp a xys
     |otherwise = lookUp a xys

toString :: Char -> String
toString c = [c]
	 
	 
split :: [Char] -> String -> (String, [String])
split x [] = ("", [])
split (seps) (y:ys)
    | elem y (seps) =  (y : xs', ys') 
	| otherwise = (xs', ((toString y ++ ys'))
       where 
        (xs', ys') = split (seps) (ys)	   
 

combine :: String -> [String] -> [String]
combine = error "TODO: implement combine"

getKeywordDefs :: [String] -> KeywordDefs
getKeywordDefs = error "TODO: implement getKeywordDefs"

expand :: FileContents -> FileContents -> FileContents
expand = error "TODO: implement expand"

-- You may wish to uncomment and implement this helper function
-- when implementing expand
-- replaceWord :: String -> KeywordDefs -> String



main :: IO ()
-- The provided main program which uses your functions to merge a
-- template and source file.
main = do
  args <- getArgs
  main' args

  where
    main' :: [String] -> IO ()
    main' [template, source, output] = do
      t <- readFile template
      i <- readFile source
      writeFile output (expand t i)
    main' _ = putStrLn ("Usage: runghc MP <template> <info> <output>")

