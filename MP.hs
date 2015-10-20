module MP where

import System.Environment

type FileContents = String

type Keyword      = String
type KeywordValue = String
type KeywordDefs  = [(Keyword, KeywordValue)]

separators :: String
separators
  = " \n\t.,:;!\"\'()<>/\\"


--lookUp :: String -> [(String, a)] -> [a]
--lookUp a [] = []
--lookUp a ((x,y) : xys)
  -- |a == x = y : lookUp a xys
  --  |otherwise = lookUp a xys

lookUp :: String -> [(String, a)] -> [a]
lookUp a v = [y | (x,y) <- v, x == a]

split :: [Char] -> String -> (String, [String])
split x [] = ("", "":[])
split (seps) (y:ys)
    | elem y (seps) =  (y : xs', [] : ys') 
    | otherwise = (xs', (y:y') : yss')
        where 
                (xs', ys') = split (seps) (ys)
                y':yss' = ys'
           			

combine :: String -> [String] -> [String]
combine [] y = y 
combine x [] = x : []
combine (x : xs) (y:ys) = y : [x] : combine (xs) (ys)
    
            

	  
getKeywordDefs :: [String] -> KeywordDefs
getKeywordDefs [] = []
getKeywordDefs ("" : xs) = getKeywordDefs xs
getKeywordDefs (x : xs) =  (y, concat (combine seps ys))  : getKeywordDefs xs
    where 
    (a, (y:ys)) = (split " " x)
    seps = if a == [] then "" else tail a
  --  (y:ys) = if b == [] then ('' : "") else (head b : tail b)
	   
	   
	   
expand :: FileContents -> FileContents -> FileContents
expand [] y = []
expand x [] = x
expand x y = concat (combine seps replaced)
   where
     replaced = map (flip (replaceWord) keywords) splited   
     (seps, splited) = split separators x
     (_, b) = split "\n" y
     keywords = getKeywordDefs b 
                                                                                                                                                                                                                                                                            


replaceWord :: String -> KeywordDefs -> String
replaceWord "" x = ""
replaceWord a@('$' : as) b = head (lookUp a b)
replaceWord a b = a  


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

