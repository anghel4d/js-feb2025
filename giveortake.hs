import qualified System.Console.ANSI as ANSI
import Data.Matrix
import Data.List

-- Original wordlists as read from the problem statement.
wordlist1 :: [String]
wordlist1 =
  [ "ADMIRERS"
  , "ASTIR"
  , "BLACK"
  , "DITHER"
  , "DRINK"
  , "HOAGIES"
  , "JOLTS"
  , "OKRA"
  , "PREMISE"
  , "STICKERS"
  , "SURFACED"
  , "SWARM"
  , "WILTS"
  , "WRAP"
  ]

wordlist2 :: [String]
wordlist2 =
  [ "ARK"
  , "CHILDREN'S"
  , "CUIRASS"
  , "FOR"
  , "HOE"
  , "ISOMER"
  , "LANE"
  , "LORDS"
  , "NOCTURNES"
  , "RIDDLE"
  , "SAT"
  , "SOLE"
  , "TRIONYM"
  , "TROPE"
  ]

-- Word Matrix
wordmatrix :: Matrix [String]
wordmatrix = fromLists
  [ [ a, b ]
  , [ c, d ]
  ]
  where
    (a, b) = splitAt 7 wordlist1
    (c, d) = splitAt 7 wordlist2

-- Define a custom operator for indexing.
infixl 9 !.
(!.) :: Matrix a -> (Int, Int) -> a
m !. (i, j) = getElem i j m

-- The original instructions as read from the problem statement.
data Token = Lit String | Red Int | Blue Int
originalInstructions :: [[Token]]
originalInstructions =
  [ [Red 1, Lit "on the", Red 6]
  , [Blue 2, Blue 4]
  , [Red 5, Red 11, Lit "of the", Blue 11, Blue 12]
  , [Lit "The", Blue 10, Blue 5, Blue 3]
  , [Blue 7, Lit "of the", Blue 13, Blue 6]
  , [Lit "..."]   -- The Pivot point
  , [Lit "and the", Red 3, Blue 1]
  , [Red 4, Red 9]
  , [Red 13, Red 8]
  , [Red 10, Blue 14]
  , [Red 2, Red 12]
  , [Blue 8, Lit "of the", Red 14]
  , [Red 7, Lit "of a", Blue 9]
  ]

-- Print out the wordlists entirely.
printwords :: [String] -> [String] -> IO ()
printwords redlist bluelist = do
  ANSI.setSGR [ANSI.SetColor ANSI.Foreground ANSI.Vivid ANSI.Red]
  mapM_ putStrLn redlist
  ANSI.setSGR [ANSI.Reset]
  ANSI.setSGR [ANSI.SetColor ANSI.Foreground ANSI.Vivid ANSI.Blue]
  mapM_ putStrLn bluelist
  ANSI.setSGR [ANSI.Reset]

-- Prints a single Token as a word from the given lists.
naiveTokenPrint :: [String] -> [String] -> Token -> IO ()
naiveTokenPrint _ _ (Lit s) = putStr (s ++ " ")
naiveTokenPrint redWords _ (Red i) = do
  ANSI.setSGR [ANSI.SetColor ANSI.Foreground ANSI.Vivid ANSI.Red]
  putStr (redWords !! (i - 1) ++ " ")
  ANSI.setSGR [ANSI.Reset]
naiveTokenPrint _ blueWords (Blue i) = do
  ANSI.setSGR [ANSI.SetColor ANSI.Foreground ANSI.Vivid ANSI.Blue]
  putStr (blueWords !! (i - 1) ++ " ")
  ANSI.setSGR [ANSI.Reset]

-- Process each sublist (line) of originalInstructions and print them.
naiveSubstitition :: [String] -> [String] -> IO ()
naiveSubstitition redWords blueWords = mapM_ printLine originalInstructions
  where
    printLine :: [Token] -> IO ()
    printLine tokens = do
      mapM_ (naiveTokenPrint redWords blueWords) tokens
      putStrLn ""


-- ENTRY POINT.
main :: IO ()
main = do
  putStrLn "\nOriginal Lists"
  putStrLn   "--------------"
  printwords wordlist1 wordlist2

  putStrLn "\nWord Matrix"
  putStrLn   "-----------"
  print wordmatrix
  putStr "\n Row 1: "
  print (getRow 1 wordmatrix)
  putStr "\n Row 2: "
  print (getRow 2 wordmatrix)
  putStr "\n Col 1: "
  print (getCol 1 wordmatrix)
  putStr "\n Col 2: "
  print (getCol 2 wordmatrix)
  putStr "\n First Word: "
  print (wordmatrix !. (1, 1) !! 0)
  putStr "\n First Letter: "
  print (wordmatrix !. (1, 1) !! 0 !! 0)
  
  putStrLn "\nNaive Substitition"
  putStrLn   "=================="

  putStrLn   "\nTOP = RED, BOTTOM = BLUE"
  naiveSubstitition wordlist1 wordlist2

  putStrLn   "\nTOP = BLUE, BOTTOM = RED"
  putStrLn   "------------------"
  naiveSubstitition wordlist2 wordlist1

  putStrLn "\nLEFT = RED, RIGHT = BLUE"
  putStrLn "------------------"
  (let redwords  = wordmatrix !. (1, 1) ++ wordmatrix !. (2, 1)
       bluewords = wordmatrix !. (1, 2) ++ wordmatrix !. (2, 2)
    in naiveSubstitition redwords bluewords)

  putStrLn "\nLEFT = BLUE, RIGHT = RED"
  putStrLn "------------------"
  (let redwords  = wordmatrix !. (1, 2) ++ wordmatrix !. (2, 2)
       bluewords = wordmatrix !. (1, 1) ++ wordmatrix !. (2, 1)
   in naiveSubstitition redwords bluewords)