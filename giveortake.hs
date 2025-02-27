import qualified System.Console.ANSI as ANSI
import Data.List
import Data.Maybe (mapMaybe)


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

-- Word Grid
wordgrid :: [String] -> [String] -> (([String], [String]), ([String], [String]))
wordgrid row1 row2 =
  ( ( a, b )
  , ( c, d )
  )
  where
    (a, b) = splitAt 7 row1
    (c, d) = splitAt 7 row2

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


-- | Attempt to interpret puzzle instructions as letter-extractions:
--   Here we do something very simplistic:
--   * 'Red i' => take the i-th letter of the i-th RED word
--   * 'Blue i' => take the i-th letter of the i-th BLUE word
--   * 'Lit _' => ignore
--
-- Feel free to adapt to your own logic if you suspect "5 11 of the 11 12"
-- means something more complex (like concatenating words #11 and #12, etc.).
extractMessage :: [String]         -- ^ Red words
               -> [String]         -- ^ Blue words
               -> [[Token]]        -- ^ The puzzle instructions
               -> String           -- ^ The extracted "message"
extractMessage redWords blueWords = 
  mapMaybe (getLetterFromToken redWords blueWords) . concat
  where
    getLetterFromToken :: [String] -> [String] -> Token -> Maybe Char
    getLetterFromToken reds blues (Red i) =
      let wIndex = i - 1
      in if wIndex >= 0 && wIndex < length reds
         then let word = reds !! wIndex
              in if i <= length word
                 then Just (word !! (i - 1))  -- i-th letter
                 else Nothing
         else Nothing
    getLetterFromToken reds blues (Blue i) =
      let wIndex = i - 1
      in if wIndex >= 0 && wIndex < length blues
         then let word = blues !! wIndex
              in if i <= length word
                 then Just (word !! (i - 1))
                 else Nothing
         else Nothing
    getLetterFromToken _    _     (Lit _) = Nothing



-- ENTRY POINT.
main :: IO ()
main = do
  putStrLn "\nOriginal Lists"
  putStrLn   "--------------"
  printwords wordlist1 wordlist2

  putStrLn "\nWord Grid"
  putStrLn   "-----------"
  let ((r1c1, r1c2), (r2c1, r2c2)) = (wordgrid wordlist1 wordlist2)
  let (r1, r2, c1, c2) = (r1c1 ++ r1c2, r2c1 ++ r2c2, r1c1 ++ r2c1, r1c2 ++ r2c2) -- Common Haskell W
  print (wordgrid wordlist1 wordlist2)
  putStr "\n Row 1: "
  print (r1)
  putStr "\n Row 2: "
  print (r2)
  putStr "\n Col 1: "
  print (c1)
  putStr "\n Col 2: "
  print (c2)
  putStr "\n First Word: "
  print (r1 !! 0)
  putStr "\n First Letter: "
  print (r1 !! 0 !! 0)
  

  putStrLn "\nNaive Substitition"
  putStrLn   "=================="

  putStrLn   "\nTOP = RED, BOTTOM = BLUE"
  putStrLn "------------------"
  naiveSubstitition r1 r2

  putStrLn   "\nTOP = BLUE, BOTTOM = RED"
  putStrLn   "------------------"
  naiveSubstitition r2 r1

  putStrLn "\nLEFT = RED, RIGHT = BLUE"
  putStrLn "------------------"
  naiveSubstitition c1 c2

  putStrLn "\nLEFT = BLUE, RIGHT = RED"
  putStrLn "------------------"
  naiveSubstitition c2 c1


  putStrLn "\nE : E Substitition"
  putStrLn   "=================="

  let allWords = wordlist1 ++ wordlist2
  let withE = sort $ filter (elem 'E') allWords
  let withoutE = sort $ filter (not . elem 'E') allWords
  let ((er1c1, er1c2), (er2c1, er2c2)) = (wordgrid withE withoutE)
  let (er1, er2, ec1, ec2) = (er1c1 ++ er1c2, er2c1 ++ er2c2, er1c1 ++ er2c1, er1c2 ++ er2c2) -- Common Haskell W

  print (wordgrid withE withoutE)
  putStr "\n Row 1: "
  print (er1)
  putStr "\n Row 2: "
  print (er2)
  putStr "\n Col 1: "
  print (ec1)
  putStr "\n Col 2: "
  print (ec2)
  putStr "\n First Word: "
  print (er1 !! 0)
  putStr "\n First Letter: "
  print (er1 !! 0 !! 0)

  putStrLn   "\nTOP = RED, BOTTOM = BLUE"
  putStrLn "------------------"
  naiveSubstitition er1 er2

  putStrLn   "\nTOP = BLUE, BOTTOM = RED"
  putStrLn   "------------------"
  naiveSubstitition er2 er1

  putStrLn "\nLEFT = RED, RIGHT = BLUE"
  putStrLn "------------------"
  naiveSubstitition ec1 ec2

  putStrLn "\nLEFT = BLUE, RIGHT = RED"
  putStrLn "------------------"
  naiveSubstitition ec2 ec1


  putStrLn "\n\n"
  putStrLn "\nLetter Extractions"
  putStrLn   "=================="
  -- Startomg with r1 r2
  let topRedBottomBlueMsg = extractMessage r1 r2 originalInstructions
  putStrLn "Hidden message (TOP=RED, BOTTOM=BLUE) =>"
  putStrLn topRedBottomBlueMsg

  -- next r2 r1 
  let topBlueBottomRedMsg = extractMessage r2 r1 originalInstructions
  putStrLn "\nHidden message (TOP=BLUE, BOTTOM=RED) =>"
  putStrLn topBlueBottomRedMsg

  -- next c1 c2
  let leftRedRightBlueMsg = extractMessage c1 c2 originalInstructions
  putStrLn "\nHidden message (LEFT=RED, RIGHT=BLUE) =>"
  putStrLn leftRedRightBlueMsg

  -- next c2 c1
  let leftBlueRightRedMsg = extractMessage c2 c1 originalInstructions
  putStrLn "\nHidden message (LEFT=BLUE, RIGHT=RED) =>"
  putStrLn leftBlueRightRedMsg

  -- next er1 er2
  let eTopRedBottomBlueMsg = extractMessage er1 er2 originalInstructions
  putStrLn "\nHidden message (TOP=RED, BOTTOM=BLUE) =>"
  putStrLn topRedBottomBlueMsg

  -- next er2 er1
  let eTopBlueBottomRedMsg = extractMessage er2 er1 originalInstructions
  putStrLn "\nHidden message (TOP=BLUE, BOTTOM=RED) =>"
  putStrLn topBlueBottomRedMsg
  
  -- next ec1 ec2
  let eLeftRedRightBlueMsg = extractMessage ec1 ec2 originalInstructions
  putStrLn "\nHidden message (LEFT=RED, RIGHT=BLUE) =>"
  putStrLn leftRedRightBlueMsg

  -- next ec2 ec1
  let eLeftBlueRightRedMsg = extractMessage ec2 ec1 originalInstructions
  putStrLn "\nHidden message (LEFT=BLUE, RIGHT=RED) =>"
  putStrLn leftBlueRightRedMsg
  
  -- TODO: Break this up into smaller functions as "Strategies" and test them individually.