import Data.Char
import Data.Maybe
import Data.List
import Data.Time
import System.Exit
import System.IO
import System.FilePath
import System.Directory
import System.Process
import Text.Printf

-----------------------------------------------------------------------------
-- Configuration and Customisation
-----------------------------------------------------------------------------

type Amount         = Float
type Balance        = Float
type Title          = String
type Overridden     = Bool
type Billable       = Bool
type CategoryPrefix = String
type Currency       = String
type ItemId         = Int
type Category       = String

main = do
  putStrLn "Trying to convert expenses.csv"
  contents <- readFile "expenses.csv"
  putStrLn $ csv2verify . parseCSV $ contents
  let json = expensify2json . csv2expensify . parseCSV $ contents
  putStrLn $ json
  writeFile "expenses.json" json

-----------------------------------------------------------------------------
-- Expensify Report
-----------------------------------------------------------------------------

csv2verify :: [[String]] -> String
csv2verify items = concatMap (\row -> concat 
									 ["date=",row!!0
									 ,",title=",row!!1
									 ,",currency=",row!!8
									 ,",amount=",row!!9
									 ,",category=",row!!4
									 ,",billable=",row!!7
									 ,"\n"
									 ] 
				        ) . safeTail . ensureLength 8 $ items
 
csv2expensify :: [[String]] -> [(Day,Title,Currency,Amount,Category,Billable)]
csv2expensify =
   map (\row ->
           let date      = read . take 10 $ row!!0
               title     = row!!1
               currency  = row!!8
               amount    = parseFloat $ row!!9
               category  = row!!4
               billable  = "yes" `isInfixOf` (map toLower $ row!!7)
                 in (date,title,currency,amount,category,billable)
         )
    . safeTail . ensureLength 8

expensify2json :: [(Day,Title,Currency,Amount,Category,Billable)]
               -> String
expensify2json items =
  concat ["var expenses = [\n"
         ,concatMap (\((date,title,currency,amount,category,_),l) ->
                    concat ["   { "
                           ,"\"date\":\"",formatDayOracle date,"\", "
                           ,"\"title\":\"",title,"\", "
                           ,"\"currency\":\"",currency,"\", "
                           ,"\"amount\":\"",roundToStr 2 amount,"\", "
                           ,"\"category\":\"",category,"\""
                           ," }"
                           ,if l then "" else ","
                           ,"\n"
                           ]
                    )
                    $ listLast $ reverse items
         ,"]\n"
         ]


------------------------------------------------------------------------------
-- CSV Processing
------------------------------------------------------------------------------

-- Converts a String containing a CSV file into a list of list of strings.
parseCSV ::   String     -- ^ CSV File Contents
           -> [[String]] -- List of lists
parseCSV = map (\line -> splitCSVLine line) . lines

-- Split a CSV Line into a List of Strings taking into account that
-- Strings are separated by double quotes and may contain commas.
-- Example:
--          splitCSVLine "1,\"Food, Hotels and Petrol\",2.34"
--                    == ["1","Foods, Hotels and Petrol","2.34"]
--
splitCSVLine :: String -> [String]
splitCSVLine  [] = []
splitCSVLine row = parser row False "" [] where
  parser :: String   -- ^ CSV Line to parse
         -> Bool     -- True - String mode, ignore comma
         -> String   -- Current field
         -> [String] -- List of fields
         -> [String] -- Final result
  parser []     _     field fields = fields ++ [field]
  parser (x:xs) False field fields
    | x == ','     = parser xs False []            (fields ++ [field])
    | x == '\"'    = parser xs True  field          fields
    | otherwise    = parser xs False (field ++ [x]) fields
  parser (x:xs) True  field fields
    | x == '\"'    = parser xs False field          fields
    | otherwise    = parser xs True  (field ++ [x]) fields

------------------------------------------------------------------------------
-- Date Functions
------------------------------------------------------------------------------

-- Zero Date
dayZero :: Day
dayZero = fromGregorian 0 0 0

-- Converts DDMmmYYYY to Day
-- 14Apr2015
parseDate :: String -> Day
parseDate date = fromGregorian year month day where
                 day = read . take 2 $ date
                 month =   (+1)
                         . fromJust
                         . elemIndex (take 3 . drop 2 $ date)
                         $ ["Jan","Feb","Mar","Apr","May","Jun",
                            "Jul","Aug","Sep","Oct","Nov","Dec"]
                 year =  read . take 4 . drop 5 $ date

-- Converts integer date to String
-- E.g. formatDay 20150509 == 09.05.15
formatDay :: Day -> String
formatDay day =
      padN int_day
   ++ "."
   ++ padN int_month
   ++ "."
   ++ padN (int_year - 2000)
          where (int_year,int_month,int_day) = toGregorian day

formatDayOracle :: Day -> String
formatDayOracle day =
      padN int_day
   ++ "-"
   ++ (case int_month of
             1 -> "Jan"
             2 -> "Feb"
             3 -> "Mar"
             4 -> "Apr"
             5 -> "May"
             6 -> "Jun"
             7 -> "Jul"
             8 -> "Aug"
             9 -> "Sep"
             10 -> "Oct"
             11 -> "Nov"
             12 -> "Dec"
      )
   ++ "-"
   ++ padN (int_year - 2000)
          where (int_year,int_month,int_day) = toGregorian day

-- Given two dates create a range of file prefixes for the matching
-- months. For example:
--
-- filePrefixMonthRange (fromGreogorian 2014 12 00)
--                      (fromGreogorian 2015 02 00)
--                            ==  ["2014-12","2015-01","2015-02"]

filePrefixMonthRange :: Day -> Day -> [String]
filePrefixMonthRange dayFrom dayTo =
    (take 7 . show $ dayFrom)
  : let dayNew = addGregorianMonthsClip 1 dayFrom in
      if dayNew > dayTo then [] else filePrefixMonthRange dayNew dayTo


------------------------------------------------------------------------------
-- Utility, List/String and Number
------------------------------------------------------------------------------

-- | Takes a list and returns a list that tells whether the last element has
-- been reached

listLast :: [a] -> [(a,Bool)]
listLast []     = []
listLast [x]    = (x,True):[]
listLast (x:xs) = (x,False):listLast xs

-- Adds a fixed length to a String by adding white space to the tail
-- Example:
--           padS 5  "Hello World" == "Hello "
--           padS 15 "Hello World" == "Hello World    "
padS :: (Show a) => Int         -- ^ Padding size
                 -> a           -- ^ Object to be padded
                 -> String      -- ^ Padded String
padS size o = take size ((filter (/='\"') $ show o) ++ repeat ' ')

-- Pads with 0 digits equal or below 9.
-- E.g. pad 9  == "09"
-- E.g. pad 11 == "11"
padN :: (Num a, Ord a, Show a) => a -> String
padN n | n <= 9    = '0':(show n)
       | otherwise = show n

-- Converts a String to Float by removing non-number/comma characters from
-- the string.
-- Example:
--    readFloat "Price 123.34 " == 123.34
readFloat :: String -> Float
readFloat = read . filter (`elem` "1234567890.")

-- Same as tail but returns [] when the list is empty
safeTail [] = []
safeTail xs = tail xs

-- Same as init but returns [] when the list is empty
safeInit [] = []
safeInit xs = init xs


-- Splits a String by a given character
-- Example
--          splitBy '|' "1|Food|2.3" == ["1","Food","2.3"]
splitBy :: Char -> String -> [String]
splitBy c [] = []
splitBy c xs = takeWhile (/=c) xs : (splitBy c $ safeTail . dropWhile (/=c) $ xs)

-- Returns the list of data constructors defined for a type
-- E.g. enumeration :: [Bool] == [False,True]
enumeration :: (Bounded a , Enum a) => [a]
enumeration = [ (minBound) .. (maxBound)]

--- Prints a list in a more user-friendly manner
listPrettyPrint :: (Show a) => [a] -> String
listPrettyPrint = foldr (\l r -> (show l) ++ "\n" ++ r) []

-- Time file name
-- Drops the YYYY prefix and file extension suffix from a file
-- E.g. trimFileName "2005-05-BASE_Expenses.cvs" == "BASE_Expenses"
trimFileName :: FilePath -> String
trimFileName = drop 8 . takeBaseName

-- Displays a floating point value correctly
roundToStr :: (PrintfArg a, Floating a) => Int -> a -> String
roundToStr = printf "%0.*f"

-- Parses a float number
parseFloat :: String -> Float
parseFloat = read . filter (\c -> c `elem` "0123456789.")
-- Ensure every line has the required minimum length
ensureLength :: Show a => Int -> [[a]] -> [[a]]
ensureLength n =
  map (\columns -> if (length columns) >= n then
                      columns
                   else
                      error $ concat ["Wrong length\n"
                                     ,"Actual: "
                                     ,show $ length columns
                                     ,"\nExpected: "
                                     ,show n
                                     ," in : "
                                     ,show columns
                                     ]
       )


asciiOnly :: String -> String
asciiOnly = filter (`elem` (   "abcdefghijklmnopqrstuvwxyz"
                            ++ "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
                            ++ "0123456789-+,.'_i$£%&!* ()=@:"))
