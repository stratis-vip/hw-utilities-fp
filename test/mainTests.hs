module Main (main) where

import Helpers
  ( fromIntDate,
    isNonEmpty,
    remove2ndElement,
    removeQuotes,
    rmdups,
    safeReadInt,
    splitString,
    trim,
  )
import Test.Tasty
import Test.Tasty.HUnit

allTests :: TestTree
allTests =
  testGroup
    "All Tests"
    [ testGroup
        "remove2ndElement"
        [ testCase "Removes second element" $
            remove2ndElement ([1, 2, 3, 4] :: [Int]) @?= Just [1, 3, 4],
          testCase "Handles empty list" $
            remove2ndElement ([] :: [Int]) @?= Nothing,
          testCase "Handles single-element list" $
            remove2ndElement ([42] :: [Int]) @?= Nothing,
          testCase "Handles two-element list" $
            remove2ndElement ([5, 9] :: [Int]) @?= Just [5], -- <- Check logic here?
          testCase "Handles repeated elements" $
            remove2ndElement ([7, 7, 7, 7] :: [Int]) @?= Just [7, 7, 7]
        ],
      testGroup
        "removeQuotes"
        [ testCase "Removes surrounding quotes" $
            removeQuotes "\"Hello\"" @?= "Hello",
          testCase "Handles empty string" $
            removeQuotes "" @?= "",
          testCase "Handles no quotes" $
            removeQuotes "Haskell" @?= "Haskell",
          testCase "Handles multiple quotes" $
            removeQuotes "\"\"Hello\"\"" @?= "Hello",
          testCase "Handles only quotes" $
            removeQuotes "\"\"" @?= "",
          testCase "Handles spaces inside quotes" $
            removeQuotes "\" Hello \"" @?= " Hello "
        ],
      testGroup
        "splitString"
        [ testCase "Splits on commas" $
            splitString ',' "a,b,c" @?= ["a", "b", "c"],
          testCase "Trims surrounding spaces" $
            splitString ',' " a , b , c " @?= ["a", "b", "c"],
          testCase "Handles empty elements" $
            splitString ',' "a,,c" @?= ["a", "", "c"],
          testCase "Handles leading/trailing delimiters" $
            splitString ',' ",a,b," @?= ["", "a", "b", ""],
          testCase "Handles no delimiters" $
            splitString ',' "hello" @?= ["hello"],
          testCase "Handles only delimiters" $
            splitString ',' ",,," @?= ["", "", "", ""],
          testCase "Handles empty string" $
            splitString ',' "" @?= [""]
        ],
      testGroup
        "trim"
        [ testCase "Trims leading spaces" $
            trim "   hello" @?= "hello",
          testCase "Trims trailing spaces" $
            trim "hello   " @?= "hello",
          testCase "Trims both sides" $
            trim "   hello   " @?= "hello",
          testCase "Trims tabs and newlines" $
            trim "\n\t hello \t\n" @?= "hello",
          testCase "Leaves internal spaces alone" $
            trim "   hello   world   " @?= "hello   world",
          testCase "Empty string remains empty" $
            trim "" @?= "",
          testCase "Whitespace only becomes empty" $
            trim "   \n\t  " @?= ""
        ],
      testGroup
        "fromIntDate"
        [ testCase "Converts date to DD-MM-YYYY format" $
            fromIntDate 20250416 @?= "16-Apr-2025",
          testCase "Handles single-digit day and month" $
            fromIntDate 20250101 @?= "01-Jan-2025",
          testCase "Handles non-single-digit month" $
            fromIntDate 20251225 @?= "25-Dec-2025",
          testCase "Handles month as 2-digit code" $
            fromIntDate 20220605 @?= "05-Jun-2022",
          testCase "Handles February 29th in leap year" $
            fromIntDate 20200229 @?= "29-Feb-2020",
          testCase "Handles January 1st" $
            fromIntDate 20230101 @?= "01-Jan-2023",
          testCase "Handles December 31st" $
            fromIntDate 20231231 @?= "31-Dec-2023"
        ],
      testGroup
        "rmdups"
        [ testCase "Empty list" $
            rmdups ([] :: [Int]) @?= [],
          testCase "No duplicates" $
            rmdups [1, 2, 3, 4] @?= [1, 2, 3, 4],
          testCase "All duplicates" $
            rmdups [5, 5, 5, 5] @?= [5],
          testCase "Consecutive duplicates" $
            rmdups [1, 1, 2, 2, 3, 3] @?= [1, 2, 3],
          testCase "Interspersed duplicates" $
            rmdups [1, 2, 1, 3, 2, 4] @?= [1, 2, 3, 4],
          testCase "Strings: preserves first occurrences" $
            rmdups ["a", "b", "a", "c", "b"] @?= ["a", "b", "c"],
          testCase "Mixed values" $
            rmdups [1, 2, 2, 3, 1, 4, 3] @?= [1, 2, 3, 4]
        ],
      testGroup
        "safeReadInt"
        [ testCase "Parses positive integer" $
            safeReadInt "123" @?= Just 123,
          testCase "Parses negative integer" $
            safeReadInt "-42" @?= Just (-42),
          testCase "Fails on empty string" $
            safeReadInt "" @?= Nothing,
          testCase "Fails on non-numeric" $
            safeReadInt "abc" @?= Nothing,
          testCase "Fails on partially numeric" $
            safeReadInt "12abc" @?= Nothing,
          testCase "Fails on decimal" $
            safeReadInt "3.14" @?= Nothing
        ],
      testGroup
        "isNonEmpty"
        [ testCase "Empty list → False" $
            isNonEmpty ([] :: [Int]) @?= False,
          testCase "Non-empty list → True" $
            isNonEmpty [1] @?= True,
          testCase "String non-empty" $
            isNonEmpty "hello" @?= True,
          testCase "List of lists" $
            isNonEmpty ([[]] :: [[Int]]) @?= True,
          testCase "After filtering may become empty" $
            let xs = filter (> 5) [1, 2, 3] in isNonEmpty xs @?= False
        ]
    ]

-- Run all tests
main :: IO ()
main = defaultMain allTests