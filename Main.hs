module Main where

import Common
import News
import Subjects

import System.Environment (getArgs)
import Data.List (findIndex)

main :: IO ()
main = do
  args <- getArgs
  let len = length args
  case len of
    0 -> putStrLn "Co najmniej jeden argument jest wymagany"
    _ -> processArgs args

  where
    processArgs :: [String] -> IO ()
    processArgs args = do
      let h = head args
      case h of
        "--news" -> do
          let url = findIndex (== "--url") args
          case url of
            Nothing -> printNews mainURL
            Just n -> do
              if n + 1 == length args
                then putStrLn "Brak wartości dla argumentu --url"
              else
                printNews $ args !! (n + 1)
        "--print-subjects" -> do
          if length args == 1
            then putStrLn "Niepoprawne argumenty dla --print-subjects"
          else
            printSubjectList $ args !! 1
        "--print-specific" -> do
          if length args < 3
            then putStrLn "Niepoprawne argumenty dla --print-specific"
          else
            printSpecific (read (args !! 1) :: Int) (args !! 2)
        "--match-property" -> do
          if length args < 4
            then putStrLn "Niepoprawne argumenty dla --match-property"
          else
            printMatching (TableItem (args !! 1) (args !! 2)) (args !! 3)
        _ -> putStrLn "Niepoprawna opcja!\nDostępne opcje to:\n\
                      \--news\n\
                      \--url [url] - opcjonalne dla --news\n\
                      \--print-subjects [semestr]\n\
                      \--print-specific [id] [semestr]\n\
                      \--match-property [pole] [wartość] [semestr]"

