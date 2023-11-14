{-# LANGUAGE OverloadedStrings #-}
module News where

import Common

import Text.HTML.Scalpel
import Data.Maybe (fromMaybe, fromJust)
import Data.List (findIndex, isPrefixOf, tails)
import Data.Time (UniversalTime, parseTimeOrError, defaultTimeLocale)

type Description = String
type Author = String
type Title = String
type Date = UniversalTime
  
data News
  = NewsItem Date Author Title Description
  deriving (Show, Eq)
  
getNews :: URL -> IO (Maybe [News])
getNews url = scrapeURL url getElem
  where
    getElem :: Scraper String [News]
    getElem = chroots ("div" @: [hasClass "od-news-item"]) getElemAux

    getElemAux :: Scraper String News
    getElemAux = do
      date <- text $ "span" @: [hasClass "od-news-date"]
      h3 <- text $ "h3" @: [hasClass "display-3"]
      author <- text $ "span" @: [hasClass "od-news-author"]
      desc <- text $ "div" @: [hasClass "my-3"]
      return $ NewsItem (dateFromString $ cleanupDate date) author h3 (trim desc)

    dateFromString :: String -> UniversalTime
    dateFromString xs = parseTimeOrError True defaultTimeLocale
                      "%d.%m.%Y %k:%M" xs :: UniversalTime

    cleanupDate :: String -> String
    cleanupDate xs = trim $ filter (not . (`elem` ("\183" :: String)))
               (snd $ splitAt (fromMaybe (-1) (findIndex
                                               (isPrefixOf ",")
                                               (tails xs)) + 1) xs)

printNews :: URL -> IO ()
printNews url = do
  newsList <- fmap fromJust $ getNews url
  mapM_ printResult newsList
  where
    printResult :: News -> IO ()
    printResult (NewsItem date author title desc) =
      putStr $ "\n" ++ author ++ ", " ++ (show date)
        ++ "\n" ++ "Tytuł: " ++ title ++ "\n"
