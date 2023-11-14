{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Subjects where

import Common

import Text.HTML.Scalpel

import Data.Maybe (fromJust, fromMaybe)
import Data.List (findIndex, find, findIndices)
import Data.List.Split (splitOn)
import Data.Aeson (FromJSON, ToJSON, decode)

import GHC.Generics (Generic)
import qualified Data.ByteString.Lazy.Char8 as C

type Title = String

data SubjectInfo = SubjectInfo {
  id :: Int,
  name :: String,
  courseType :: Int,
  recommendedForFirstYear :: Bool,
  owner :: Int,
  effects :: [Int],
  tags :: [Int],
  url :: String
  }
  deriving (Generic, Show)

instance FromJSON SubjectInfo
instance ToJSON SubjectInfo

data Subject
  = SemesterItem Title URL
  | TableItem String String
  deriving (Show, Eq)

getSemesterURL :: Subject -> URL
getSemesterURL (SemesterItem title url) = url

getSemesterTitle :: Subject -> Title
getSemesterTitle (SemesterItem title url) = title

getSemesterData :: IO ([Subject])
getSemesterData = fmap (\n -> head . tail $
                         fromMaybe
                         [[SemesterItem "2022/23 zimowy" "/courses/semester/346"]]
                         n) getSemesterData'
  where
    getSemesterData' :: IO (Maybe [[Subject]])
    getSemesterData' = scrapeURL coursesURL getElem

    getElem :: Scraper String [[Subject]]
    getElem = chroots ("div" @: [hasClass "dropdown-menu"]) getElemAux

    getElemAux :: Scraper String [Subject]
    getElemAux = do
      url <-  attrs "href" $ "a" @: [hasClass "semester-link"]
      title <- texts $ "a" @: [hasClass "semester-link"]
      return $ map (\(a, b) -> SemesterItem a b) (zip title url)

extractTable :: String -> IO ([Subject])
extractTable url = fmap fromJust (extractTable' url)
  where
    extractTable' :: String -> IO (Maybe [Subject])
    extractTable' url = scrapeURL url getElem

    getElem :: Scraper String [Subject]
    getElem = chroot ("table" @: [hasClass "table-md-responsive"]) getElemAux

    getElemAux :: Scraper String [Subject]
    getElemAux  = do
      thElems <- texts $ "th"
      tdElems <- texts $ "td"
      return $ map (\(a, b) -> TableItem (cleanUP a) (cleanUP b)) (zip thElems tdElems)

    cleanUP :: String -> String
    cleanUP str = trim $ snd (splitAt
                               (fromMaybe (-1) (findIndex (=='}') str) + 1)
                               str)

getCoursesData :: URL -> IO (Maybe String)
getCoursesData url = scrapeURL url getElem
  where
    getElem :: Scraper String String
    getElem = chroot ("script" @: ["id" @= "courses-data"]) getElemAux

    getElemAux :: Scraper String String
    getElemAux = do
      all <- text anySelector
      return $ tail (fst (splitAt ((length all) - 2) all))

printSpecific :: Int -> Title -> IO ()
printSpecific x semester = do
  semList <- getSemesterData
  forSemURL <- findSemester semester
  case forSemURL of
    Nothing -> do
      incorrectSemErr semList
    Just _ -> do
      subList <- extractSubjectInfo url (getSemesterURL $ fromJust forSemURL)
      if x <= 0 || x > length subList
        then putStrLn $ "Podane id przedmiotu jest poza zakresem!\n\
                        \Poprawny zakres to 1 do " ++ show (length subList)
                        ++ " włącznie."
      else
         printall $ mainURL ++ (subList !! (x - 1))
  where
    printall :: URL -> IO ()
    printall url = do
      extractTable url >>= mapM_ (\(TableItem n m) ->
                                      putStrLn $ (trim n) ++ ": " ++ (trim m))
      extractGroups url >>= mapM_ putStr

extractSubjectInfo :: (SubjectInfo -> String) ->  URL -> IO [String]
extractSubjectInfo func link = fmap (\n -> map func (fmap fromJust n)) (printJSON link)
  where
    printJSON :: URL -> IO ([Maybe SubjectInfo])
    printJSON link =
      fmap (\tmp -> fmap (\n ->
                            decode (C.pack $ n ++ "}") :: Maybe SubjectInfo) tmp)
      (fmap (splitOn "},") $ fmap fromJust $ getCoursesData (mainURL ++ link))
    
findSemester :: Title -> IO (Maybe Subject)
findSemester semester = do
  semList <- getSemesterData
  return $ find (\(SemesterItem a b) -> a == semester) semList

incorrectSemErr :: Foldable t => t Subject -> IO ()
incorrectSemErr semList = do
  putStrLn "Podany semestr nie jest poprawny!\nDostępne semestry:"
  mapM_ (putStrLn . getSemesterTitle) semList

printSubjectList :: String -> IO ()
printSubjectList semester = do
  semList <- getSemesterData
  forSemURL <- findSemester semester
  case forSemURL of
    Nothing -> incorrectSemErr semList
    Just _ -> do
      res <- (extractSubjectInfo name) $ getSemesterURL (fromJust forSemURL)
      let toPrint = zip [1..(length res)] res
      mapM_ (\(a, b) -> printList a b) toPrint
  where
    printList :: Int -> String -> IO ()
    printList x y = do
      putStrLn ((show x) ++ ": " ++ y)

printMatching :: Subject -> Title -> IO ()
printMatching t sem = (getSubjectProp >>= (\n -> return $ findIndices (isInList t) n))
            >>= mapM_ (\n -> printSubjectName n sem)
  where
    getSubjectProp :: IO [[Subject]]
    getSubjectProp = do
      semList <- getSemesterData
      forSemURL <- findSemester sem
      res <- extractSubjectInfo url (getSemesterURL (fromJust forSemURL))
      sequence $ map (\x -> extractTable (mainURL ++ x)) res 

    isInList :: Subject -> [Subject] -> Bool
    isInList t l = case (find (== t) l) of
      Just _ -> True
      Nothing -> False

    printSubjectName :: Int -> Title -> IO ()
    printSubjectName i semester = do
      semList <- getSemesterData
      forSemURL <- findSemester semester
      case forSemURL of
        Nothing -> incorrectSemErr semList
        Just _ -> do
          res <- (extractSubjectInfo name) $ getSemesterURL (fromJust forSemURL)
          putStrLn $ res !! i

extractGroups :: String -> IO ([String])
extractGroups url = (fmap fromJust (extractTable' url)) >>= return . tail .
                    init . map unlines
  where
    extractTable' :: String -> IO (Maybe [[String]])
    extractTable' url = scrapeURL url getElem

    getElem :: Scraper String [[String]]
    getElem = chroots ("table" ) getElemAux

    getElemAux :: Scraper String [String]
    getElemAux  = do
      tdElems <- texts $ "td"
      return $ map trim tdElems
