module Common where

trim :: String -> String
trim = unwords . words

mainURL :: String
mainURL = "https://zapisy.ii.uni.wroc.pl" 

coursesURL :: String
coursesURL = mainURL ++ "/courses"
