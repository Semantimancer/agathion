{-# LANGUAGE OverloadedStrings #-}
module Agathion.Github where

import Brick
import Brick.AttrMap
import Brick.Types
import Data.Time
import Data.List (isInfixOf)
import Data.List.Split (splitOn)

type Repo = (String,[Commit])

data Commit = C { commitID :: String, commitDate :: String, commitText :: String }
  deriving (Read, Show, Eq, Ord)

stringToCommits :: String -> [Commit]
stringToCommits str = [ x | Just x <- map stringToCommit $ splitOn "commit" str]

stringToCommit :: String -> Maybe Commit
stringToCommit "" = Nothing
stringToCommit str' = validateCommit vID (fmap (take 24) vDate) vText
  where str = splitOn "\n" str'
        vID = validateString " " $ str!!0
        vMerge = validateString "Merge: " $ str!!1

        (vAuthor,vDate,vText) = case vMerge of
          Nothing -> (validateString "Author: " $ str!!1,validateString "Date:   " $ str!!2,validateString "    " $ str!!4)
          _       -> (validateString "Author: " $ str!!2,validateString "Date:   " $ str!!3,validateString "    " $ str!!4)
  
        validateString valid input = if valid `isInfixOf` input then Just $ drop (length valid) input else Nothing
        validateCommit (Just i) (Just d) (Just t) = Just $ C i d t
        validateCommit _ _ _ = Nothing


repoTitle :: Repo -> Widget n
repoTitle r = withAttr "red" $ str $ head $ reverse $ splitOn "/" $ fst r

commitData :: Bool -> [Commit] -> Widget n
commitData filterToday [] = str $ if filterToday then "  Nothing today!" else "  Nothing this week!"
commitData filterToday cs = vBox $ map (\c -> hBox [(withAttr "blue" $ str $ commitToStringID c)
                                                   ,(withAttr "green" $ str $ commitToStringDate c)
                                                   ,(str $ commitToStringText c)]) cs

commitToString :: Commit -> String
commitToString c = "\n  "++(take 7 $ commitID c)++" ["++(take 10 $ commitDate c)++"] - "++(commitText c)

commitToStringID :: Commit -> String
commitToStringID c = concat ["  ",take 7 $ commitID c]

commitToStringDate :: Commit -> String
commitToStringDate c = concat [" [",take 10 $ commitDate c,"]"]

commitToStringText :: Commit -> String
commitToStringText c = concat [" - ",commitText c]

commitToday :: UTCTime -> TimeZone -> Commit -> Bool
commitToday time zone c = (f $ take 2 $ drop 8 $ show $ utcToLocalTime zone time)==(f $ take 2 $ drop 8 $ commitDate c)
  where f ('0':x) = filter (`elem` ['0'..'9']) x
        f x = filter (`elem` ['0'..'9']) x
