{-# LANGUAGE OverloadedStrings #-}
module Agathion.Twitter where

import Brick
import Brick.AttrMap
import Brick.Types
import Control.Applicative ((<|>))
import Data.String.Utils (replace)
import Text.HTML.Scalpel

data Tweet = T { tweet :: String, author :: String }
           | Discard
  deriving (Read, Show, Eq, Ord)

tweetToString :: Maybe Tweet -> String
tweetToString Nothing = ""
tweetToString (Just t) = concat [author t,": ",tweet t,"\n\n"]

tweetWidget :: [Tweet] -> Widget n
tweetWidget [] = withAttr "red" $ str "No tweets found!"
tweetWidget xs = vBox $ concatMap (\t -> [hBox [withAttr "red" $ str $ author t,str $ " - ",withAttr "blue" $ str $ (tweet t)],str " "]) xs

getTweets :: [String] -> IO [Tweet]
getTweets xs = do
  ts <- mapM getTweet xs
  return $ concatMap (\x -> if x==Nothing then [] else let (Just x') = x in [x']) ts

getTweet :: String -> IO (Maybe Tweet)
getTweet str = do
  t<- scrapeURL ("https://twitter.com/"++str) twitter
  return $ discardFromMaybe t
  where discardFromMaybe Nothing = Nothing
        discardFromMaybe (Just x) = discardTweets x

        discardTweets [] = Nothing
        discardTweets (Discard:ts) = discardTweets ts
        discardTweets (t:_) = Just t

twitter :: Scraper String [Tweet]
twitter = chroots ((TagString "div") @: [hasClass "tweet"]) filterPins

filterPins :: Scraper String Tweet
filterPins = pinned <|> good

pinned :: Scraper String Tweet
pinned = do
  context <- innerHTML $ (TagString "div") @: [hasClass "tweet"] // 
                         (TagString "div") @: [hasClass "context"] //
                         (tagSelector "div")
  return Discard

good :: Scraper String Tweet
good = do
  user <- text $ (TagString "div") @: [hasClass "stream-item-header"] // 
                 (tagSelector "a") // 
                 (tagSelector "span") // 
                 (tagSelector "b")
  txt <- text $ (TagString "p") @: [hasClass "tweet-text"]
  return $ T (replace "\n" "\\\\" txt) user
