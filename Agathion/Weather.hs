{-# LANGUAGE OverloadedStrings #-}
module Agathion.Weather where

import Brick
import Brick.AttrMap
import Brick.Types
import Data.Char (toLower)
import Text.HTML.Scalpel

data Weather = W { high :: String, low :: String, current :: String, conditions :: String }
  deriving (Show, Read, Eq, Ord)

weatherStringLine :: Maybe Weather -> String
weatherStringLine Nothing = "Weather data not found!"
weatherStringLine (Just w) = concat ["It is ",current w,"\176 and ",map toLower $ conditions w," right now."]

weatherWidget :: Maybe Weather -> Widget n
weatherWidget Nothing = withAttr "red" $ str "Weather data not found!"
weatherWidget w'@(Just w) = vBox [str $ weatherStringLine w'
                                 ,str "\n"
                                 ,hBox [withAttr "red" $ str "High: ",str $ high w]
                                 ,hBox [withAttr "red" $ str "Low: ",str $ low w]]

getWUnderground :: String -> IO (Maybe Weather)
getWUnderground str = scrapeURL ("https://www.wunderground.com/weather/"++str) wunderground

wunderground :: Scraper String Weather
wunderground = do
  high <- text $ (TagString "span") @: [hasClass "hi"]
  low <- text $ (TagString "span") @: [hasClass "lo"]
  current <- text $ (TagString "span") @: [hasClass "wu-value", hasClass "wu-value-to"]
  conditions <- text $ ((TagString "div") @: [hasClass "condition-icon"]) // (tagSelector "p")
  return $ W high low current conditions
