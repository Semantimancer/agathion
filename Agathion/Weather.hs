{-# LANGUAGE OverloadedStrings #-}
module Agathion.Weather where

import Brick
import Brick.AttrMap
import Brick.Types
import Text.HTML.Scalpel

data Weather = W { high :: String, low :: String, current :: String, conditions :: String }
  deriving (Show, Read, Eq, Ord)

weatherWidget :: Maybe Weather -> Widget n
weatherWidget Nothing = withAttr "red" $ str "Weather data not found!"
weatherWidget w'@(Just w) = vBox $ map makeLine [("Current: ",(current w)++"\176")
                                                ,("Conditions: ",conditions w)
                                                ,(" "," ")
                                                ,("Today's High: ",high w)
                                                ,("Today's Low: ",low w)]
  where makeLine (x,y) = hBox [withAttr "red" $ str x,str y]

getWUnderground :: String -> IO (Maybe Weather)
getWUnderground str = scrapeURL ("https://www.wunderground.com/weather/"++str) wunderground

wunderground :: Scraper String Weather
wunderground = do
  high <- text $ (TagString "span") @: [hasClass "hi"]
  low <- text $ (TagString "span") @: [hasClass "lo"]
  current <- text $ (TagString "span") @: [hasClass "wu-value", hasClass "wu-value-to"]
  conditions <- text $ ((TagString "div") @: [hasClass "condition-icon"]) // (tagSelector "p")
  return $ W high low current conditions
