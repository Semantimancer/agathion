{-# LANGUAGE OverloadedStrings #-}
module Main where

import Agathion.Github
import Agathion.Twitter
import Agathion.Weather

import Brick
import Brick.AttrMap
import Brick.Types
import Brick.Widgets.Border
import Brick.Widgets.Border.Style
import Brick.Widgets.Center
import Control.Monad.IO.Class
import Data.List.Split
import Data.Time
import Graphics.Vty
import System.Directory
import System.Process (readProcess)

data State = State { weather :: Maybe Weather
                   , uptime :: String
                   , host :: String
                   , kernel :: String
                   , bits :: String
                   , ram :: String
                   , drives :: String
                   , img :: [String]
                   , time :: UTCTime
                   , timeZone :: TimeZone
                   , repos :: [Repo] 
                   , tweets :: [Maybe Tweet]
                   , options :: [Option]
                   } 

data Option = OptionRepos [FilePath]
            | OptionWeather String
            | OptionTwitter String
  deriving (Show)

stringToOption :: String -> [Option]
stringToOption s = case fmap (drop 1) . break (=='=') $ s of
  ("twitter",ts)  -> map OptionTwitter $ splitOn "," ts
  ("github",rs)   -> [OptionRepos $ splitOn "," rs]
  ("location",l)  -> [OptionWeather l]
  _               -> []


data Resource = Left | Right
  deriving (Show, Eq, Ord)

drawUI :: State -> [Widget Resource]
drawUI st = [padTop (Pad 1) $ vBox [(boxDiag <+> boxWeather),(boxTwitter <=> (boxRepoDay <+> boxRepoWeek)) <+> boxImg]]
  where boxDiag = let titleBar = hBox [withAttr "blue" $ str $ host st,str $ " ("++kernel st++", "++bits st++"-bit)"]
                      box = padBottom (Pad 1) . padLeftRight 2 . withBorderStyle ascii . borderWithLabel titleBar . padAll 1
                  in vLimit 15 $ box $ hBox [fill ' ',str $ drives st,fill ' ',str $ ram st,fill ' ']
        boxWeather = let i = textWidth $ weatherStringLine $ weather st
                     in hLimit (i+(i `div` 2)) $ vLimit 15 $ makeBox (Just "Weather") . bottom $ weatherWidget $ weather st
        boxTwitter = makeBox (Just "Twitter") . left $ vBox $ map tweetWidget $ tweets st
        boxRepoDay = let reposToday = map (\(t,cs) -> (t,filter (commitToday (time st) (timeZone st)) cs)) $ repos st
                     in makeBox (Just "Today") . left $ vBox $ map (\r -> (repoTitle r) <=> (commitData True $ snd r)) $ reposToday
        boxRepoWeek = makeBox (Just "This Week") . left $ vBox $ 
                        map (\r -> (repoTitle r) <=> (commitData True $ snd r)) $ repos st
        boxImg = let i = textWidth $ (img st)!!0
                 in hLimit (i+(i `div` 2)) $ makeBox Nothing $ vBox $ [hCenter $ hBox [withAttr "red" $ str "Uptime: ",str $ uptime st]
                                                                      ,fill ' '
                                                                      ]++(map (\x -> hCenter $ withAttr "blue" $ str x) (img st))++[fill ' ']

makeBox :: Maybe String -> Widget n -> Widget n
makeBox (Just x) = padBottom (Pad 1) . padLeftRight 2 . withBorderStyle ascii . borderWithLabel (str x) . padAll 1
makeBox Nothing = padBottom (Pad 1) . padLeftRight 2 . withBorderStyle ascii . border . padAll 1

left :: Widget n -> Widget n
left = flip (<+>) (fill ' ')

right :: Widget n -> Widget n
right = (<+>) (fill ' ')

bottom :: Widget n -> Widget n
bottom = flip (<=>) (fill ' ')

chooseCursor :: State -> [CursorLocation Resource] -> Maybe (CursorLocation Resource)
chooseCursor st _ = Nothing

handleEvent :: State -> BrickEvent Resource e -> EventM Resource (Next State)
handleEvent st (VtyEvent ev) = case ev of
  EvKey KEsc []         -> halt st
  EvKey (KChar 'q') []  -> halt st
  EvKey (KChar 'r') []  -> suspendAndResume $ refreshState st
  _                     -> continue st
handleEvent st _ = continue st

refreshState :: State -> IO State
refreshState s = refreshState' (s { tweets = [] }) (options s)

refreshState' :: State -> [Option] -> IO State
refreshState' s ((OptionRepos fps):os) = do
  newCommits <- mapM (\x -> withCurrentDirectory x $ readProcess "git" ["log","--since=\"7 days ago\""] "") fps
  return =<< refreshState' (s { repos = zip fps (map stringToCommits newCommits) }) os
refreshState' s ((OptionWeather str):os) = do
  newWeather <- getWUnderground str
  return =<< refreshState' (s { weather = newWeather }) os
refreshState' s ((OptionTwitter str):os) = do
  newTweet <- getTweet str
  return =<< refreshState' (s { tweets = newTweet:(tweets s) }) os
refreshState' s [] = do
  newUptime <- readProcess "uptime" ["-p"] ""
  newHostname <- readProcess "hostname" [] ""
  newKernel <- readProcess "uname" ["-r"] ""
  newBits <- readProcess "uname" ["-m"] ""
  newRam <- readProcess "free" ["-mht"] ""
  newDrives <- readProcess "df" ["-h"] ""
  newTime <- getCurrentTime
  newZone <- getTimeZone newTime

  return $ s { uptime = drop 3 newUptime 
             , host = init newHostname
             , kernel = init newKernel
             , bits = init newBits
             , ram = newRam
             , drives = newDrives
             , time = newTime
             , timeZone = newZone
             }

theMap :: AttrMap
theMap = attrMap defAttr [(attrName "red", fg red)
                         ,(attrName "blue", fg blue)
                         ,(attrName "green", fg green)
                         ]

main :: IO ()
main = do
  dataPath <- getAppUserDataDirectory "agathion"

  configExists <- doesFileExist $ dataPath++"/config"
  opts <- if configExists
          then do
            f <- readFile $ dataPath++"/config"
            return $ concatMap stringToOption $ splitOn "\n" f
          else return defaultOptions

  imgExists <- doesFileExist $ dataPath++"/img"
  imgF <- if imgExists
          then do
            f <- readFile $ dataPath++"/img"
            return $ splitOn "\n" f
          else return [""]

  initialState <- refreshState $ State Nothing "" "" "" "" "" "" imgF (error "No time found!") (error "No time zone found!") [] [] opts

  let app = App { appDraw = drawUI
                , appChooseCursor = chooseCursor
                , appHandleEvent = handleEvent
                , appStartEvent = return
                , appAttrMap = const theMap
                }
  finalState <- defaultMain app initialState

  return ()
  where defaultOptions = [OptionTwitter "inspire_us"]
