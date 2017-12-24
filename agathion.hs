{-# LANGUAGE OverloadedStrings #-}
module Main where

import Agathion.Github
import Agathion.Twitter
import Agathion.Weather

import Brick
import Brick.AttrMap
import Brick.BChan
import Brick.Types
import Brick.Widgets.Border
import Brick.Widgets.Border.Style
import Brick.Widgets.Center
import Control.Concurrent
import Control.Monad (forever)
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
                   , tweets :: [Tweet]
                   , options :: [Option]
                   } 
  deriving (Show)

data Option = OptionRepos [FilePath]
            | OptionWeather String
            | OptionTwitter [String]
  deriving (Show)

stringToOption :: String -> [Option]
stringToOption s = case fmap (drop 1) . break (=='=') $ s of
  ("twitter",ts)  -> [OptionTwitter $ splitOn "," ts]
  ("github",rs)   -> [OptionRepos $ splitOn "," rs]
  ("location",l)  -> [OptionWeather l]
  _               -> []

--This is used to mark time passing
data Step = Step
  deriving (Ord, Eq)

drawUI :: State -> [Widget Step]
drawUI st = [padTop (Pad 1) $ vBox [(boxDiag <+> boxWeather),(boxTwitter <=> (boxRepoDay <+> boxRepoWeek)) <+> boxImg]]
  where boxDiag = let titleBar = hBox [withAttr "blue" $ str $ host st,str $ " ("++kernel st++", "++bits st++"-bit)"]
                      box = padBottom (Pad 1) . padLeftRight 2 . withBorderStyle ascii . borderWithLabel titleBar . padAll 1
                  in vLimit 15 $ box $ hBox [fill ' ',str $ drives st,fill ' ',str $ ram st,fill ' ']
        boxWeather = let i = textWidth $ weatherStringLine $ weather st
                     in hLimit (i+(i `div` 2)) $ vLimit 15 $ makeBox (Just "Weather") . bottom $ weatherWidget $ weather st
        boxTwitter = makeBox (Just "Twitter") . left $ tweetWidget $ tweets st
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

bottom :: Widget n -> Widget n
bottom = flip (<=>) (fill ' ')

chooseCursor :: State -> [CursorLocation Step] -> Maybe (CursorLocation Step)
chooseCursor st _ = Nothing

handleEvent :: State -> BrickEvent Step e -> EventM Step (Next State)
handleEvent st (VtyEvent ev) = case ev of
  EvKey KEsc []         -> halt st
  EvKey (KChar 'q') []  -> halt st
  EvKey (KChar 'R') []  -> suspendAndResume $ refreshState st
  EvKey (KChar 'r') []  -> suspendAndResume $ updateState st
  EvKey (KChar 'u') []  -> suspendAndResume $ updateState st
  _                     -> suspendAndResume $ updateState st
handleEvent st _ = continue st

refreshState :: State -> IO State
refreshState s = do
  dataPath <- getAppUserDataDirectory "agathion"
  forkIO $ mapM_ (refreshOption dataPath) $ options s
  return s

refreshOption :: FilePath -> Option -> IO ()
refreshOption dataPath (OptionRepos fps) = do
  newCommits <- mapM (\x -> withCurrentDirectory x $ readProcess "git" ["log","--since=\"7 days ago\""] "") fps
  writeFile (dataPath++"/.github") $ show $ zip fps (map stringToCommits newCommits)
refreshOption dataPath (OptionWeather w) = writeFile (dataPath++"/.weather") =<< showIO =<< getWUnderground w
refreshOption dataPath (OptionTwitter t) = writeFile (dataPath++"/.twitter") =<< showIO =<< getTweets t

showIO :: Show a => a -> IO String
showIO a = return (show a)

updateState :: State -> IO State
updateState s = do
  dataPath <- getAppUserDataDirectory "agathion"

  reposExist <- doesFileExist $ dataPath++"/.github"
  newRepos <- if reposExist
              then do
                f <- readFile $ dataPath++"/.github"
                return (read f :: [Repo])
              else return []

  tweetsExist <- doesFileExist $ dataPath++"/.twitter"
  newTweets <- if tweetsExist
               then do
                 f <- readFile $ dataPath++"/.twitter"
                 return (read f :: [Tweet])
               else return []

  weatherExists <- doesFileExist $ dataPath++"/.weather"
  newWeather <- if weatherExists
                then do
                  f <- readFile $ dataPath++"/.weather"
                  return (read f :: Maybe Weather)
                else return Nothing

  newUptime <- readProcess "uptime" ["-p"] ""
  newHostname <- readProcess "hostname" [] ""
  newKernel <- readProcess "uname" ["-r"] ""
  newBits <- readProcess "uname" ["-m"] ""
  newRam <- readProcess "free" ["-mht"] ""
  newDrives <- readProcess "df" ["-h"] ""
  newTime <- getCurrentTime
  newZone <- getTimeZone newTime

  return $ s { weather = newWeather
             , uptime = drop 3 newUptime
             , host = init newHostname
             , kernel = init newKernel
             , bits = init newBits
             , ram = newRam
             , drives = newDrives
             , time = newTime
             , timeZone = newZone
             , repos = newRepos
             , tweets = newTweets
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
          else return [OptionTwitter ["inspire_us"]]
  newTime <- getCurrentTime
  newZone <- getTimeZone newTime

  imgExists <- doesFileExist $ dataPath++"/img"
  imgF <- if imgExists
          then do
            f <- readFile $ dataPath++"/img"
            return $ splitOn "\n" f
          else return [""]

  mapM_ (refreshOption dataPath) opts
  initialState <- updateState $ State Nothing "" "" "" "" "" "" imgF newTime newZone [] [] opts

  let app = App { appDraw = drawUI
                , appChooseCursor = chooseCursor
                , appHandleEvent = handleEvent
                , appStartEvent = return
                , appAttrMap = const theMap
                }
  chan <- newBChan 1
  forkIO $ forever $ do
    writeBChan chan Step
    threadDelay 60000000 --One minute
  finalState <- customMain (mkVty defaultConfig) (Just chan) app initialState

  mapM_ (\x -> do
    exists <- doesFileExist $ dataPath++"/"++x
    if exists then removeFile $ dataPath++"/"++x else return ()
    ) [".weather",".twitter",".github"]
