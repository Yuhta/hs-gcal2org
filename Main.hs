{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Applicative
import Control.Monad (liftM, mzero)
import Data.Aeson
import Data.Aeson.Types (parseMaybe)
import Data.HashMap.Strict (toList)
import Data.List (intercalate)
import Data.Time (LocalTime)
import Data.Time.Calendar (addDays, Day)
import Data.Time.Clock (getCurrentTime, addUTCTime, UTCTime)
import Data.Time.Format (readTime, formatTime, FormatTime)
import Data.Time.LocalTime (localDay)
import Network.Google.OAuth2
import Network.HTTP.Base (urlEncode)
import Network.HTTP.Conduit (simpleHttp)
import System.Directory (doesFileExist)
import System.Environment (getArgs)
import System.Locale (defaultTimeLocale)
import System.Posix.Env (getEnv)
import System.Posix.Files
import System.Process (rawSystem)
import qualified Data.ByteString.Lazy.Char8 as BS
import qualified Data.Text as T

data Event = Event { summary     :: String
                   , description :: Maybe String
                   , start       :: DateTime
                   , end         :: DateTime
                   } deriving Show

instance FromJSON Event where
  parseJSON (Object v) = Event <$>
                         v .:  "summary"     <*>
                         v .:? "description" <*>
                         v .:  "start"       <*>
                         v .:  "end"

data DateTime = Date Day | DateTime LocalTime

instance Show DateTime where
  show (Date d)      = formatLocalTime "%F %a"    d
  show (DateTime dt) = formatLocalTime "%F %a %R" dt

instance FromJSON DateTime where
  parseJSON (Object hm) = return $ case head . toList $ hm of
    ("date",     String d)  -> Date     $ textToTime "%F"      d
    ("dateTime", String dt) -> DateTime $ textToTime "%FT%T%z" dt
    where textToTime fmt t = readTime defaultTimeLocale fmt $ T.unpack t

data Calendar = Calendar { calendarId      :: T.Text
                         , calendarSummary :: T.Text
                         } deriving Show

instance FromJSON Calendar where
  parseJSON (Object o) = Calendar <$>
                         o .: "id" <*>
                         o .: "summary"

oAuthClient = OAuth2Client { clientId = "200701486136-td9daeuse8917ai8v4ojgpkf8ofblvv6.apps.googleusercontent.com"
                           , clientSecret = "n45wv56dJvFOqW6eaDz9Td7g"
                           }
permissionUrl = formUrl oAuthClient
                        ["https://www.googleapis.com/auth/calendar.readonly"]

oAuthTokenFile :: IO String
oAuthTokenFile = do
  maybeFile <- getEnv "HS_GCAL2ORG_OAUTH_TOKEN_FILE"
  case maybeFile of
    Just file -> return file
    Nothing   -> liftM defaultFile $ getEnv "HOME"
  where defaultFile (Just home) = home ++ "/.hs-gcal2org-oauth-token"
        defaultFile Nothing     = error "cannot find user home"

oAuthToken :: IO String
oAuthToken = do
  file <- oAuthTokenFile
  tokenExists <- doesFileExist file
  tokens <- if tokenExists
               then do oldTokensStr <- readFile file
                       refreshTokens oAuthClient $ read oldTokensStr
               else do putStrLn $ "Load URL: " ++ show permissionUrl
                       rawSystem "xdg-open" [permissionUrl]
                       putStrLn "Please paste the verification code:"
                       getLine >>= exchangeCode oAuthClient
  writeFile file $ show tokens
  setFileMode file $ unionFileModes ownerReadMode ownerWriteMode
  return $ accessToken tokens

formatLocalTime :: FormatTime t => String -> t -> String
formatLocalTime = formatTime defaultTimeLocale

timeAround :: UTCTime -> (String, String)
timeAround t = let dt = 60 * 60 * 24 * 7
                   add d = formatLocalTime "%FT%T%z" $ addUTCTime d t
               in (add (negate dt), add dt)

listEventsUri :: String -> String -> String -> String -> String
listEventsUri accessToken timeMin timeMax calendarId =
  "https://www.googleapis.com/calendar/v3/calendars/" ++
  urlEncode calendarId ++ "/events?" ++
  parameters [ ("singleEvents", "true")
             , ("timeMin",      timeMin)
             , ("timeMax",      timeMax)
             , ("access_token", accessToken)
             ]
  where parameters xs = intercalate "&" $ map pairToParam xs
        pairToParam (k, v) = k ++ "=" ++ urlEncode v

orgTimestamp :: DateTime -> DateTime -> String
orgTimestamp s@(Date s1) e@(Date e1)
  | addDays 1 s1 == e1 = "<" ++ show s ++ ">"
  | otherwise          = "<" ++ show s ++ ">--<" ++ show e ++ ">"
orgTimestamp s@(DateTime s1) e@(DateTime e1)
  | localDay s1 == localDay e1 = "<" ++ show s ++ "-" ++
                                 formatLocalTime "%R" e1 ++ ">"
  | otherwise                  = "<" ++ show s ++ ">--<" ++ show e ++ ">"

eventToOrg :: Event -> String
eventToOrg (Event s d st ed) = "** " ++ s ++ "\n" ++
                               "   " ++ orgTimestamp st ed ++
                               case d of Just d  -> '\n' : d
                                         Nothing -> ""

printEvents :: String -> [Event] -> IO ()
printEvents title xs = do putStrLn $ "* " ++ title
                          mapM_ (putStrLn . eventToOrg) xs

decodeJSON :: (FromJSON a) => BS.ByteString -> T.Text -> (a -> b) -> b
decodeJSON json key f = case decode json >>= parseMaybe (.: key) of
  Just x  -> f x
  Nothing -> error $ "cannot decode JSON: " ++ BS.unpack json

printCalendar :: String -> UTCTime -> String -> IO ()
printCalendar token time id = do
  let (timeMin, timeMax) = timeAround time
  json <- simpleHttp $ listEventsUri token timeMin timeMax id
  decodeJSON json "items" $ printEvents id

calendarList :: String -> IO [Calendar]
calendarList token = do
  let uri = "https://www.googleapis.com/calendar/v3/users/me/calendarList" ++
            "?access_token=" ++ token
  json <- simpleHttp uri
  decodeJSON json "items" return

main = do
  calendarIds <- getArgs
  token       <- oAuthToken
  time        <- getCurrentTime
  case calendarIds of
    [] -> calendarList token >>= mapM_ print
    _  -> mapM_ (printCalendar token time) calendarIds
