module Test (tests) where

import Data.Time
import Data.Time.Format
import Data.Time.LocalTime
import Main
import System.Locale
import Test.HUnit

sampleTime :: ParseTime t => Int -> t
sampleTime n = times !! n
  where times = map readLocalTime [ ("%F",    "2014-06-21")
                                  , ("%F %R", "2014-06-21 07:00")
                                  , ("%F",    "2014-06-22")
                                  , ("%F",    "2014-06-23")
                                  ]
        readLocalTime (fmt, s) = readTime defaultTimeLocale fmt s

testTimeAround =
  "timeAround" ~:
  ("2014-06-14T00:00:00+0000", "2014-06-28T00:00:00+0000") ~=?
   timeAround (sampleTime 0)

testListEventsUri =
  "listEventsUri" ~:
  "https://www.googleapis.com/calendar/v3/calendars/" ++
  "id%20with%20space/events?singleEvents=true&" ++
  "timeMin=mintime&timeMax=maxtime&access_token=token" ~=?
  listEventsUri "token" "mintime" "maxtime" "id with space"

testOrgTimestamp =
  "orgTimestamp" ~:
  [ "Date" ~:
    [ "same day" ~:
      "<2014-06-21 Sat>" ~=? orgSample Date 0 2
    , "different day" ~:
      "<2014-06-21 Sat>--<2014-06-22 Sun>" ~=? orgSample Date 0 3
    ]
  , "DateTime" ~:
    [ "same day" ~:
      "<2014-06-21 Sat 00:00-07:00>" ~=? orgSample DateTime 0 1
    , "different day" ~:
      "<2014-06-21 Sat 07:00>--<2014-06-22 Sun 00:00>" ~=?
      orgSample DateTime 1 2
    ]
  ]
  where orgSample tc m n =
          orgTimestamp (tc $ sampleTime m) (tc $ sampleTime n)

testEventToOrg = "eventToOrg" ~:  org ~=? eventToOrg e
  where e   = Event "summary" (Just "description")
              (DateTime $ sampleTime 0)
              (DateTime $ sampleTime 1)
        org = "** summary\n" ++
              "   <2014-06-21 Sat 00:00-07:00>\n" ++
              "description"

tests = runTestTT $ test [ testTimeAround
                         , testListEventsUri
                         , testOrgTimestamp
                         , testEventToOrg
                         ]
