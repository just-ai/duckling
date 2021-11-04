-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree. An additional grant
-- of patent rights can be found in the PATENTS file in the same directory.


{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# LANGUAGE OverloadedStrings #-}

module Duckling.Time.RU.Rules
  ( rules
  ) where

import Data.Maybe
import Data.Text (Text, head)
import Prelude
import qualified Data.Text as Text

import Duckling.Dimensions.Types
import Duckling.Duration.Helpers (duration)
import Duckling.Duration.Types (DurationData (..))
import Duckling.Numeral.Helpers (isNatural, parseInt)
import Duckling.Numeral.Types (NumeralData (..))
import Duckling.Ordinal.Types (OrdinalData (..))
import Duckling.Regex.Types
import Duckling.Time.Computed
import Duckling.Time.Helpers
import Duckling.Time.Types (TimeData (..), TimeIntervalType (..))
import Duckling.Types
import qualified Duckling.Duration.Types as TDuration
import qualified Duckling.Numeral.Types as TNumeral
import qualified Duckling.Ordinal.Types as TOrdinal
import qualified Duckling.Time.Types as TTime
import qualified Duckling.TimeGrain.Types as TG

ruleIntersect :: Rule
ruleIntersect = Rule
  { name = "intersect"
  , pattern =
    [ Predicate $ isGrainFinerThan TG.Year
    , Predicate $ or . sequence [isNotLatent, isGrainOfTime TG.Year]
    ]
  , prod = \tokens -> case tokens of
      (Token Time td1:Token Time td2:_)
        | (not $ TTime.latent td1) || (not $ TTime.latent td2) ->
        Token Time . notLatent <$> intersect td1 td2
      _ -> Nothing
  }

ruleIntersectOf :: Rule -- todo
ruleIntersectOf = Rule
  { name = "intersect by \",\", \"of\", \"from\", \"'s\""
  , pattern =
    [ Predicate isNotLatent
    , regex "of|from|for|'s|,"
    , Predicate isNotLatent
    ]
  , prod = \tokens -> case tokens of
      (Token Time td1:_:Token Time td2:_) ->
        Token Time . notLatent <$> intersect td1 td2
      _ -> Nothing
  }

ruleIntersectYear :: Rule -- todo
ruleIntersectYear = Rule
  { name = "intersect by \",\", \"of\", \"from\" for year"
  , pattern =
    [ Predicate isNotLatent
    , regex "of|from|,"
    , Predicate $ isGrainOfTime TG.Year
    ]
  , prod = \tokens -> case tokens of
      (Token Time td1:_:Token Time td2:_) ->
        Token Time . notLatent <$> intersect td1 td2
      _ -> Nothing
  }

ruleAbsorbOnDay :: Rule
ruleAbsorbOnDay = Rule
  { name = "on <day>"
  , pattern =
    [ regex "на"
    , Predicate $ isGrainOfTime TG.Day
    ]
  , prod = \tokens -> case tokens of
      (_:token:_) -> Just token
      _ -> Nothing
  }

ruleAbsorbOnADOW :: Rule
ruleAbsorbOnADOW = Rule
  { name = "в <named-day>"
  , pattern =
    [ regex "в"
    , Predicate isADayOfWeek
    ]
  , prod = \tokens -> case tokens of
      (_:token:_) -> Just token
      _ -> Nothing
  }

ruleTuesday :: Rule
ruleTuesday = Rule
  { name = "во вторник"
  , pattern =
    [ regex "во"
    , regex "вторник"
    ]
  , prod = \_ -> tt $ dayOfWeek 2
  }

ruleAbsorbInMonthYear :: Rule
ruleAbsorbInMonthYear = Rule
  { name = "в(течение)/на <named-month>|year"
  , pattern =
    [ regex "(на|в(|\\sтечение))"
    , Predicate $ or . sequence [isAMonth, isGrainOfTime TG.Year]
    ]
  , prod = \tokens -> case tokens of
      (_:Token Time td:_) -> tt $ notLatent td
      _ -> Nothing
  }

ruleAbsorbInYearSuffix :: Rule
ruleAbsorbInYearSuffix = Rule
  { name = "в(течение) <named-month>|year года"
  , pattern =
    [ regex "в(|\\sтечение)"
    , Predicate $ isGrainOfTime TG.Year
    , regex "год.?"
    ]
  , prod = \tokens -> case tokens of
      (_:Token Time td:_) -> tt $ notLatent td
      _ -> Nothing
  }

ruleAbsorbCommaTOD :: Rule
ruleAbsorbCommaTOD = Rule
  { name = "absorption of , after named day"
  , pattern =
    [ Predicate isADayOfWeek
    , regex ","
    ]
  , prod = \tokens -> case tokens of
      (token:_) -> Just token
      _ -> Nothing
  }

ruleInstants :: [Rule]
ruleInstants = mkRuleInstants
  [ ("right now"            , TG.Second, 0  , "прямо? сейчас"     )
  , ("today"                , TG.Day   , 0  , "сегодня"           )
  , ("tomorrow"             , TG.Day   , 1  , "завтра"            )
  , ("tomorrow"             , TG.Day   , 1  , "завтрашн...?"      )
  , ("day-after-tomorrow"   , TG.Day   , 2  , "послезавтра"       )
  , ("day-after-tomorrow"   , TG.Day   , 2  , "послезавтрашн...?" )
  , ("yesterday"            , TG.Day   , - 1, "вчера"             )
  , ("yesterday"            , TG.Day   , - 1, "вчерашн...?"       )
  , ("day-before-yesterday" , TG.Day   , - 2, "позавчера"         )
  , ("day-before-yesterday" , TG.Day   , - 2, "позавчерашн...,"   )
  ]

ruleInstantDay :: Rule
ruleInstantDay = Rule
  { name = "(после)завтрашний/вчерашний день"
  , pattern =
    [ regex "((?:после)?завтрашн|(?:поза)?вчерашн)...?"
    , regex "(день|дня|дне)"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):_) ->
        case Text.toLower match of
          "завтрашн"      -> tt $ cycleNth TG.Day 1
          "послезавтрашн" -> tt $ cycleNth TG.Day 2
          "вчерашн"       -> tt $ cycleNth TG.Day $ - 1
          "позазавтрашн"  -> tt $ cycleNth TG.Day $ - 2
          _ -> Nothing
      _ -> Nothing
  }

ruleNow :: Rule
ruleNow = Rule
  { name = "now"
  , pattern =
    [ regex "сейчас"
    ]
  , prod = \_ -> tt now
  }

ruleNextDOW :: Rule
ruleNextDOW = Rule
  { name = "this|next <day-of-week>"
  , pattern =
    [ regex "этот|следующ...?"
    , Predicate isADayOfWeek
    ]
  , prod = \tokens -> case tokens of
      (_:Token Time td:_) -> tt $ predNth 0 True td
      _ -> Nothing
  }

ruleThisTime :: Rule -- todo fix
ruleThisTime = Rule
  { name = "this <time>"
  , pattern =
    [ regex "this|current|coming"
    , Predicate isOkWithThisNext
    ]
  , prod = \tokens -> case tokens of
      (_:Token Time td:_) -> tt $ predNth 0 False td
      _ -> Nothing
  }

ruleNextTime :: Rule
ruleNextTime = Rule
  { name = "next <time>"
  , pattern =
    [ regex "следующ...?"
    , Predicate isOkWithThisNext
    ]
  , prod = \tokens -> case tokens of
      (_:Token Time td:_) -> tt $ predNth 0 True td
      _ -> Nothing
  }

ruleLastTime :: Rule
ruleLastTime = Rule
  { name = "на прошедшей <time>"
  , pattern =
    [ regex "(в|на)\\s(последн...?|прошл...?|прошедш...?)"
    , Predicate isOkWithThisNext
    ]
  , prod = \tokens -> case tokens of
      (_:Token Time td:_) -> tt $ predNth (- 1) False td
      _ -> Nothing
  }

ruleLastWeekendOfMonth :: Rule -- todo fix
ruleLastWeekendOfMonth = Rule
  { name = "last weekend of <named-month>"
  , pattern =
    [ regex "last\\s(week(\\s|-)?end|wkend)\\s(of|in)"
    , Predicate isAMonth
    ]
  , prod = \tokens -> case tokens of
      (_:Token Time td2:_) -> tt $ predLastOf weekend td2
      _ -> Nothing
  }

ruleTimeBeforeLastAfterNext :: Rule -- todo fix
ruleTimeBeforeLastAfterNext = Rule
  { name = "<time> before last|after next"
  , pattern =
    [ dimension Time
    , regex "(before last|after next)"
    ]
  , prod = \tokens -> case tokens of
      (Token Time td:Token RegexMatch (GroupMatch (match:_)):_) ->
        tt $ predNth 1 (Text.toLower match == "after next") td
      _ -> Nothing
  }

ruleLastDOWOfTime :: Rule
ruleLastDOWOfTime = Rule
  { name = "last <day-of-week> of <time>"
  , pattern =
    [ regex "(в|на) последн.."
    , Predicate isADayOfWeek
    -- , regex "of"
    , dimension Time
    ]
  , prod = \tokens -> case tokens of
      -- (_:Token Time td1:_:Token Time td2:_) ->
      (_:Token Time td1:Token Time td2:_) ->
        tt $ predLastOf td1 td2
      _ -> Nothing
  }

ruleLastCycleOfTime :: Rule -- todo fix
ruleLastCycleOfTime = Rule
  { name = "last <cycle> of <time>"
  , pattern =
    [ regex "last"
    , dimension TimeGrain
    , regex "of|in"
    , dimension Time
    ]
  , prod = \tokens -> case tokens of
      (_:Token TimeGrain grain:_:Token Time td:_) ->
        tt $ cycleLastOf grain td
      _ -> Nothing
  }

ruleLastNight :: Rule -- todo fix
ruleLastNight = Rule
  { name = "last night"
  , pattern =
    [ regex "(late )?last night"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):_) ->
        let hours = if Text.toLower match == "late " then 3 else 6
            start = durationBefore (DurationData hours TG.Hour) today
        in Token Time . partOfDay . notLatent <$> interval TTime.Open start today
      _ -> Nothing
  }

ruleNthTimeOfTime :: Rule -- todo fix
ruleNthTimeOfTime = Rule
  { name = "nth <time> of <time>"
  , pattern =
    [ dimension Ordinal
    , dimension Time
    , regex "of|in"
    , dimension Time
    ]
  , prod = \tokens -> case tokens of
      (Token Ordinal od:Token Time td1:_:Token Time td2:_) -> Token Time .
        predNth (TOrdinal.value od - 1) False <$> intersect td2 td1
      _ -> Nothing
  }

ruleTheNthTimeOfTime :: Rule -- todo fix
ruleTheNthTimeOfTime = Rule
  { name = "the nth <time> of <time>"
  , pattern =
    [ regex "the"
    , dimension Ordinal
    , dimension Time
    , regex "of|in"
    , dimension Time
    ]
  , prod = \tokens -> case tokens of
      (_:Token Ordinal od:Token Time td1:_:Token Time td2:_) -> Token Time .
         predNth (TOrdinal.value od - 1) False <$> intersect td2 td1
      _ -> Nothing
  }

ruleNthTimeAfterTime :: Rule -- todo fix
ruleNthTimeAfterTime = Rule
  { name = "nth <time> after <time>"
  , pattern =
    [ dimension Ordinal
    , dimension Time
    , regex "after"
    , dimension Time
    ]
  , prod = \tokens -> case tokens of
      (Token Ordinal od:Token Time td1:_:Token Time td2:_) ->
        tt $ predNthAfter (TOrdinal.value od - 1) td1 td2
      _ -> Nothing
  }

ruleTheNthTimeAfterTime :: Rule -- todo fix
ruleTheNthTimeAfterTime = Rule
  { name = "the nth <time> after <time>"
  , pattern =
    [ regex "the"
    , dimension Ordinal
    , dimension Time
    , regex "after"
    , dimension Time
    ]
  , prod = \tokens -> case tokens of
      (_:Token Ordinal od:Token Time td1:_:Token Time td2:_) ->
        tt $ predNthAfter (TOrdinal.value od - 1) td1 td2
      _ -> Nothing
  }

ruleYearLatent :: Rule
ruleYearLatent = Rule
  { name = "<year> (latent)"
  , pattern = [ Predicate $ isIntegerBetween 25 10000 ]
  , prod = \tokens -> case tokens of
      (token:_) -> do
        n <- getIntValue token
        tt . mkLatent $ year n
      _ -> Nothing
  }

ruleYearLatent0 :: Rule
ruleYearLatent0 = Rule
  { name = "<00-year> (latent)"
  , pattern =
    [ regex "(00[0-9]*)"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (t:_)):_) -> do
        n <- parseInt t
        td <- year2 n
        tt . mkLatent $ td
      _ -> Nothing
  }

ruleYearLatent2 :: Rule
ruleYearLatent2 = Rule
  { name = "<year> год (latent)"
  , pattern =
      [ Predicate $
        or . sequence [isIntegerBetween (- 10000) 0, isIntegerBetween 25 10000]
      , regex "год.?"
      ]
  , prod = \tokens -> case tokens of
      (token:_) -> do
        n <- getIntValue token
        tt . mkLatent $ year n
      _ -> Nothing
  }

ruleYearOrdinalLatent :: Rule
ruleYearOrdinalLatent = Rule
  { name = "<year> (ordinal, latent)"
  , pattern =
      [ Predicate $
        or . sequence [isOrdinalBetween (- 10000) 0, isOrdinalBetween 25 10000]
      ]
  , prod = \tokens -> case tokens of
      (token:_) -> do
        n <- getIntValue token
        tt . mkLatent $ year n
      _ -> Nothing
  }

ruleYearOrdinalLatent2 :: Rule
ruleYearOrdinalLatent2 = Rule
  { name = "<year> (ordinal, latent)"
  , pattern =
      [ Predicate $
        or . sequence [isOrdinalBetween (- 10000) 0, isOrdinalBetween 25 10000]
      , regex "год.?"
      ]
  , prod = \tokens -> case tokens of
      (token:_) -> do
        n <- getIntValue token
        tt . mkLatent $ year n
      _ -> Nothing
  }

ruleYearADBC :: Rule -- todo fix
ruleYearADBC = Rule
  { name = "<year> (bc|ad)"
  , pattern =
    [ Predicate $ isIntegerBetween (-10000) 10000
    , regex "(a\\.?d\\.?|b\\.?c\\.?)"
    ]
  , prod = \case
    (token:Token RegexMatch (GroupMatch (ab:_)):_) -> do
      y <- getIntValue token
      tt . yearADBC $ if Text.head (Text.toLower ab) == 'b' then -y else y
    _ -> Nothing
  }

ruleDOMLatent :: Rule
ruleDOMLatent = Rule
  { name = "<day-of-month> (ordinal)"
  , pattern = [Predicate isDOMOrdinal]
  , prod = \tokens -> case tokens of
      (token:_) -> do
        n <- getIntValue token
        tt . mkLatent $ dayOfMonth n
      _ -> Nothing
  }

ruleTheDOMNumeral :: Rule -- todo fix
ruleTheDOMNumeral = Rule
  { name = "the <day-of-month> (number)"
  , pattern =
    [ regex "the"
    , Predicate isDOMInteger
    ]
  , prod = \tokens -> case tokens of
      (_:token:_) -> do
        n <- getIntValue token
        tt . mkLatent $ dayOfMonth n
      _ -> Nothing
  }

ruleTheDOMOrdinal :: Rule -- todo fix
ruleTheDOMOrdinal = Rule
  { name = "the <day-of-month> (ordinal)"
  , pattern =
    [ regex "the"
    , Predicate isDOMOrdinal
    ]
  , prod = \tokens -> case tokens of
      (_:
       Token Ordinal OrdinalData{TOrdinal.value = v}:
       _) -> tt $ dayOfMonth v
      _ -> Nothing
  }

ruleNamedDOMOrdinal :: Rule
ruleNamedDOMOrdinal = Rule
  { name = "<named-month>|<named-day> <day-of-month> (ordinal)"
  , pattern =
    [ Predicate $ or . sequence [isAMonth, isADayOfWeek]
    , Predicate isDOMOrdinal
    ]
  , prod = \tokens -> case tokens of
      (Token Time td:token:_) -> Token Time <$> intersectDOM td token
      _ -> Nothing
  }

ruleMonthDOMNumeral :: Rule
ruleMonthDOMNumeral = Rule
  { name = "<named-month> <day-of-month> (non ordinal)"
  , pattern =
    [ Predicate isAMonth
    , Predicate isDOMInteger
    ]
  , prod = \tokens -> case tokens of
      (Token Time td:token:_) -> Token Time <$> intersectDOM td token
      _ -> Nothing
  }

ruleDOMOfMonth :: Rule -- todo maybe fix
ruleDOMOfMonth = Rule
  { name = "<day-of-month> (ordinal or number) of <named-month>"
  , pattern =
    [ Predicate isDOMValue
    -- , regex "of|in"
    , Predicate isAMonth
    ]
  , prod = \tokens -> case tokens of
      -- (token:_:Token Time td:_) -> Token Time <$> intersectDOM td token
      (token:Token Time td:_) -> Token Time <$> intersectDOM td token
      _ -> Nothing
  }

ruleDOMMonth :: Rule
ruleDOMMonth = Rule
  { name = "<day-of-month> (ordinal or number) <named-month>"
  , pattern =
    [ Predicate isDOMValue
    , Predicate isAMonth
    ]
  , prod = \tokens -> case tokens of
      (token:Token Time td:_) -> Token Time <$> intersectDOM td token
      _ -> Nothing
  }

ruleDOMMonthYear :: Rule
ruleDOMMonthYear = Rule
  { name = "<day-of-month>(ordinal or number)/<named-month>/<year> (1)"
  , pattern =
    [ Predicate isDOMValue
    , regex "[-/\\s]"
    , Predicate isAMonth
    , regex "[-/\\s]"
    , regex "(\\d{4})"
    ]
  , prod = \tokens -> case tokens of
      (token:
       _:
       Token Time td:
       _:
       Token RegexMatch (GroupMatch (ymatch:_)):
       _) -> do
         intVal <- parseInt ymatch
         dom <- intersectDOM td token
         td <- year2 intVal
         Token Time <$> intersect dom td
      _ -> Nothing
  }

ruleDOMMonthYear2 :: Rule
ruleDOMMonthYear2 = Rule
  { name = "<day-of-month>(ordinal or number)/<named-month>/<year> (2)"
  , pattern =
    [ Predicate isDOMValue
    , regex "[-/\\s]"
    , Predicate isAMonth
    , regex "[-/\\s]"
    , Predicate $ isGrainOfTime TG.Year
    ]
  , prod = \tokens -> case tokens of
      (token:_:Token Time td1:_:Token Time td2:_) -> do
        dom <- intersectDOM td1 token
        Token Time <$> intersect dom td2
      _ -> Nothing
  }

ruleDOMMonthYear3 :: Rule
ruleDOMMonthYear3 = Rule
  { name = "<day-of-month>(ordinal or number)/<named-month>/<year> year"
  , pattern =
    [ Predicate isDOMValue
    , regex "[-/\\s]"
    , Predicate isAMonth
    , regex "[-/\\s]"
    , Predicate $ isGrainOfTime TG.Year
    , regex "год.?"
    ]
  , prod = \tokens -> case tokens of
      (token:_:Token Time td1:_:Token Time td2:_) -> do
        dom <- intersectDOM td1 token
        Token Time <$> intersect dom td2
      _ -> Nothing
  }

ruleDOMOrdinalMonthYear :: Rule
ruleDOMOrdinalMonthYear = Rule
  { name = "<day-of-month>(ordinal) <named-month> year"
  , pattern =
    [ Predicate isDOMOrdinal
    , Predicate isAMonth
    , regex "(\\d{2,4})"
    ]
  , prod = \tokens -> case tokens of
      (token:Token Time td:Token RegexMatch (GroupMatch (yy:_)):_) -> do
        dom <- intersectDOM td token

        y <- parseInt yy
        case Text.length yy of
          2 -> Token Time <$> intersect dom (year y)
          4 -> do
            yt <- year2 y
            Token Time <$> intersect dom yt
          _ -> Nothing
        
        
      _ -> Nothing
  }

ruleIdesOfMonth :: Rule -- todo fix
ruleIdesOfMonth = Rule
  { name = "the ides of <named-month>"
  , pattern =
    [ regex "the ides? of"
    , Predicate isAMonth
    ]
  , prod = \tokens -> case tokens of
      (_:Token Time td@TimeData {TTime.form = Just (TTime.Month m)}:_) ->
        Token Time <$>
          intersect td (dayOfMonth $ if elem m [3, 5, 7, 10] then 15 else 13)
      _ -> Nothing
  }

ruleTODLatent :: Rule
ruleTODLatent = Rule
  { name = "time-of-day (latent)"
  , pattern =
    [ Predicate $ isIntegerBetween 2 23
    ]
  , prod = \tokens -> case tokens of
      (token:_) -> do
        n <- getIntValue token
        tt . mkLatent $ hour (0 < n && n < 13) n
      _ -> Nothing
  }

ruleAtTOD :: Rule
ruleAtTOD = Rule
  { name = "at <time-of-day>"
  , pattern =
    [ regex "в|на|@"
    , Predicate isATimeOfDay
    ]
  , prod = \tokens -> case tokens of
      (_:Token Time td:_) -> tt $ notLatent td
      _ -> Nothing
  }

ruleTODOClock1 :: Rule
ruleTODOClock1 = Rule
  { name = "N часов (0, 5..20)"
  , pattern =
    [ Predicate $ or . sequence [isIntegerBetween 0 0, isIntegerBetween 5 20]
    , regex "часов"
    ]
  , prod = \tokens -> case tokens of
      (token:_) -> do
        n <- getIntValue token
        tt . notLatent $ hour (0 < n && n < 13) n
      _ -> Nothing
  }

ruleTODOClock2 :: Rule
ruleTODOClock2 = Rule
  { name = "N час (1..4, 22..23)"
  , pattern =
    [ Predicate $ or . sequence [isIntegerBetween 1 4, isIntegerBetween 22 23]
    , regex "часа?"
    ]
  , prod = \tokens -> case tokens of
      (token:_) -> do
        n <- getIntValue token
        tt . notLatent $ hour (0 < n && n < 13) n
      _ -> Nothing
  }

ruleHHMM :: Rule
ruleHHMM = Rule
  { name = "hh:mm"
  , pattern = [regex "((?:[01]?\\d)|(?:2[0-3]))[:-]([0-5]\\d)"]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (hh:mm:_)):_) -> do
        h <- parseInt hh
        m <- parseInt mm
        tt $ hourMinute (0 < h && h < 13) h m
      _ -> Nothing
  }

ruleHHMMLatent :: Rule
ruleHHMMLatent = Rule
  { name = "hhmm (latent)"
  , pattern =
    [ regex "((?:[01]?\\d)|(?:2[0-3]))([0-5]\\d)(?!.\\d)"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (hh:mm:_)):_) -> do
        h <- parseInt hh
        m <- parseInt mm
        tt . mkLatent $ hourMinute True h m
      _ -> Nothing
  }

ruleHHMMSS :: Rule
ruleHHMMSS = Rule
  { name = "hh:mm:ss"
  , pattern = [regex "((?:[01]?\\d)|(?:2[0-3]))[:.]([0-5]\\d)[:.]([0-5]\\d)"]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (hh:mm:ss:_)):_) -> do
        h <- parseInt hh
        m <- parseInt mm
        s <- parseInt ss
        tt $ hourMinuteSecond True h m s
      _ -> Nothing
  }

ruleMilitaryAMPM :: Rule -- todo fix?
ruleMilitaryAMPM = Rule
  { name = "hhmm (military) am|pm"
  , pattern =
    [ regex "((?:1[012]|0?\\d))([0-5]\\d)"
    , regex "([ap])\\.?m?\\.?"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (hh:mm:_)):
       Token RegexMatch (GroupMatch (ap:_)):
       _) -> do
        h <- parseInt hh
        m <- parseInt mm
        tt . timeOfDayAMPM (Text.toLower ap == "a") $ hourMinute True h m
      _ -> Nothing
  }

ruleMilitarySpelledOutAMPM :: Rule -- todo fix?
ruleMilitarySpelledOutAMPM = Rule
  { name = "military spelled out numbers am|pm"
  , pattern =
    [ Predicate $ isIntegerBetween 10 12
    , Predicate $ isIntegerBetween 1 59
    , regex "(in the )?([ap])(\\s|\\.)?m?\\.?"
    ]
    , prod = \tokens -> case tokens of
        (h:m:Token RegexMatch (GroupMatch (_:ap:_)):_) -> do
          hh <- getIntValue h
          mm <- getIntValue m
          tt . timeOfDayAMPM (Text.toLower ap == "a") $ hourMinute True hh mm
        _ -> Nothing
  }

ruleMilitarySpelledOutAMPM2 :: Rule -- todo fix?
ruleMilitarySpelledOutAMPM2 = Rule
  { name = "six thirty six a.m."
  , pattern =
    [ Predicate $ isIntegerBetween 110 999
    , regex "(in the )?([ap])(\\s|\\.)?m?\\.?"
    ]
  , prod = \tokens -> case tokens of
      (token:Token RegexMatch (GroupMatch (_:ap:_)):_) -> do
        n <- getIntValue token
        m <- case mod n 100 of
          v | v < 60 -> Just v
          _          -> Nothing
        let h = quot n 100
        tt . timeOfDayAMPM (Text.toLower ap == "a") $ hourMinute True h m
      _ -> Nothing
  }

ruleTODAMPM :: Rule -- todo fix?
ruleTODAMPM = Rule
  { name = "<time-of-day> am|pm"
  , pattern =
    [ Predicate isATimeOfDay
    , regex "(in the )?([ap])(\\s|\\.)?(m?)\\.?"
    ]
  , prod = \tokens -> case tokens of
      (Token Time td@TimeData{TTime.latent = True}:
       Token RegexMatch (GroupMatch (_:ap:_:"":_)):
       _) ->
        tt . mkLatent $ timeOfDayAMPM (Text.toLower ap == "a") td
      (Token Time td@TimeData{TTime.form = Just (TTime.TimeOfDay (Just hours) _)}:
       Token RegexMatch (GroupMatch (_:ap:_)):
       _) | hours < 13 ->
        tt $ timeOfDayAMPM (Text.toLower ap == "a") td
      _ -> Nothing
  }

ruleHONumeral :: Rule
ruleHONumeral = Rule
  { name = "<hour-of-day> <integer>"
  , pattern =
    [ Predicate isAnHourOfDay
    , Predicate $ isIntegerBetween 1 59
    ]
  , prod = \tokens -> case tokens of
      (Token Time TimeData{TTime.form = Just (TTime.TimeOfDay (Just hours) is12H)
                          ,TTime.latent = isLatent}:
       token:
       _) -> do
        n <- getIntValue token
        if isLatent
          then tt . mkLatent $ hourMinute is12H hours n
          else tt $ hourMinute is12H hours n
      _ -> Nothing
  }

ruleHONumeralWithZero :: Rule
ruleHONumeralWithZero = Rule
  { name = "<hour-of-day> ноль 0..9"
  , pattern =
    [ Predicate isAnHourOfDay
    , regex "ноль"
    , Predicate $ isIntegerBetween 0 9
    ]
  , prod = \tokens -> case tokens of
      (Token Time TimeData{TTime.form = Just (TTime.TimeOfDay (Just hours) is12H)
                          ,TTime.latent = isLatent}:
       _:
       token:
       _) -> do
        n <- getIntValue token
        if isLatent
          then tt . mkLatent $ hourMinute is12H hours n
          else tt $ hourMinute is12H hours n
      _ -> Nothing
  }

ruleHONumeralSuffix :: Rule
ruleHONumeralSuffix = Rule
  { name = "<hour-of-day> <integer> минут"
  , pattern =
    [ Predicate isAnHourOfDay
    , Predicate $ isIntegerBetween 0 59
    , regex "мин(ут|\\.|)"
    ]
  , prod = \tokens -> case tokens of
      (Token Time TimeData{TTime.form = Just (TTime.TimeOfDay (Just hours) is12H)
                          ,TTime.latent = isLatent}:
       token:
       _) -> do
        n <- getIntValue token
        if isLatent
          then tt . mkLatent $ hourMinute is12H hours n
          else tt $ hourMinute is12H hours n
      _ -> Nothing
  }

ruleHONumeralSuffixWithZero :: Rule
ruleHONumeralSuffixWithZero = Rule
  { name = "<hour-of-day> ноль 0..9 минут"
  , pattern =
    [ Predicate isAnHourOfDay
    , regex "ноль"
    , Predicate $ isIntegerBetween 0 9
    , regex "мин(ут|\\.|)"
    ]
  , prod = \tokens -> case tokens of
      (Token Time TimeData{TTime.form = Just (TTime.TimeOfDay (Just hours) is12H)
                          ,TTime.latent = isLatent}:
       _:
       token:
       _) -> do
        n <- getIntValue token
        if isLatent
          then tt . mkLatent $ hourMinute is12H hours n
          else tt $ hourMinute is12H hours n
      _ -> Nothing
  }

hourToHalfToken :: Int -> Maybe Token
hourToHalfToken h = if (1 <= h && h <= 12) then
                      tt $ hourMinute True (h - 1) 30
                    else
                      Nothing

ruleHODHalf :: Rule -- todo remove?
ruleHODHalf = Rule
  { name = "half <hour-of-day>"
  , pattern =
    [ regex "(пол|половин.)"
    , Predicate isAnHourOfDay
    ]
  , prod = \tokens -> case tokens of
      (_:Token Time TimeData{TTime.form = Just (TTime.TimeOfDay (Just hours) True)}:_) 
        -> hourToHalfToken hours
      _ -> Nothing
  }

ruleHODHalfOrdinal :: Rule
ruleHODHalfOrdinal = Rule
  { name = "half-long <hour-of-day-ordinal>"
  , pattern =
    [ regex "половин."
    , dimension Ordinal
    ]
  , prod = \tokens -> case tokens of
      (_:token:_) -> do
        h <- getIntValue token
        hourToHalfToken h
      _ -> Nothing
  }

ruleHODHalfShortOrdinal :: Rule
ruleHODHalfShortOrdinal = Rule
  { name = "half-short <hour-of-day-ordinal>"
  , pattern =
    [ regex "пол"
    , dimension Ordinal
    ]
  , prod = \tokens -> case tokens of
      (_:token:_) -> do
        h <- getIntValue token
        hourToHalfToken h
      _ -> Nothing
  }

ruleHODHalfShortJoinedOrdinal :: Rule
ruleHODHalfShortJoinedOrdinal = Rule
  { name = "half-short <hour-of-day-ordinal>"
  , pattern =
    [ regex "пол-?(первого|второго|третьего|четвёртого|пятого|шестого|седьмого|восьмого|девятого|десятого|одинн?адцатого|двенадцатого)"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):_) -> do
        let h = case Text.toLower match of
              "первого"       -> 1
              "второго"       -> 2
              "третьего"      -> 3
              "четвёртого"    -> 4
              "пятого"        -> 5
              "шестого"       -> 6
              "седьмого"      -> 7
              "восьмого"      -> 8
              "девятого"      -> 9
              "десятого"      -> 10
              "одинадцатого"  -> 11
              "одиннадцатого" -> 11
              "двенадцатого"  -> 12
              _               -> -1
        hourToHalfToken h
      _ -> Nothing
  }

ruleQuarterHODOrdinal :: Rule -- todo fix
ruleQuarterHODOrdinal = Rule
  { name = "четверть <hour-of-day-ordinal>"
  , pattern =
    [ regex "четверть"
    , dimension Ordinal
    ]
  , prod = \tokens -> case tokens of
      (_:h:_) -> do
        hh <- getIntValue h
        case hh of
          x | 1 <= x && x <= 12 -> tt $ hourMinute True (hh - 1) 15
          _ -> Nothing
      _ -> Nothing
  }

ruleWithoutQuarterToHOD :: Rule
ruleWithoutQuarterToHOD = Rule
  { name = "без четверти <hour-of-day>"
  , pattern =
    [ regex "без"
    , regex "четверти"
    , Predicate isAnHourOfDay
    ]
  , prod = \tokens -> case tokens of
      (_:_:Token Time td:_) -> do
        Token Time <$> minutesBefore 15 td
      _ -> Nothing
  }

ruleWithoutNumeralToHOD :: Rule
ruleWithoutNumeralToHOD = Rule
  { name = "без <integer> <hour-of-day>"
  , pattern =
    [ regex "без"
    , Predicate $ isIntegerBetween 1 29
    , Predicate isAnHourOfDay
    ]
  , prod = \tokens -> case tokens of
      (_:token:Token Time td:_) -> do
        n <- getIntValue token
        Token Time <$> minutesBefore n td
      _ -> Nothing
  }

ruleNumeralToHODOrdinal :: Rule
ruleNumeralToHODOrdinal = Rule
  { name = "<integer> минут <hour-of-day-ordinal>"
  , pattern =
    [ Predicate $ isIntegerBetween 1 29
    , regex "мин(ут|\\.|)"
    , dimension Ordinal
    ]
  , prod = \tokens -> case tokens of
      (m:_:h:_) -> do
        mm <- getIntValue m
        hh <- getIntValue h
        tt $ hourMinute (0 < hh && hh < 13) (hh - 1) mm
      _ -> Nothing
  }

-- todo from here, alala
ruleHalfToHOD :: Rule -- todo fix
ruleHalfToHOD = Rule
  { name = "half to|till|before <hour-of-day>"
  , pattern =
    [ regex "half (to|till|before|of)"
    , Predicate isAnHourOfDay
    ]
  , prod = \tokens -> case tokens of
      (_:Token Time td:_) -> Token Time <$> minutesBefore 30 td
      _ -> Nothing
  }

ruleQuarterToHOD :: Rule -- todo fix
ruleQuarterToHOD = Rule
  { name = "quarter to|till|before <hour-of-day>"
  , pattern =
    [ regex "(a|one)? ?quarter (to|till|before|of)"
    , Predicate isAnHourOfDay
    ]
  , prod = \tokens -> case tokens of
      (_:Token Time td:_) -> Token Time <$> minutesBefore 15 td
      _ -> Nothing
  }

ruleNumeralAfterHOD :: Rule -- todo fix
ruleNumeralAfterHOD = Rule
  { name = "integer after|past <hour-of-day>"
  , pattern =
    [ Predicate $ isIntegerBetween 1 59
    , regex "after|past"
    , Predicate isAnHourOfDay
    ]
  , prod = \tokens -> case tokens of
      (token:_:Token Time td:_) -> do
        n <- getIntValue token
        Token Time <$> minutesAfter n td
      _ -> Nothing
  }

ruleHalfAfterHOD :: Rule
ruleHalfAfterHOD = Rule
  { name = "half after|past <hour-of-day>"
  , pattern =
    [ regex "half (after|past)"
    , Predicate isAnHourOfDay
    ]
  , prod = \tokens -> case tokens of
      (_:Token Time td:_) -> Token Time <$> minutesAfter 30 td
      _ -> Nothing
  }

ruleQuarterAfterHOD :: Rule
ruleQuarterAfterHOD = Rule
  { name = "quarter after|past <hour-of-day>"
  , pattern =
    [ regex "(a|one)? ?quarter (after|past)"
    , Predicate isAnHourOfDay
    ]
  , prod = \tokens -> case tokens of
      (_:Token Time td:_) -> Token Time <$> minutesAfter 15 td
      _ -> Nothing
  }

ruleHalfHOD :: Rule
ruleHalfHOD = Rule
  { name = "half <integer> (UK style hour-of-day)"
  , pattern =
    [ regex "half"
    , Predicate isAnHourOfDay
    ]
  , prod = \tokens -> case tokens of
      (_:Token Time td:_) -> Token Time <$> minutesAfter 30 td
      _ -> Nothing
  }

ruleDDMM :: Rule
ruleDDMM = Rule
  { name = "dd.mm"
  , pattern =
    [ regex "(3[01]|[12]\\d|0?[1-9])[./](0[1-9]|1[0-2]?|[2-9])"
    ]
  , prod = \case
      (Token RegexMatch (GroupMatch (dd:mm:_)):_) -> do
        d <- parseInt dd
        m <- parseInt mm
        tt $ monthDay m d
      _ -> Nothing
  }

ruleDDMM2 :: Rule
ruleDDMM2 = Rule
  { name = "dd mm"
  , pattern =
    [ regex "(3[01]|[12]\\d|0?[1-9]) (0[1-9]|1[0-2]?|[2-9])"
    ]
  , prod = \case
      (Token RegexMatch (GroupMatch (dd:mm:_)):_) -> do
        d <- parseInt dd
        m <- parseInt mm
        tt $ monthDay m d
      _ -> Nothing
  }

ruleDDMMYY :: Rule
ruleDDMMYY = Rule
  { name = "dd.mm.yy"
  , pattern =
    [ regex "(3[01]|[12]\\d|0?[1-9])\\.(0?[1-9]|1[0-2])\\.(\\d{2,4})"
    ]
  , prod = \case
      (Token RegexMatch (GroupMatch (dd:mm:yy:_)):_) -> do
        d <- parseInt dd
        m <- parseInt mm
        y <- parseInt yy
        case Text.length yy of
          2 -> tt $ yearMonthDay y m d
          4 -> do
            td <- yearMonthDay2 y m d
            tt $ td
          _ -> Nothing
      _ -> Nothing
  }

ruleDDMMYY2 :: Rule
ruleDDMMYY2 = Rule
  { name = "dd mm yy"
  , pattern =
    [ regex "(3[01]|[12]\\d|0?[1-9]) (0?[1-9]|1[0-2]) (\\d{2,4})"
    ]
  , prod = \case
      (Token RegexMatch (GroupMatch (dd:mm:yy:_)):_) -> do
        d <- parseInt dd
        m <- parseInt mm
        y <- parseInt yy
        case Text.length yy of
          2 -> tt $ yearMonthDay y m d
          4 -> do
            td <- yearMonthDay2 y m d
            tt $ td
          _ -> Nothing
      _ -> Nothing
  }

ruleMMYYYY :: Rule
ruleMMYYYY = Rule
  { name = "mm/yyyy"
  , pattern =
    [ regex "(0?[1-9]|1[0-2])[/-](\\d{4})"
    ]
  , prod = \case
      (Token RegexMatch (GroupMatch (mm:yy:_)):_) -> do
        y <- parseInt yy
        m <- parseInt mm
        td <- yearMonth2 y m
        tt $ td
      _ -> Nothing
  }

ruleDDMMYY3 = Rule
  { name = "dd/mm/{yy, yyyy}?"
  , pattern =
    [ regex "(3[01]|[12]\\d|0?[1-9])/(0?[1-9]|1[0-2])/(\\d{2,4})"
    ]
  , prod = \case
      (Token RegexMatch (GroupMatch (dd:mm:yy:_)):_) -> do
        d <- parseInt dd
        m <- parseInt mm
        -- tt $ monthDay m d
        y <- parseInt yy
        case Text.length yy of
          2 -> tt $ yearMonthDay y m d
          4 -> do
            td <- yearMonthDay2 y m d
            tt $ td
          _ -> Nothing
      (Token RegexMatch (GroupMatch (dd:mm:_)):_) -> do
        d <- parseInt dd
        m <- parseInt mm
        tt $ monthDay m d
      _ -> Nothing
  }


ruleYYYYMM :: Rule
ruleYYYYMM = Rule
  { name = "yyyy-mm"
  , pattern =
    [ regex "(\\d{4})\\s*[/-]\\s*(1[0-2]|0?[1-9])"
    ]
  , prod = \case
      (Token RegexMatch (GroupMatch (yy:mm:_)):_) -> do
        y <- parseInt yy
        m <- parseInt mm
        td <- yearMonth2 y m
        tt $ td
      _ -> Nothing
  }

ruleYYYYMMDD :: Rule
ruleYYYYMMDD = Rule
  { name = "yyyy-mm-dd"
  , pattern =
    [ regex "(\\d{2,4})-(0?[1-9]|1[0-2])-(3[01]|[12]\\d|0?[1-9])"
    ]
  , prod = \case
      (Token RegexMatch (GroupMatch (yy:mm:dd:_)):_) -> do
        y <- parseInt yy
        m <- parseInt mm
        d <- parseInt dd
        case Text.length yy of
          2 -> tt $ yearMonthDay y m d
          4 -> do
            td <- yearMonthDay2 y m d
            tt $ td
          _ -> Nothing
      _ -> Nothing
  }

ruleYYYYQQ :: Rule
ruleYYYYQQ = Rule
  { name = "yyyyqq"
  , pattern =
    [ regex "(\\d{2,4})q([1-4])"
    ]
  , prod = \case
      (Token RegexMatch (GroupMatch (yy:qq:_)):_) -> do
        y <- parseInt yy
        q <- parseInt qq
        tt . cycleNthAfter True TG.Quarter (q - 1) $ year y
      _ -> Nothing
  }

ruleNoon :: Rule
ruleNoon = Rule
  { name = "полдень"
  , pattern =
    [ regex "(в )?(полдень|полудн.)"
    ]
  , prod = \tokens -> case tokens of
      _ -> tt $ hour False 12
  }

ruleAfterNoon :: Rule
ruleAfterNoon = Rule
  { name = "<time-of-day> пополудни"
  , pattern =
    [ Predicate isATimeOfDay
    , regex "пополудни"
    ]
  , prod = \tokens -> case tokens of
      (Token Time td:_) -> do
        tt $ timeOfDayAMPM False td
      _ -> Nothing
  }

ruleAfterMidnight :: Rule
ruleAfterMidnight = Rule
  { name = "<time-of-day> пополудни"
  , pattern =
    [ Predicate isATimeOfDay
    , regex "(пополуночи|после полудня)"
    ]
  , prod = \tokens -> case tokens of
      (Token Time td:_) -> do
        tt $ timeOfDayAMPM True td
      _ -> Nothing
  }

rule1Hour :: Rule
rule1Hour = Rule
  { name = "в час"
  , pattern =
    [ regex "(в )?час" -- todo make latent if "в" is absent?
    ]
  , prod = \tokens -> case tokens of
      _ -> tt $ hour True 1
  }

ruleMidnight :: Rule
ruleMidnight = Rule
  { name = "полночь"
  , pattern =
    [ regex "(в )?полноч."
    ]
  , prod = \tokens -> case tokens of
      _ -> tt $ hour False 0
  }

ruleHourPartOfDays :: Rule
ruleHourPartOfDays = Rule
  { name = "<time-of-day> <part-of-day>"
  , pattern = -- todo add "в"? is "час" processed here?  todo hour process "в 15 дня"?
    [ Predicate isATimeOfDay
    , regex "(утра|дня|вечера|ночи)"
    ]
  , prod = \tokens -> case tokens of
      (Token Time td@TimeData{TTime.form = Just (TTime.TimeOfDay (Just hours) is12H)}
       :Token RegexMatch (GroupMatch (match:_)):_) ->
        case is12H of
          True -> do
            let isAMM = case Text.toLower match of
                  "утра"   -> case hours of
                                12         -> Just False
                                x | 3 <= x -> Just True
                                _          -> Nothing
                  "дня"    -> case hours of
                                x | x <= 7 -> Just False
                                12         -> Just False
                                _          -> Nothing
                  "вечера" -> case hours of
                                12         -> Just True
                                1          -> Nothing
                                _          -> Just False
                  "ночи"   -> case hours of
                                x | x <= 4 -> Just True
                                12         -> Just True
                                x | 8 <= x -> Just False
                                _          -> Nothing
                  _        -> Nothing
            case isAMM of
              Just isAM -> tt $ timeOfDayAMPM isAM td
              _ -> Nothing
          False -> do
            let tdMaybe = case Text.toLower match of
                  "утра"   -> case hours of
                                x | 12 < x -> Nothing
                                _          -> Just td
                  "дня"    -> case hours of
                                x | x < 10 -> Nothing
                                x | x > 17 -> Nothing
                                _          -> Just td
                  "вечера" -> case hours of
                                x | x < 15 -> Nothing
                                _          -> Just td
                  "ночи"   -> case hours of
                                x | 19 < x -> Just td
                                x | x <= 6 -> Just td
                                _          -> Nothing
                  _        -> Nothing
            case tdMaybe of
              Just td -> tt $ td
              _       -> Nothing
      _ -> Nothing
  }


rulePartOfDays :: Rule
rulePartOfDays = Rule
  { name = "part-of-days-1"
  , pattern =
    [ regex "(утро|день|вечер|ночь)"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):_) -> do
        (start, end) <- case Text.toLower match of
              "утро"  -> Just (hour False 0, hour False 9)
              "день"  -> Just (hour False 12, hour False 18)
              "вечер" -> Just (hour False 18, hour False 22)
              "ночь" -> Just (hour False 22, hour False 00)
              _       -> Nothing
        Token Time . partOfDay . mkLatent <$> interval TTime.Open start end
      _ -> Nothing
  }

rulePartOfDays2 :: Rule
rulePartOfDays2 = Rule
  { name = "part-of-days-2"
  , pattern =
    [ regex "(утром|днем|днём|вечером|ночью)"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):_) -> do
        (start, end) <- case Text.toLower match of
              "утром"   -> Just (hour False 0, hour False 9)
              "днем"    -> Just (hour False 12, hour False 18)
              "днём"    -> Just (hour False 12, hour False 18)
              "вечером" -> Just (hour False 18, hour False 22)
              "ночью"   -> Just (hour False 22, hour False 00)
              _         -> Nothing
        Token Time . partOfDay . notLatent <$> interval TTime.Open start end
      _ -> Nothing
  }

ruleEndOfDay :: Rule
ruleEndOfDay = Rule
  { name = "end-of-day"
  , pattern =
    [ regex "(конец|конца|концу|конце) дня"
    ]
  , prod = \case 
      _ -> tt . hour False $ 0
  }

ruleEarlyMorning :: Rule
ruleEarlyMorning = Rule
  { name = "рано утром"
  , pattern =
    [ regex "ран(о|не..?) утром?"
    ]
  , prod = \_ -> Token Time . partOfDay . mkLatent <$>
      interval TTime.Open (hour False 4) (hour False 9)
  }

rulePODIn :: Rule
rulePODIn = Rule
  { name = "in|during the <part-of-day>"
  , pattern =
    [ regex "(in|during)( the)?"
    , Predicate isAPartOfDay
    ]
  , prod = \tokens -> case tokens of
      (_:Token Time td:_) -> tt $ notLatent td
      _ -> Nothing
  }

rulePODThis :: Rule
rulePODThis = Rule
  { name = "this <part-of-day>"
  , pattern =
    [ regex "this"
    , Predicate isAPartOfDay
    ]
  , prod = \tokens -> case tokens of
      (_:Token Time td:_) -> Token Time . partOfDay . notLatent <$>
        intersect today td
      _ -> Nothing
  }

ruleTonight :: Rule
ruleTonight = Rule
  { name = "tonight"
  , pattern = [regex "(late )?toni(ght|gth|te)s?"]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):_) -> do
        let h = if Text.toLower match == "late " then 21 else 18
        evening <- interval TTime.Open (hour False h) (hour False 0)
        Token Time . partOfDay . notLatent <$> intersect today evening
      _ -> Nothing
  }

ruleAfterPartofday :: Rule
ruleAfterPartofday = Rule
  { name = "after lunch/work/school"
  , pattern =
    [ regex "after[\\s-]?(lunch|work|school)"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):_) -> do
        (start, end) <- case Text.toLower match of
          "lunch"  -> Just (hour False 13, hour False 17)
          "work"   -> Just (hour False 17, hour False 21)
          "school" -> Just (hour False 15, hour False 21)
          _        -> Nothing
        td <- interval TTime.Open start end
        Token Time . partOfDay . notLatent <$> intersect today td
      _ -> Nothing
  }

-- Since part of days are latent, general time intersection is blocked
ruleTimePOD :: Rule
ruleTimePOD = Rule
  { name = "<time> <part-of-day>"
  , pattern =
    [ dimension Time
    , Predicate isAPartOfDay
    ]
  , prod = \tokens -> case tokens of
      (Token Time td:Token Time pod:_) -> Token Time <$> intersect pod td
      _ -> Nothing
  }

rulePODofTime :: Rule
rulePODofTime = Rule
  { name = "<part-of-day> of <time>"
  , pattern =
    [ Predicate isAPartOfDay
    , regex "of"
    , dimension Time
    ]
  , prod = \tokens -> case tokens of
      (Token Time pod:_:Token Time td:_) -> Token Time <$> intersect pod td
      _ -> Nothing
  }

ruleWeekend :: Rule
ruleWeekend = Rule
  { name = "выходные"
  , pattern =
    [ regex "((на|в)\\s)?выходн..|уикенд.?|викенд.?|уикэнд.?|викэнд.?"
    ]
  , prod = \_ -> tt $ mkOkForThisNext weekend
  }

ruleWeek :: Rule
ruleWeek = Rule
 { name = "week"
 , pattern = [regex "(all|rest of the) week"]
 , prod = \case
     (Token RegexMatch (GroupMatch (match:_)):_) ->
       let end = cycleNthAfter True TG.Day (-2) $ cycleNth TG.Week 1
           period = case Text.toLower match of
                      "all" -> interval Closed (cycleNth TG.Week 0) end
                      "rest of the" -> interval Open today end
                      _ -> Nothing
       in Token Time <$> period
     _ -> Nothing
 }

ruleSeason :: Rule
ruleSeason = Rule
  { name = "last|this|next <season>"
  , pattern =
    [ regex "(this|current|next|last|past|previous) seasons?"
    ]
  , prod = \case
      (Token RegexMatch (GroupMatch (match:_)):_) -> do
        n <- case Text.toLower match of
               "this" -> Just 0
               "current" -> Just 0
               "last" -> Just (-1)
               "past" -> Just (-1)
               "previous" -> Just (-1)
               "next" -> Just 1
               _ -> Nothing
        tt $ predNth n False season
      _ -> Nothing
  }

ruleSeasons :: [Rule]
ruleSeasons = mkRuleSeasons
  [ ( "summer", "summer"     , monthDay  6 21, monthDay  9 23 )
  , ( "fall"  , "fall|autumn", monthDay  9 23, monthDay 12 21 )
  , ( "winter", "winter"     , monthDay 12 21, monthDay  3 20 )
  , ( "spring", "spring"     , monthDay  3 20, monthDay  6 21 )
  ]

ruleTODPrecision :: Rule
ruleTODPrecision = Rule
  { name = "<time-of-day> sharp|exactly"
  , pattern =
    [ Predicate isATimeOfDay
    , regex "(sharp|exactly|-?ish|approximately)"
    ]
  , prod = \tokens -> case tokens of
      (Token Time td:_) -> tt $ notLatent td
      _ -> Nothing
  }

rulePrecisionTOD :: Rule
rulePrecisionTOD = Rule
  { name = "ровно|точно <time-of-day>"
  , pattern =
    [ regex "(ровно|точно|примерно|ориентировочно)"
    , Predicate $ isGrainFinerThan TG.Year
    ]
  , prod = \tokens -> case tokens of
      (_:Token Time td:_) -> tt $ notLatent td
      _ -> Nothing
  }

ruleIntervalMonthDDDD :: Rule
ruleIntervalMonthDDDD = Rule
  { name = "<month> dd-dd (interval)"
  , pattern =
    [ Predicate isAMonth
    , Predicate isDOMValue
    , regex "\\-|to|th?ru|through|(un)?til(l)?"
    , Predicate isDOMValue
    ]
  , prod = \tokens -> case tokens of
      (Token Time td:
       token1:
       _:
       token2:
       _) -> do
        dom1 <- intersectDOM td token1
        dom2 <- intersectDOM td token2
        Token Time <$> interval TTime.Closed dom1 dom2
      _ -> Nothing
  }

ruleIntervalDDDDMonth :: Rule
ruleIntervalDDDDMonth = Rule
  { name = "dd-dd <month> (interval)"
  , pattern =
    [ Predicate isDOMValue
    , regex "\\-|to|th?ru|through|(un)?til(l)?"
    , Predicate isDOMValue
    , Predicate isAMonth
    ]
  , prod = \tokens -> case tokens of
      (token1:
       _:
       token2:
       Token Time td:
       _) -> do
        dom1 <- intersectDOM td token1
        dom2 <- intersectDOM td token2
        Token Time <$> interval TTime.Closed dom1 dom2
      _ -> Nothing
  }

ruleIntervalFromMonthDDDD :: Rule
ruleIntervalFromMonthDDDD = Rule
  { name = "from <month> dd-dd (interval)"
  , pattern =
    [ regex "from"
    , Predicate isAMonth
    , Predicate isDOMValue
    , regex "\\-|to|th?ru|through|(un)?til(l)?"
    , Predicate isDOMValue
    ]
  , prod = \tokens -> case tokens of
      (_:
       Token Time td:
       token1:
       _:
       token2:
       _) -> do
        dom1 <- intersectDOM td token1
        dom2 <- intersectDOM td token2
        Token Time <$> interval TTime.Closed dom1 dom2
      _ -> Nothing
  }

ruleIntervalFromDDDDMonth :: Rule
ruleIntervalFromDDDDMonth = Rule
  { name = "с <day-of-month> (ordinal or number) до <day-of-month> (ordinal or number) <named-month> (interval)"
  , pattern =
    [ regex "(|начиная\\s)с"
    , Predicate isDOMValue
    , regex "(и\\s)?(по|до)"
    , Predicate isDOMValue
    , Predicate isAMonth
    ]
  , prod = \tokens -> case tokens of
      (_:
       token1:
       _:
       token2:
       Token Time td:
       _) -> do
        dom1 <- intersectDOM td token1
        dom2 <- intersectDOM td token2
        Token Time <$> interval TTime.Closed dom1 dom2
      _ -> Nothing
  }

-- Blocked for :latent time. May need to accept certain latents only, like hours
ruleIntervalDash :: Rule
ruleIntervalDash = Rule
  { name = "<datetime> - <datetime> (interval)"
  , pattern =
    [ Predicate isNotLatent
    , regex "\\-|to|th?ru|through|(un)?til(l)?"
    , Predicate isNotLatent
    ]
  , prod = \tokens -> case tokens of
      (Token Time td1:_:Token Time td2:_) ->
        Token Time <$> interval TTime.Closed td1 td2
      _ -> Nothing
  }

ruleIntervalSlash :: Rule
ruleIntervalSlash = Rule
  { name = "<datetime>/<datetime> (interval)"
  , pattern =
    [ Predicate isNotLatent
    , regex "/"
    , Predicate isNotLatent
    ]
  , prod = \tokens -> case tokens of
      (Token Time td1:_:Token Time td2:_) ->
        if sameGrain td1 td2 then
          Token Time <$> interval TTime.Closed td1 td2
        else Nothing
      _ -> Nothing
  }

ruleIntervalFromTo :: Rule
ruleIntervalFromTo = Rule
  { name = "c <datetime> по <datetime> (interval)"
  , pattern =
    [ regex "(|начиная\\s)с"
    , dimension Time
    , regex "(и\\s)?(по|до)"
    , dimension Time
    ]
  , prod = \tokens -> case tokens of
      (_:Token Time td1:_:Token Time td2:_) ->
          Token Time <$> interval TTime.Closed td1 td2
      _ -> Nothing
  }

ruleIntervalDOMFromTo :: Rule
ruleIntervalDOMFromTo = Rule
  { name = "с <day-of-month> по <day-of-month> (interval)"
  , pattern =
    [ regex "(|начиная\\s)с"
    , Predicate isDOMValue
    , regex "(и\\s)?по"
    , Predicate isDOMValue
    ]
  , prod = \tokens -> case tokens of
      (_:token1:_:token2:_) -> do
        n1 <- getIntValue token1
        n2 <- getIntValue token2
        Token Time <$> interval TTime.Closed (dayOfMonth n1) (dayOfMonth n2)
      _ -> Nothing
  }

ruleIntervalTODFromTo :: Rule
ruleIntervalTODFromTo = Rule
  { name = "с <time-of-day> до <time-of-day> (interval)"
  , pattern =
    [ regex "(|начиная\\s)с"
    , Predicate isATimeOfDay
    , regex "(и\\s)?до"
    , Predicate isATimeOfDay
    ]
  , prod = \tokens -> case tokens of
      (_:
       Token Time td1@TimeData{TTime.form = Just (TTime.TimeOfDay (Just h1) is12H1)}:
       _:
       Token Time td2@TimeData{TTime.form = Just (TTime.TimeOfDay (Just h2) is12H2)}:
       _) ->
        if is12H1 == is12H2 then
          Token Time <$> interval TTime.Closed td1 td2
        else
          Token Time <$> interval TTime.Closed (hour False h1) (hour False h2)
      _ -> Nothing
  }

ruleIntervalBy :: Rule
ruleIntervalBy = Rule
  { name = "by <time>"
  , pattern =
    [ regex "к"
    , dimension Time
    ]
  , prod = \tokens -> case tokens of
      (_:Token Time td:_) -> Token Time <$> interval TTime.Open now td
      _ -> Nothing
  }

ruleIntervalByHours :: Rule
ruleIntervalByHours = Rule
  { name = "by <time>"
  , pattern =
    [ regex "к"
    , dimension Time
    , regex "часам"
    ]
  , prod = \tokens -> case tokens of
      (_:Token Time td:_) -> Token Time <$> interval TTime.Open now td
      _ -> Nothing
  }

ruleIntervalByTheEndOf :: Rule
ruleIntervalByTheEndOf = Rule
  { name = "by the end of <time>"
  , pattern =
    [ regex "by (the )?end of"
    , dimension Time
    ]
  , prod = \tokens -> case tokens of
      (_:Token Time td:_) -> Token Time <$> interval TTime.Closed now td
      _ -> Nothing
  }

ruleIntervalUntilTime :: Rule
ruleIntervalUntilTime = Rule
  { name = "until <time>"
  , pattern =
    [ regex "до"
    , dimension Time
    ]
  , prod = \tokens -> case tokens of
      (_:Token Time td:_) -> tt . withDirection TTime.Before $ notLatent td
      _ -> Nothing
  }

ruleIntervalUntilDOM :: Rule
ruleIntervalUntilDOM = Rule
  { name = "until <day-of-month>"
  , pattern =
    [ regex "по"
    , Predicate $ isGrainOfTime TG.Day
    ]
  , prod = \tokens -> case tokens of
      (_:Token Time td:_) -> tt . withDirection TTime.Before $ notLatent td
      _ -> Nothing
  }

ruleIntervalUntilDOMOrdinal :: Rule
ruleIntervalUntilDOMOrdinal = Rule
  { name = "until <day-of-month> (ordinal)"
  , pattern =
    [ regex "по"
    , Predicate isDOMValue
    ]
  , prod = \tokens -> case tokens of
      (_:token:_) -> do
        n <- getIntValue token
        tt . withDirection TTime.Before $ dayOfMonth n
      _ -> Nothing
  }

ruleIntervalAfterFromSinceTime :: Rule
ruleIntervalAfterFromSinceTime = Rule
  { name = "с|после <time>"
  , pattern =
    [ regex "со?|после"
    , dimension Time
    ]
  , prod = \tokens -> case tokens of
      (_:Token Time td:_) -> tt . withDirection TTime.After $ notLatent td
      _ -> Nothing
  }

ruleDaysOfWeek :: [Rule]
ruleDaysOfWeek = mkRuleDaysOfWeek
  [ ( "Monday"   , "понедельник[ауе]?|пн\\.?"  )
  , ( "Tuesday"  , "вторник[ауе]?|вт\\.?"      )
  , ( "Wednesday", "сред[аыеу]|ср\\.?"           )
  , ( "Thursday" , "четверг[ауе]?|чт\\.?"      )
  , ( "Friday"   , "пятниц[аыеу]|пт\\.?"         )
  , ( "Saturday" , "суббот[аыеу]|сб\\.?"         )
  , ( "Sunday"   , "воскресень[еяю]|вс\\.?"      )
  ]

ruleMonths :: [Rule]
ruleMonths = mkRuleMonths
  [ ( "January"  , "январ[ьяюе]|янв\\.?"      )
  , ( "February" , "феврал[ьяюе]|фев\\.?"     )
  , ( "March"    , "март[аяе]?"                )
  , ( "April"    , "апрел[ьяюе]|апр\\.?"      )
  , ( "May"      , "ма[йея]"                  )
  , ( "June"     , "июн[ьяюе]"                )
  , ( "July"     , "июл[ьяюе]"                )
  , ( "August"   , "август[ауе]?|авг\\.?"      )
  , ( "September", "сентябр[ьяюе]|сент?\\.?"  )
  , ( "October"  , "октябр[ьяюе]|окт\\.?"     )
  , ( "November" , "ноябр[ьяюе]|нояб\\.?"     )
  , ( "December" , "декабр[ьяюе]|дек\\.?"     )
  ]

rulePartOfMonth :: Rule
rulePartOfMonth = Rule
  { name = "part of <named-month>"
  , pattern =
    [ regex "(early|mid|late)-?( of)?"
    , Predicate isAMonth
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):Token Time td:_) -> do
        (sd, ed) <- case Text.toLower match of
          "early" -> Just (1, 10)
          "mid"   -> Just (11, 20)
          "late"  -> Just (21, -1)
          _       -> Nothing
        start <- intersect td $ dayOfMonth sd
        end <- if ed /= -1
          then intersect td $ dayOfMonth ed
          else Just $ cycleLastOf TG.Day td
        Token Time <$> interval TTime.Open start end
      _ -> Nothing
  }

ruleEndOrBeginningOfMonth :: Rule
ruleEndOrBeginningOfMonth = Rule
  { name = "at the beginning|end of <named-month>"
  , pattern =
    [ regex "(at the )?(beginning|end) of"
    , Predicate isAMonth
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (_:match:_)):Token Time td:_) -> do
        (sd, ed) <- case Text.toLower match of
          "beginning" -> Just (1, 10)
          "end"       -> Just (21, -1)
          _           -> Nothing
        start <- intersect td $ dayOfMonth sd
        end <- if ed /= -1
          then intersect td $ dayOfMonth ed
          else Just $ cycleLastOf TG.Day td
        Token Time <$> interval TTime.Open start end
      _ -> Nothing
  }

ruleEndOfMonth :: Rule
ruleEndOfMonth = Rule
  { name = "end of month"
  , pattern = [ regex "(by (the )?|(at )?the )?(EOM|end of (the )?month)" ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):_)
        | (Just start, Just end) <- parsed ->
          Token Time <$> interval TTime.Open start end
        where
          cycleMonth = cycleNth TG.Month
          parsed = if "by" `Text.isPrefixOf` Text.toLower match
            then
              ( Just now
              , intersect (dayOfMonth 1) $ cycleMonth 1)
            else
              ( intersect (dayOfMonth 21) $ cycleMonth 0
              , Just $ cycleLastOf TG.Day $ cycleMonth 0)
      _ -> Nothing
  }

ruleBeginningOfMonth :: Rule
ruleBeginningOfMonth = Rule
  { name = "beginning of month"
  , pattern = [ regex "((at )?the )?(BOM|beginning of (the )?month)" ]
  , prod = \_ -> do
      start <- intersect (dayOfMonth 1) $ cycleNth TG.Month 0
      end <- intersect (dayOfMonth 10) $ cycleNth TG.Month 0
      Token Time <$> interval TTime.Open start end
  }

ruleEndOrBeginningOfYear :: Rule
ruleEndOrBeginningOfYear = Rule
  { name = "at the beginning|end of <year>"
  , pattern =
    [ regex "(at the )?(beginning|end) of"
    , Predicate $ isGrainOfTime TG.Year
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (_:match:_)):Token Time td:_) -> do
        (sd, ed) <- case Text.toLower match of
          "beginning" -> Just (1, 4)
          "end"       -> Just (9, -1)
          _           -> Nothing
        start <- intersect td $ month sd
        end <- if ed /= -1
          then intersect td $ cycleLastOf TG.Month $ month ed
          else cycleNthAfter False TG.Year 1 <$> intersect td (month 1)
        Token Time <$> interval TTime.Open start end
      _ -> Nothing
  }

ruleEndOfYear :: Rule
ruleEndOfYear = Rule
  { name = "end of year"
  , pattern = [ regex "(by (the )?|(at )?the )?(EOY|end of (the )?year)" ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):_) -> do
        start <- std
        end <- intersect (month 1) $ cycleYear 1
        Token Time <$> interval TTime.Open start end
          where
            std = if "by" `Text.isPrefixOf` Text.toLower match
              then Just now
              else intersect (month 9) $ cycleYear 0
            cycleYear = cycleNth TG.Year
      _ -> Nothing
  }

ruleBeginningOfYear :: Rule
ruleBeginningOfYear = Rule
  { name = "beginning of year"
  , pattern = [ regex "((at )?the )?(BOY|beginning of (the )?year)" ]
  , prod = \_ -> do
      start <- intersect (month 1) $ cycleNth TG.Year 0
      end <- intersect (month 4) $ cycleNth TG.Year 0
      Token Time <$> interval TTime.Open start end
  }

ruleEndOrBeginningOfWeek :: Rule
ruleEndOrBeginningOfWeek = Rule
  { name = "at the beginning|end of <week>"
  , pattern =
    [ regex "(at the )?(beginning|end) of"
    , Predicate $ isGrainOfTime TG.Week
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (_:match1:_)):Token Time td:_) -> do
        (sd, ed) <- case Text.toLower match1 of
          "beginning" -> Just (1, 3)
          "end"       -> Just (5, 7)
          _           -> Nothing
        start <- intersect td $ dayOfWeek sd
        end <- intersect td $ dayOfWeek ed
        Token Time <$> interval TTime.Open start end
      _ -> Nothing
  }

rulePeriodicHolidays :: [Rule]
rulePeriodicHolidays = mkRuleHolidays
  [ ("Новый год", "нов(ый|ого|ому|ом|ым) год.?", monthDay 1 1 )
  , ("Рождество", "рождеств.(\\sхристов.)?", monthDay 1 7)
  , ("День защитника отечества", "(дн.|день) защитника отечества", monthDay 2 23)
  , ("международный женский день", "международн...? женск...? (дн.|день)", monthDay 3 8)
  , ("Праздник Весны и Труда", "праздник.? весны и труда", monthDay 5 1)
  , ("День Победы", "(дн.|день) победы", monthDay 5 9)
  , ("День России", "(дн.|день) россии", monthDay 6 12)
  , ("День народного единств", "(дн.|день) народного единств.?", monthDay 11 4)
  ]


ruleCycleThis :: Rule
ruleCycleThis = Rule
  { name = "этот <cycle>"
  , pattern =
    [ regex "эт(а|у|о[тйм])"
    , dimension TimeGrain
    ]
  , prod = \tokens -> case tokens of
      (_:Token TimeGrain grain:_) ->
        tt $ cycleNth grain 0
      _ -> Nothing
  }

ruleCycleThis2LastNext :: Rule
ruleCycleThis2LastNext = Rule
  { name = "текущий|следующий|прошлый <cycle>"
  , pattern =
    [ regex "(текущ|ближайш|предстоящ|будущ|следующ|грядущ|прошл|прошедш|предыдущ)...?"
    , dimension TimeGrain
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):Token TimeGrain grain:_) ->
        case Text.toLower match of
          "текущ"     -> tt $ cycleNth grain 0
          "ближайш"   -> tt $ cycleNth grain 0
          "предстоящ" -> tt $ cycleNth grain 1
          "будущ"     -> tt $ cycleNth grain 1
          "следующ"   -> tt $ cycleNth grain 1
          "грядущ"    -> tt $ cycleNth grain 1
          "прошл"     -> tt . cycleNth grain $ - 1
          "прошедш"   -> tt . cycleNth grain $ - 1
          "предыдущ"  -> tt . cycleNth grain $ - 1
          _ -> Nothing
      _ -> Nothing
  }

ruleDOMOfTimeMonth :: Rule
ruleDOMOfTimeMonth = Rule
  { name = "<day-of-month> (ordinal or number) of <month>"
  , pattern =
    [ Predicate isDOMValue
    , regex "of( the)?"
    , Predicate $ isGrainOfTime TG.Month
    ]
  , prod = \tokens -> case tokens of
      (token:_:Token Time td:_) -> Token Time <$> intersectDOM td token
      _ -> Nothing
  }

ruleCycleTheAfterBeforeTime :: Rule
ruleCycleTheAfterBeforeTime = Rule
  { name = "the <cycle> after|before <time>"
  , pattern =
    [ regex "the"
    , dimension TimeGrain
    , regex "(after|before)"
    , dimension Time
    ]
  , prod = \tokens -> case tokens of
      (  _
       : Token TimeGrain grain
       : Token RegexMatch (GroupMatch (match:_))
       : Token Time td
       : _) ->
        let n = if Text.toLower match == "after" then 1 else - 1 in
          tt $ cycleNthAfter False grain n td
      _ -> Nothing
  }

-- e.g. "минут через 10", "лет через 5"
ruleCycleAfterCount :: Rule
ruleCycleAfterCount = Rule
  { name = "<cycle> через <count>"
  , pattern =
    [ dimension TimeGrain
    , regex "через"
    , Predicate isNatural
    ]
  , prod = \tokens -> case tokens of
      (Token TimeGrain grain:
       _:
       Token Numeral NumeralData{TNumeral.value = v}:
       _) -> 
        tt $ cycleNth grain $ floor v
      _ -> Nothing
  }

ruleCycleOrdinalOfTime :: Rule
ruleCycleOrdinalOfTime = Rule
  { name = "<ordinal> <cycle> of <time>"
  , pattern =
    [ dimension Ordinal
    , dimension TimeGrain
    , regex "of|in|from"
    , dimension Time
    ]
  , prod = \tokens -> case tokens of
      (token:Token TimeGrain grain:_:Token Time td:_) -> do
        n <- getIntValue token
        tt $ cycleNthAfter True grain (n - 1) td
      _ -> Nothing
  }

ruleCycleLastOrdinalOfTime :: Rule
ruleCycleLastOrdinalOfTime = Rule
  { name = "<ordinal> last <cycle> of <time>"
  , pattern =
    [ dimension Ordinal
    , regex "last"
    , dimension TimeGrain
    , regex "of|in|from"
    , dimension Time
    ]
  , prod = \tokens -> case tokens of
      (token:_:Token TimeGrain grain:_:Token Time td:_) -> do
        n <- getIntValue token
        tt . cycleNthAfter True grain (-n) . cycleNthAfter True (timeGrain td) 1 $ td
      _ -> Nothing
  }

ruleCycleTheOrdinalOfTime :: Rule
ruleCycleTheOrdinalOfTime = Rule
  { name = "the <ordinal> <cycle> of <time>"
  , pattern =
    [ regex "the"
    , dimension Ordinal
    , dimension TimeGrain
    , regex "of|in|from"
    , dimension Time
    ]
  , prod = \tokens -> case tokens of
      (_:token:Token TimeGrain grain:_:Token Time td:_) -> do
        n <- getIntValue token
        tt $ cycleNthAfter True grain (n - 1) td
      _ -> Nothing
  }

ruleCycleTheLastOrdinalOfTime :: Rule
ruleCycleTheLastOrdinalOfTime = Rule
  { name = "the <ordinal> last <cycle> of <time>"
  , pattern =
    [ regex "the"
    , dimension Ordinal
    , regex "last"
    , dimension TimeGrain
    , regex "of|in|from"
    , dimension Time
    ]
  , prod = \tokens -> case tokens of
      (_:token:_:Token TimeGrain grain:_:Token Time td:_) -> do
        n <- getIntValue token
        tt . cycleNthAfter True grain (-n) . cycleNthAfter True (timeGrain td) 1 $ td
      _ -> Nothing
  }

ruleCycleTheOfTime :: Rule
ruleCycleTheOfTime = Rule
  { name = "the <cycle> of <time>"
  , pattern =
    [ regex "the"
    , dimension TimeGrain
    , regex "of"
    , dimension Time
    ]
  , prod = \tokens -> case tokens of
      (_:Token TimeGrain grain:_:Token Time td:_) ->
        tt $ cycleNthAfter True grain 0 td
      _ -> Nothing
  }

ruleCycleOrdinalAfterTime :: Rule
ruleCycleOrdinalAfterTime = Rule
  { name = "<ordinal> <cycle> after <time>"
  , pattern =
    [ dimension Ordinal
    , dimension TimeGrain
    , regex "after"
    , dimension Time
    ]
  , prod = \tokens -> case tokens of
      (token:Token TimeGrain grain:_:Token Time td:_) -> do
        n <- getIntValue token
        tt $ cycleNthAfter True grain (n - 1) td
      _ -> Nothing
  }

ruleCycleTheOrdinalAfterTime :: Rule
ruleCycleTheOrdinalAfterTime = Rule
  { name = "<ordinal> <cycle> after <time>"
  , pattern =
    [ regex "the"
    , dimension Ordinal
    , dimension TimeGrain
    , regex "after"
    , dimension Time
    ]
  , prod = \tokens -> case tokens of
      (_:token:Token TimeGrain grain:_:Token Time td:_) -> do
        n <- getIntValue token
        tt $ cycleNthAfter True grain (n - 1) td
      _ -> Nothing
  }

ruleCycleOrdinalQuarter :: Rule
ruleCycleOrdinalQuarter = Rule
  { name = "<ordinal> quarter"
  , pattern =
    [ dimension Ordinal
    , Predicate $ isGrain TG.Quarter
    ]
  , prod = \tokens -> case tokens of
      (token:_) -> do
        n <- getIntValue token
        tt . cycleNthAfter True TG.Quarter (n - 1) $
          cycleNth TG.Year 0
      _ -> Nothing
  }

ruleCycleTheOrdinalQuarter :: Rule
ruleCycleTheOrdinalQuarter = Rule
  { name = "the <ordinal> quarter"
  , pattern =
    [ regex "the"
    , dimension Ordinal
    , Predicate $ isGrain TG.Quarter
    ]
  , prod = \tokens -> case tokens of
      (_:token:_) -> do
        n <- getIntValue token
        tt . cycleNthAfter True TG.Quarter (n - 1) $
          cycleNth TG.Year 0
      _ -> Nothing
  }

ruleCycleOrdinalQuarterYear :: Rule
ruleCycleOrdinalQuarterYear = Rule
  { name = "<ordinal> quarter <year>"
  , pattern =
    [ dimension Ordinal
    , Predicate $ isGrain TG.Quarter
    , dimension Time
    ]
  , prod = \tokens -> case tokens of
      (token:_:Token Time td:_) -> do
        n <- getIntValue token
        tt $ cycleNthAfter False TG.Quarter (n - 1) td
      _ -> Nothing
  }

ruleDurationInWithinAfter :: Rule
ruleDurationInWithinAfter = Rule
  { name = "через|в течение|после <duration>"
  , pattern =
    [ regex "(через|в течение|после)"
    , dimension Duration
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):
       Token Duration dd@DurationData{TDuration.grain, TDuration.value}:
       _) -> tt $ inDuration dd
      _ -> Nothing
  }

ruleDurationLastNext :: Rule
ruleDurationLastNext = Rule
  { name = "last|past|next <duration>"
  , pattern =
    [ regex "([lp]ast|next)"
    , dimension Duration
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):
       Token Duration DurationData{TDuration.grain, TDuration.value}:
       _) -> case Text.toLower match of
         "next" -> tt $ cycleN True grain value
         "last" -> tt $ cycleN True grain (- value)
         "past" -> tt $ cycleN True grain (- value)
         _      -> Nothing
      _ -> Nothing
  }

ruleNDOWago :: Rule
ruleNDOWago = Rule
  { name = "<integer> <named-day> ago|back"
  , pattern =
    [ Predicate isNatural
    , Predicate isADayOfWeek
    , regex "ago|back"
    ]
  , prod = \tokens -> case tokens of
      (Token Numeral NumeralData{TNumeral.value = v}:Token Time td:_) ->
        tt $ predNth (- (floor v)) False td
      _ -> Nothing
  }

ruleDurationHenceAgo :: Rule
ruleDurationHenceAgo = Rule
  { name = "<duration> спустя|назад"
  , pattern =
    [ dimension Duration
    , regex "(спустя|назад)"
    ]
  , prod = \tokens -> case tokens of
      (Token Duration dd:
       Token RegexMatch (GroupMatch (match:_)):
       _) -> case Text.toLower match of
        "назад" -> tt $ durationAgo dd
        _       -> tt $ inDuration dd
      _ -> Nothing
  }

ruleDurationHence :: Rule
ruleDurationHence = Rule
  { name = "спустя <duration>"
  , pattern =
    [ regex "спустя"
    , dimension Duration
    ]
  , prod = \tokens -> case tokens of
      (_:Token Duration dd:_) -> tt $ inDuration dd
      _ -> Nothing
  }

ruleDayDurationHenceAgo :: Rule
ruleDayDurationHenceAgo = Rule
  { name = "<day> <duration> hence|ago"
  , pattern =
    [ Predicate $ or . sequence [isGrainOfTime TG.Day, isGrainOfTime TG.Month]
    , dimension Duration
    , regex "(from now|hence|ago)"
    ]
  , prod = \tokens -> case tokens of
      (Token Time td:
       Token Duration dd:
       Token RegexMatch (GroupMatch (match:_)):
       _) -> case Text.toLower match of
         "ago" -> Token Time <$> intersect td (durationIntervalAgo dd)
         _     -> Token Time <$> intersect td (inDurationInterval dd)
      _ -> Nothing
  }

ruleDayInDuration :: Rule
ruleDayInDuration = Rule
  { name = "<day> in <duration>"
  , pattern =
    [ Predicate $ or . sequence [isGrainOfTime TG.Day, isGrainOfTime TG.Month]
    , regex "in"
    , dimension Duration
    ]
  , prod = \tokens -> case tokens of
      (Token Time td:_:Token Duration dd:_) ->
        Token Time <$> intersect td (inDurationInterval dd)
      _ -> Nothing
  }

ruleInDurationAtTime :: Rule
ruleInDurationAtTime = Rule
  { name = "in <duration> at <time-of-day>"
  , pattern =
    [ regex "in"
    , Predicate $ isDurationGreaterThan TG.Hour
    , regex "at"
    , Predicate isATimeOfDay
    ]
  , prod = \tokens -> case tokens of
      (_:Token Duration dd:_:Token Time td:_) ->
        Token Time <$> intersect td (inDurationInterval dd)
      _ -> Nothing
  }

ruleInNumeral :: Rule
ruleInNumeral = Rule
  { name = "in <number> (implicit minutes)"
  , pattern =
    [ regex "in"
    , Predicate $ isIntegerBetween 0 60
    ]
  , prod = \tokens -> case tokens of
      (_:Token Numeral NumeralData{TNumeral.value = v}:_) ->
        tt . inDuration . duration TG.Minute $ floor v
      _ -> Nothing
  }

ruleDurationAfterBeforeTime :: Rule
ruleDurationAfterBeforeTime = Rule
  { name = "<duration> after|before|from <time>"
  , pattern =
    [ dimension Duration
    , regex "(after|before|from)"
    , dimension Time
    ]
  , prod = \tokens -> case tokens of
      (Token Duration dd:
       Token RegexMatch (GroupMatch (match:_)):
       Token Time td:
       _) -> case Text.toLower match of
         "before" -> tt $ durationBefore dd td
         _        -> tt $ durationAfter dd td
      _ -> Nothing
  }

ruleIntervalForDurationFrom :: Rule
ruleIntervalForDurationFrom = Rule
  { name = "for <duration> from <time>"
  , pattern =
    [ regex "for"
    , dimension Duration
    , regex "(from|starting|beginning|after|starting from)"
    , dimension Time
    ]
  , prod = \tokens -> case tokens of
      (_:Token Duration dd:_:Token Time td1:_) ->
        Token Time <$> interval TTime.Closed td1 (durationAfter dd td1)
      _ -> Nothing
}

ruleIntervalTimeForDuration :: Rule
ruleIntervalTimeForDuration = Rule
  { name = "<time> for <duration>"
  , pattern =
    [ Predicate isNotLatent
    , regex "for"
    , dimension Duration
    ]
  , prod = \tokens -> case tokens of
      (Token Time td1:_:Token Duration dd:_) ->
        Token Time <$> interval TTime.Closed td1 (durationAfter dd td1)
      _ -> Nothing
}

ruleIntervalFromTimeForDuration :: Rule
ruleIntervalFromTimeForDuration = Rule
  { name = "from <time> for <duration>"
  , pattern =
    [ regex "(from|starting|beginning|after|starting from)"
    , Predicate isNotLatent
    , regex "for"
    , dimension Duration
    ]
  , prod = \tokens -> case tokens of
      (_:Token Time td1:_:Token Duration dd:_) ->
        Token Time <$> interval TTime.Closed td1 (durationAfter dd td1)
      _ -> Nothing
}

timezoneName :: String
timezoneName = "YEKT|YEKST|YAKT|YAKST|WITA|WIT|WIB|WGT|WGST|WFT|WET|WEST|WAT|WAST|VUT|VLAT|VLAST|VET|UZT|UYT|UYST|UTC|ULAT|TVT|TMT|TLT|TKT|TJT|TFT|TAHT|SST|SRT|SGT|SCT|SBT|SAST|SAMT|RET|PYT|PYST|PWT|PST|PONT|PMST|PMDT|PKT|PHT|PHOT|PGT|PETT|PETST|PET|PDT|OMST|OMSST|NZST|NZDT|NUT|NST|NPT|NOVT|NOVST|NFT|NDT|NCT|MYT|MVT|MUT|MST|MSK|MSD|MMT|MHT|MDT|MAWT|MART|MAGT|MAGST|LINT|LHST|LHDT|KUYT|KST|KRAT|KRAST|KGT|JST|IST|IRST|IRKT|IRKST|IRDT|IOT|IDT|ICT|HOVT|HKT|GYT|GST|GMT|GILT|GFT|GET|GAMT|GALT|FNT|FKT|FKST|FJT|FJST|EST|EGT|EGST|EET|EEST|EDT|ECT|EAT|EAST|EASST|DAVT|ChST|CXT|CVT|CST|COT|CLT|CLST|CKT|CHAST|CHADT|CET|CEST|CDT|CCT|CAT|CAST|BTT|BST|BRT|BRST|BOT|BNT|AZT|AZST|AZOT|AZOST|AWST|AWDT|AST|ART|AQTT|ANAT|ANAST|AMT|AMST|ALMT|AKST|AKDT|AFT|AEST|AEDT|ADT|ACST|ACDT"

ruleTimezone :: Rule
ruleTimezone = Rule
  { name = "<time> timezone"
  , pattern =
    [ Predicate $ and . sequence [isNotLatent, isATimeOfDay, hasNoTimezone]
    , regex $ "\\b(" ++ timezoneName ++ ")\\b"
    ]
  , prod = \tokens -> case tokens of
      (Token Time td:
       Token RegexMatch (GroupMatch (tz:_)):
       _) -> Token Time <$> inTimezone (Text.toUpper tz) td
      _ -> Nothing
  }

ruleTimezoneBracket :: Rule
ruleTimezoneBracket = Rule
  { name = "<time> (timezone)"
  , pattern =
    [ Predicate $ and . sequence [isNotLatent, isATimeOfDay, hasNoTimezone]
    , regex $ "\\((" ++ timezoneName ++ ")\\)"
    ]
  , prod = \tokens -> case tokens of
      (Token Time td:
       Token RegexMatch (GroupMatch (tz:_)):
       _) -> Token Time <$> inTimezone (Text.toUpper tz) td
      _ -> Nothing
  }

ruleIntervalDashTimezone :: Rule
ruleIntervalDashTimezone = Rule
  { name = "<datetime> - <datetime> (interval) timezone"
  , pattern =
    [ Predicate $ and . sequence [isATimeOfDay, hasNoTimezone]
    , regex "\\-|to|th?ru|through|(un)?til(l)?"
    , Predicate $ and . sequence [isATimeOfDay, hasNoTimezone]
    , regex $ "\\b(" ++ timezoneName ++ ")\\b"
    ]
  , prod = \tokens -> case tokens of
      (Token Time td1:
       _:
       Token Time td2:
       Token RegexMatch (GroupMatch (tz:_)):
       _) -> do
        tdz1 <- inTimezone (Text.toUpper tz) td1
        tdz2 <- inTimezone (Text.toUpper tz) td2
        Token Time <$> interval TTime.Closed tdz1 tdz2
      _ -> Nothing
  }

rules :: [Rule]
rules =
  [ ruleInstantDay
  , ruleIntersect
  , ruleIntersectOf
  , ruleIntersectYear
  , ruleAbsorbOnDay
  , ruleAbsorbOnADOW
  , ruleTuesday
  , ruleAbsorbInMonthYear
  , ruleAbsorbInYearSuffix
  , ruleAbsorbCommaTOD
  , ruleNextDOW
  , ruleNextTime
  , ruleThisTime
  , ruleLastTime
  , ruleTimeBeforeLastAfterNext
  , ruleLastDOWOfTime
  , ruleLastCycleOfTime
  , ruleLastNight
  , ruleLastWeekendOfMonth
  , ruleNthTimeOfTime
  , ruleTheNthTimeOfTime
  , ruleNthTimeAfterTime
  , ruleTheNthTimeAfterTime
  , ruleYearLatent
  , ruleYearOrdinalLatent
  , ruleYearOrdinalLatent2
  , ruleYearLatent2
  , ruleYearADBC
  , ruleTheDOMNumeral
  , ruleTheDOMOrdinal
  , ruleDOMLatent
  , ruleNamedDOMOrdinal
  , ruleMonthDOMNumeral
  , ruleDOMMonth
  , ruleDOMOfMonth
  , ruleDOMOrdinalMonthYear
  , ruleDOMMonthYear
  , ruleDOMMonthYear2
  , ruleDOMMonthYear3
  , ruleIdesOfMonth
  , ruleTODLatent
  , ruleAtTOD
  , ruleTODOClock1
  , ruleTODOClock2
  , ruleHHMM
  , ruleHHMMLatent
  , ruleHHMMSS
  , ruleMilitaryAMPM
  , ruleMilitarySpelledOutAMPM
  , ruleMilitarySpelledOutAMPM2
  , ruleTODAMPM
  , ruleHONumeral
  , ruleHONumeralWithZero
  , ruleHONumeralSuffix
  , ruleHONumeralSuffixWithZero
  , ruleHODHalf
  , ruleHODHalfOrdinal
  , ruleHODHalfShortOrdinal
  , ruleHODHalfShortJoinedOrdinal
  , ruleQuarterHODOrdinal
  , ruleWithoutQuarterToHOD
  , ruleWithoutNumeralToHOD
  , ruleNumeralToHODOrdinal
  , ruleHalfToHOD
  , ruleQuarterToHOD
  , ruleNumeralAfterHOD
  , ruleHalfAfterHOD
  , ruleQuarterAfterHOD
  , ruleHalfHOD
  , ruleYYYYQQ
  , ruleYYYYMM
  , ruleYYYYMMDD
  , ruleDDMM
  , ruleDDMM2
  , ruleDDMMYY
  , ruleDDMMYY2
  , ruleMMYYYY
  , ruleDDMMYY3
  , ruleNoon
  , ruleAfterNoon
  , ruleAfterMidnight
  , rule1Hour
  , ruleMidnight
  , ruleHourPartOfDays
  , rulePartOfDays
  , rulePartOfDays2
  , ruleEndOfDay
  , ruleEarlyMorning
  , rulePODIn
  , rulePODThis
  , ruleTonight
  , ruleAfterPartofday
  , ruleTimePOD
  , rulePODofTime
  , ruleWeekend
  , ruleWeek
  , ruleTODPrecision
  , rulePrecisionTOD
  , ruleIntervalFromMonthDDDD
  , ruleIntervalFromDDDDMonth
  , ruleIntervalMonthDDDD
  , ruleIntervalDDDDMonth
  , ruleIntervalDash
  , ruleIntervalSlash
  , ruleIntervalFromTo
  , ruleIntervalDOMFromTo
  , ruleIntervalTODFromTo
  , ruleIntervalBy
  , ruleIntervalByHours
  , ruleIntervalByTheEndOf
  , ruleIntervalUntilTime
  , ruleIntervalUntilDOM
  , ruleIntervalUntilDOMOrdinal
  , ruleIntervalAfterFromSinceTime
  , ruleCycleTheAfterBeforeTime
  , ruleCycleThis
  , ruleCycleThis2LastNext
  , ruleDOMOfTimeMonth
  , ruleCycleAfterCount
  , ruleCycleOrdinalOfTime
  , ruleCycleLastOrdinalOfTime
  , ruleCycleTheOrdinalOfTime
  , ruleCycleTheLastOrdinalOfTime
  , ruleCycleTheOfTime
  , ruleCycleOrdinalAfterTime
  , ruleCycleTheOrdinalAfterTime
  , ruleCycleOrdinalQuarter
  , ruleCycleTheOrdinalQuarter
  , ruleCycleOrdinalQuarterYear
  , ruleDurationInWithinAfter
  , ruleDurationLastNext
  , ruleNDOWago
  , ruleDurationHenceAgo
  , ruleDurationHence
  , ruleDayDurationHenceAgo
  , ruleDayInDuration
  , ruleInDurationAtTime
  , ruleDurationAfterBeforeTime
  , ruleIntervalForDurationFrom
  , ruleIntervalFromTimeForDuration
  , ruleIntervalTimeForDuration
  , ruleInNumeral
  , ruleTimezone
  , ruleTimezoneBracket
  , ruleIntervalDashTimezone
  , rulePartOfMonth
  , ruleEndOrBeginningOfMonth
  , ruleEndOrBeginningOfYear
  , ruleEndOrBeginningOfWeek
  , ruleNow
  , ruleSeason
  , ruleEndOfMonth
  , ruleBeginningOfMonth
  , ruleEndOfYear
  , ruleBeginningOfYear
  ]
  ++ ruleInstants
  ++ ruleDaysOfWeek
  ++ ruleMonths
  ++ ruleSeasons
  ++ rulePeriodicHolidays
