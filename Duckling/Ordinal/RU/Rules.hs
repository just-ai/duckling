-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree. An additional grant
-- of patent rights can be found in the PATENTS file in the same directory.


{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoRebindableSyntax #-}

module Duckling.Ordinal.RU.Rules
  ( rules
  ) where

import Data.HashMap.Strict (HashMap)
import Data.String
import Prelude
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Text as Text

import Duckling.Dimensions.Types
import Duckling.Numeral.Helpers (parseInt, isPositive, hasGrain, isNatural)
import Duckling.Numeral.Types (NumeralData (..))
import Duckling.Ordinal.Helpers
import Duckling.Ordinal.Types (OrdinalData (..))
import Duckling.Regex.Types
import Duckling.Types
import qualified Duckling.Numeral.Types as TNumeral
import qualified Duckling.Ordinal.Types as TOrdinal

ordinalsFirstthMap :: HashMap Text.Text Int
ordinalsFirstthMap = HashMap.fromList
  [ ( "перв", 1 )
  , ( "втор", 2 )
  , ( "трет", 3 )
  , ( "четверт", 4 )
  , ( "пят", 5 )
  , ( "шест", 6 )
  , ( "седьм", 7 )
  , ( "восьм", 8 )
  , ( "девят", 9 )
  , ( "десят", 10 )
  , ( "одинадцат", 11 )
  , ( "одиннадцат", 11 )
  , ( "двенадцат", 12 )
  , ( "тринадцат", 13 )
  , ( "четырнадцат", 14 )
  , ( "пятнадцат", 15 )
  , ( "шестнадцат", 16 )
  , ( "семнадцат", 17 )
  , ( "восемнадцат", 18 )
  , ( "девятнадцат", 19 )
  , ( "двадцат", 20 )
  ]

cardinalsMap :: HashMap Text.Text Int
cardinalsMap = HashMap.fromList
  [ ( "двадцать", 20 )
  , ( "тридцать", 30 )
  , ( "сорок", 40 )
  , ( "пятьдесят", 50 )
  , ( "шестьдесят", 60 )
  , ( "семьдесят", 70 )
  , ( "восемьдесят", 80 )
  , ( "девяносто", 90 )
  ]

cardinalsMapCut :: HashMap Text.Text Int
cardinalsMapCut = HashMap.fromList
  [ ( "двадцат", 20 )
  , ( "тридцат", 30 )
  , ( "сороков", 40 )
  , ( "пятьдесят", 50 )
  , ( "пятидесят", 50 )
  , ( "шестьдесят", 60 )
  , ( "шестидесят", 60 )
  , ( "семьдесят", 70 )
  , ( "семидесят", 70 )
  , ( "восемьдесят", 80 )
  , ( "восьмидесят", 80 )
  , ( "девяност", 90 )
  ]

ruleOrdinalsFirstth :: Rule
ruleOrdinalsFirstth = Rule
  { name = "ordinals (first..19th)"
  , pattern =
    [ regex "(перв|втор|трет|четверт|пят|шест|седьм|восьм|девят|десят|одинадцат|одиннадцат|двенадцат|тринадцат|четырнадцат|пятнадцат|шестнадцат|семнадцат|восемнадцат|девятнадцат|двадцат)(ь(и(ми|х|)|е(го|й|))|ого|ы(ми?|й|е|х)|ой|ий|ая|ое|ья)"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):_) ->
        ordinal <$> HashMap.lookup (Text.toLower match) ordinalsFirstthMap
      _ -> Nothing
  }

ruleOrdinalsTens :: Rule
ruleOrdinalsTens = Rule
  { name = "ordinals (20, 30,..,90)"
  , pattern =
    [ regex "(двадцат|тридцат|сороков|пят(и|ь)десят|шест(и|ь)десят|сем(и|ь)десят|вос(емь|ьми)десят|девяност)(ая|о(го|е|й|(му|м)|ю)|ую|ы(е|й|(ми|м)|х))"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):_) -> do
        ordinal <$> HashMap.lookup (Text.toLower match) cardinalsMapCut
      _ -> Nothing
  }
      

ruleOrdinal :: Rule
ruleOrdinal = Rule
  { name = "ordinals (composite, e.g. восемьдесят-пятый, сорок-седьмой, двадвать-девятый, тридцать-третий"
  , pattern =
    [ regex "(двадцать|тридцать|сорок|пятьдесят|шестьдесят|семьдесят|восемьдесят|девяносто)"
    , regex "(перв|втор|трет|четверт|пят|шест|седьм|восьм|девят)(ь(и(ми|х|)|е(го|й|))|ого|ы(ми?|й|е|х)|ой|ий|ая|ое|ья)"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (m1:_)):
       Token RegexMatch (GroupMatch (m2:_)):
       _) -> do
         dozen <- HashMap.lookup (Text.toLower m1) cardinalsMap
         unit <- HashMap.lookup (Text.toLower m2) ordinalsFirstthMap
         Just . ordinal $ dozen + unit
      _ -> Nothing
  }

ruleOrdinalDigits :: Rule
ruleOrdinalDigits = Rule
  { name = "ordinal (digits)"
  , pattern =
    [ regex "0*(\\d+)-?((ы|о|и|а|e|ь)?(ее|й|я|е|го))"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):_) -> ordinal <$> parseInt match
      _ -> Nothing
  }

rule100 :: Rule
rule100 = Rule
  { name = "ordinal 100"
  , pattern =
    [ regex "сот(ы(й|х)|ого|ому|ом)"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (m:_)):_) -> Just (ordinal 100)
      _ -> Nothing
  }

ordPrefixMap :: HashMap Text.Text Int
ordPrefixMap = HashMap.fromList
  [ ( "дву", 2)
  , ( "двух", 2)
  , ( "трех", 3)
  , ( "трёх", 3)
  , ( "четырёх", 4)
  , ( "четырех", 4)
  , ( "пяти", 5)
  , ( "шести", 6)
  , ( "семи", 7)
  , ( "восьми", 8)
  , ( "девяти", 9)
  ]

rule100Composed :: Rule
rule100Composed = Rule
  { name = "ordinal [2..9]x100"
  , pattern =
    [ regex "(дву|двух|трех|трёх|четырех|четырёх|пяти|шести|семи|восьми|девяти)сот(ы(й|х)|ого|ому|ом)"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):_) -> do
        cnt <- HashMap.lookup (Text.toLower match) ordPrefixMap
        Just . ordinal $ cnt * 100
      _ -> Nothing
  }

rule1000 :: Rule
rule1000 = Rule
  { name = "ordinal 1000"
  , pattern =
    [ regex "тысячн(ы(й|х)|ого|ому|ом)"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (m:_)):_) -> Just (ordinal 1000)
      _ -> Nothing
  }

rule1000Composed :: Rule
rule1000Composed = Rule
  { name = "ordinal [2..9]x100"
  , pattern =
    [ regex "(дву|двух|трех|трёх|четырех|четырёх|пяти|шести|семи|восьми|девяти)тысячн(ы(й|х)|ого|ому|ом)"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):_) -> do
        cnt <- HashMap.lookup (Text.toLower match) ordPrefixMap
        Just . ordinal $ cnt * 1000
      _ -> Nothing
  }
  

ruleComposite :: Rule
ruleComposite = Rule
  { name = "ordinal composition"
  , pattern =
    [ Predicate $ and . sequence [hasGrain, isNatural]
    , dimension Ordinal
    ]
  , prod = \tokens -> case tokens of
      (Token Numeral NumeralData{TNumeral.value = val1, TNumeral.grain = Just g}:
       Token Ordinal OrdinalData{TOrdinal.value = val2}:
       _) -> do
        case (10 ^ g) > val2 of
          True -> Just . ordinal $ (floor val1) + val2
          _ -> Nothing
        -- (10 ** (fromIntegral g)) > val2
        -- todo (10 ** fromIntegral g) > val2
        -- val2 <- getIntValue token
        -- ordinal $ val1 + val2
      _ -> Nothing
  }

rules :: [Rule]
rules =
  [ ruleOrdinal
  , ruleOrdinalsTens
  , ruleOrdinalDigits
  , ruleOrdinalsFirstth
  , rule100
  , rule100Composed
  , rule1000
  , rule1000Composed
  , ruleComposite
  ]
