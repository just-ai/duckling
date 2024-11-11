-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree. An additional grant
-- of patent rights can be found in the PATENTS file in the same directory.


{-# LANGUAGE OverloadedStrings #-}

module Duckling.Ordinal.RU.Corpus
  ( corpus ) where

import Prelude
import Data.String

import Duckling.Locale
import Duckling.Ordinal.Types
import Duckling.Resolve
import Duckling.Testing.Types

corpus :: Corpus
corpus = (testContext {locale = makeLocale RU Nothing}, testOptions, allExamples)

allExamples :: [Example]
allExamples = concat
  [ examples (OrdinalData 1)
             [ "первый"
             , "Первая"
             , "первая"
             , "первое"
             , "первой"
             , "первого"
             , "первых"
             , "первыми"
             , "первые"
             , "первому"
             , "первом"
             , "1ая"
             , "1-ая"
             , "1ый"
             , "1-ый"
             , "1ое"
             , "1-ое"
             , "1й"
             , "1-й"
             , "1го"
             , "1-го"
             , "1-ого"
             , "первой"
             , "первым"
             , "первому"
             , "первым"
             ]
  , examples (OrdinalData 3)
             [ "третий"
             , "третья"
             , "третье"
             , "третьей"
             , "третьего"
             , "третьих"
             , "третьими"
             , "третьи"
             , "3й"
             , "3ий"
             , "3я"
             , "3ья"
             , "3е"
             , "3ье"
             , "3го"
             , "3-й"
             , "3-ий"
             , "3-я"
             , "3-ья"
             , "3-е"
             , "3-ье"
             , "3-го"
             , "3-его"
             , "третьей"
             , "третьим"
             , "третьему"
             , "третьим"
             ]
  , examples (OrdinalData 4)
             [ "четвертый"
             , "четвертая"
             , "четвертое"
             , "четвертой"
             , "четвертого"
             , "четвертых"
             , "четвёртый"
             , "четвертые"
             , "4й"
             , "4ый"
             , "4ая"
             , "4ое"
             , "4ой"
             , "4го"
             , "4-й"
             , "4-ый"
             , "4-ая"
             , "4-ое"
             , "4-ой"
             , "4-го"
             , "4-ого"
             , "четвертой"
             , "четвертым"
             , "четвёртому"
             , "четвёртым"
             ]
  , examples (OrdinalData 15)
             [ "пятнадцатый"
             , "пятнадцатых"
             , "15й"
             , "15-й"
             , "15-ый"
             , "пятнадцатая"
             , "15я"
             , "15-я"
             , "15-ая"
             , "пятнадцатое"
             , "15е"
             , "15-е"
             , "15-ое"
             , "пятнадцатому"
             , "пятнадцатые"
             , "пятнадцатой"
             , "пятнадцатым"
             , "пятнадцатому"
             , "пятнадцатым"
             ]
  , examples (OrdinalData 10)
             [ "десятый"
             , "10й"
             , "10-й"
             , "10-ый"
             , "десятая"
             , "десятой"
             ]
  , examples (OrdinalData 21)
             [ "21й"
             , "21-й"
             , "21-го"
             , "21-ого"
             , "Двадцать первый"
             , "двадцать первый"
             , "двадцать первых"
             , "двадцать первого"
             , "двадцать первой"
             ]
  , examples (OrdinalData 23)
             [ "23й"
             , "23-й"
             , "двадцать третий"
             , "двадцать третьих"
             , "двадцать третьего"
             , "двадцать третьей"
             , "23-го"
             , "23-его"
             ]
  , examples (OrdinalData 31)
             [ "31ый"
             , "31-ый"
             , "тридцать первый"
             ]
  , examples (OrdinalData 30)
             [ "30ый"
             , "30-ый"
             , "тридцатый"
             ]
  , examples (OrdinalData 48)
             [ "48ое"
             , "48-ое"
             , "сорок восьмое"
             , "сорок восьмого"
             ]
  , examples (OrdinalData 40)
             [ "40ое"
             , "40-ое"
             , "сороковой"
             , "сороковое"
             , "сороковая"
             ]
  , examples (OrdinalData 99)
             [ "99ый"
             , "99-й"
             , "99-ый"
             , "девяносто девятый"
             , "девяносто девятых"
             , "девяносто девятая"
             ]
  , examples (OrdinalData 90)
             [ "90ый"
             , "90-й"
             , "90-ый"
             , "девяностый"
             , "девяностая"
             ]
  , examples (OrdinalData 20)
             [ "20-й"
             , "двадцатый"
             , "двадцатых"
             ]
  , examples (OrdinalData 30)
             [ "30-й"
             , "тридцатый"
             , "тридцатых"
             , "тридцатому"
             ]
  , examples (OrdinalData 40)
             [ "40-й"
             , "сороковой"
             , "сороковых"
             ]
  , examples (OrdinalData 50)
             [ "50-й"
             , "пятидесятый"
             , "пятидесятых"
             ]
  , examples (OrdinalData 60)
             [ "60-й"
             , "шестидесятую"
             , "шестьдесятую"
             , "шестидесятых"
             ]
  , examples (OrdinalData 80)
             [ "80-й"
             , "восьмидесятая"
             , "восьмидесятого"
             , "восьмидесятое"
             , "восьмидесятой"
             , "восьмидесятом"
             , "восьмидесятому"
             , "восьмидесятою"
             , "восьмидесятую"
             , "восьмидесятые"
             , "восьмидесятый"
             , "восьмидесятым"
             , "восьмидесятыми"
             , "восьмидесятых"
             ]
  , examples (OrdinalData 100)
             [ "сотое"
             , "сотая"
             , "100ый"
             , "сотый"
             , "сотого"
             , "сотом"
             , "сотая"
             , "сотое"
             , "100-ая"
             , "100-я"
             , "100-й"
             , "100-е"
             ]
  , examples (OrdinalData 200)
             [ "двухсотый"
             ]
  , examples (OrdinalData 300)
             [ "трёхсотый"
             ]
  , examples (OrdinalData 400)
             [ "четырехсотый"
             ]
  , examples (OrdinalData 500)
             [ "пятисотый"
             ]
  , examples (OrdinalData 600)
             [ "шестисотый"
             ]
  , examples (OrdinalData 700)
             [ "семисотый"
             ]
  , examples (OrdinalData 800)
             [ "восьмисотый"
             ]
  , examples (OrdinalData 900)
             [ "девятисотый"
             ]
  , examples (OrdinalData 525)
             [ "пятьсот двадцать пятый"
             , "пятьсот двадцать пятого"
             ]
  , examples (OrdinalData 1000)
             [ "1000ый"
             , "1000-й"
             , "тысячный"
             , "тысячного"
             , "тысячном"
             , "тысячная"
             , "тысячное"
             ]
  , examples (OrdinalData 2000)
             [ "двутысячный"
             , "двутысячная"
             ]
  , examples (OrdinalData 3000)
             [ "трёхтысячный"
             ]
  , examples (OrdinalData 4000)
             [ "четырехтысячный"
             ]
  , examples (OrdinalData 5000)
             [ "пятитысячный"
             ]
  , examples (OrdinalData 6000)
             [ "шеститысячный"
             ]
  , examples (OrdinalData 7000)
             [ "семитысячный"
             ]
  , examples (OrdinalData 8000)
             [ "восьмитысячный"
             ]
  , examples (OrdinalData 9000)
             [ "девятитысячный"
             ]
  , examples (OrdinalData 1000000)
             [ "1000000-й"
             , "миллионный"
             , "миллионного"
             , "миллионном"
             , "миллионное"
             , "миллионная"
             , "миллионную"
             , "миллиоными"
             ]
  , examples (OrdinalData 1021)
             [ "тысяча двадцать первый"
             ]
  , examples (OrdinalData 1500)
             [ "тысяча пятисотый"
             ]
  , examples (OrdinalData 3000207)
             [ "три миллиона двести седьмой"
             ]
  , examples (OrdinalData 3000900)
             [ "три миллиона девятисотый"
             ]
  , examples (OrdinalData 3060160)
             [ "три миллиона шестьдесят тысяч сто шестидесятый"
             ]
  ]
