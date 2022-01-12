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
             ]
  , examples (OrdinalData 3)
             [ "третий"
             , "третья"
             , "третье"
             , "третьей"
             , "третьего"
             , "третьих"
             , "третьими"
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
             ]
  , examples (OrdinalData 4)
             [ "четвертый"
             , "четвертая"
             , "четвертое"
             , "четвертой"
             , "четвертого"
             , "четвертых"
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
             ]
  , examples (OrdinalData 15)
             [ "пятнадцатый"
             , "пятнадцатых"
             , "15й"
             , "15-й"
             ]
  , examples (OrdinalData 21)
             [ "21й"
             , "21-й"
             , "21-го"
             , "Двадцать первый"
             , "двадцать первый"
             , "двадцать первых"
             , "двадцать первого"
             ]
  , examples (OrdinalData 23)
             [ "23й"
             , "23-й"
             , "двадцать третий"
             , "двадцать третьих"
             , "двадцать третьего"
             ]
  , examples (OrdinalData 31)
             [ "31ый"
             , "31-ый"
             , "тридцать первый"
             , "тридцать первых"
             ]
  , examples (OrdinalData 48)
             [ "48ое"
             , "48-ое"
             , "сорок восьмое"
             , "сорок восьмого"
             ]
  , examples (OrdinalData 99)
             [ "99ый"
             , "99-й"
             , "девяносто девятый"
             , "девяносто девятых"
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
             [ "100ый"
             , "100-й"
             , "сотый"
             , "сотого"
             , "сотом"
             , "сотая"
             , "сотое"
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
