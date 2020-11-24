{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections   #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- |
-- Module      : Advent.OCR.Internal
-- Copyright   : (c) Justin Le 2020
-- License     : BSD3
--
-- Maintainer  : justin@jle.im
-- Stability   : experimental
-- Portability : non-portable
--
-- Internal utility functions for "Advent.OCR".


module Advent.OCR.Internal (
  -- * Parse
    V2(..)
  , Point
  , parseLettersV2
  , parseLettersEitherV2
  , parseAsciiMapV2
  -- * Letter Map
  , LetterMap(..)
  , lookupLetterMap
  , defaultLetterMap
  , rawLetterforms1
  , rawLetterforms2
  , parseLetterMap
  -- * Utility
  , contiguousShapes
  , contiguousShapesBy
  ) where

import           Advent.OCR.LetterMap
import           Data.Default.Class
import           Data.Maybe
import           Data.Monoid
import           Data.Set                   (Set)
import           Language.Haskell.TH.Lift
import qualified Data.Set                   as S

-- | A version of 'Advent.OCR.parseLetters' taking 'Point'.  Used
-- internally.
parseLettersV2
    :: LetterMap
    -> Set Point
    -> Maybe String
parseLettersV2 lm = (fmap . map) (either (const '?') id)
                  . parseLettersEitherV2 lm

-- | A version of 'Advent.OCR.parseLettersEither' taking (and returning)
-- 'Point'. Used internally.
parseLettersEitherV2
    :: LetterMap
    -> Set Point
    -> Maybe [Either (Set Point) Char]
parseLettersEitherV2 lm letters = listToMaybe attempts
  where
    attempts =
      [ res
      | refl <- [id, reflX]
      , rots <- [id, perp, negate, negate . perp]
      , let ls = S.map (rots . refl) letters
            ((Sum n, Sum goodCount), res) = tryMe ls
            percGood = goodCount / n :: Double
      , n > 0 && percGood >= 0.5
      ]
    tryMe = traverse (\c -> maybe ((Sum 1, Sum 0), Left c) (((Sum 1, Sum 1),) . Right) . lookupLetterMap c $ lm)
          . contiguousShapesBy v2x
    perp (V2 x y) = V2 (negate y) x
    reflX (V2 x y) = V2 (negate x) y

-- | Default is compatible with all challenges in Advent of Code 2015 to
-- 2019.
instance Default LetterMap where
    def = defaultLetterMap

-- | The default lettermap compatible all challenges in Advent of Code 2015
-- - 2019.
defaultLetterMap :: LetterMap
defaultLetterMap = $( lift $ uncurry parseLetterMap rawLetterforms1
                          <> uncurry parseLetterMap rawLetterforms2
                    )
