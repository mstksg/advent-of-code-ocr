{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections   #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Advent.OCR.Internal (
  -- * Parse
    V2(..)
  , Point
  , parseLettersV2
  , parseLettersEitherV2
  -- * Letter Map
  , LetterMap(..)
  , lookupLetterMap
  , defaultLetterMap
  , rawLetterforms1
  , rawLetterforms2
  , toLetterMap
  -- * Utility
  , contiguousShapes
  , contiguousShapesBy
  , parseAsciiMap
  ) where

import           Advent.OCR.LetterMap
import           Data.Default.Class
import           Data.Maybe
import           Data.Monoid
import           Data.Set                   (Set)
import           Language.Haskell.TH.Lift
import qualified Data.Set                   as S

parseLettersV2
    :: LetterMap
    -> Set Point
    -> Maybe String
parseLettersV2 lm = (fmap . map) (either (const '?') id)
                  . parseLettersEitherV2 lm

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
            (Any isGood, res) = tryMe ls
      , isGood
      ]
    tryMe = traverse (\c -> maybe (Any False, Left c) ((Any True,) . Right) . lookupLetterMap c $ lm)
          . contiguousShapesBy v2x
    perp (V2 x y) = V2 (negate y) x
    reflX (V2 x y) = V2 (negate x) y

instance Default LetterMap where
    def = defaultLetterMap

-- | The default lettermap compatible all challenges in Advent of Code 2015
-- - 2019.
defaultLetterMap :: LetterMap
defaultLetterMap = $( lift $ uncurry toLetterMap rawLetterforms1
                          <> uncurry toLetterMap rawLetterforms2
                    )
