-- |
-- Module      : Advent.OCR
-- Copyright   : (c) Justin Le 2020
-- License     : BSD3
--
-- Maintainer  : justin@jle.im
-- Stability   : experimental
-- Portability : non-portable
--
-- Library to parse "ASCII Art" letters from <https://adventofcode.com
-- Advent of Code> puzzles.  Compatible with all puzzles from 2015 to 2019.
--
module Advent.OCR (
  -- * Parse
    parseLetters
  , parseLettersWith
  , parseLettersEither
  , unsafeParseLetters
  , parseAsciiMap
  , asciiMapToLetters
  -- * Letter Map
  , LetterMap
  , defaultLetterMap
  -- ** Custom Letter Map
  , parseLetters'
  , parseLettersWith'
  , parseLettersEither'
  , unsafeParseLetters'
  , asciiMapToLetters'
  ) where

import           Advent.OCR.Internal
import           Data.Bifunctor
import           Data.Maybe
import           Data.Set            (Set)
import qualified Data.Set            as S

-- | A version of 'parseLettersWith'' accepting a custom 'LetterMap'
-- letterform database.
parseLettersWith'
    :: LetterMap        -- ^ database of letterforms
    -> (a -> Int)       -- ^ get X
    -> (a -> Int)       -- ^ get Y
    -> Set a
    -> Maybe String
parseLettersWith' lm f g = parseLettersV2 lm . S.map (\x -> V2 (f x) (g x))

-- | A version of 'parseLetters' that takes a set of any type of value, as
-- long as you provide functions to access the X and Y coordinates.
parseLettersWith
    :: (a -> Int)       -- ^ get X
    -> (a -> Int)       -- ^ get Y
    -> Set a
    -> Maybe String
parseLettersWith = parseLettersWith' defaultLetterMap

-- | A version of 'parseLetters'' accepting a custom 'LetterMap' letterform
-- database.
parseLetters'
    :: LetterMap            -- ^ database of letterforms
    -> Set (Int, Int)       -- ^ set of points
    -> Maybe String         -- ^ result, with unknown letters replaced with "?"
parseLetters' lm = parseLettersV2 lm . S.mapMonotonic (uncurry V2)

-- | The go-to default: given a set of point coordinates, parse it into
-- the string it represents.  Should be compatible with any Advent of Code
-- challenge from 2015 to 2019.
--
-- @
-- 'parseLetters' 'defaultLetterMap' myPoints
--
-- -- or, using Data.Default
-- 'parseLetters' 'Data.Default.Class.def' myPoints
-- @
--
-- A 'Nothing' means that there were no recognized letters found.  A 'Just'
-- means that least 50% of letter forms are recognized.  Unrecognized
-- characters will be replaced with "?"; for more information, use
-- 'parseLettersEither'.
--
-- This function is robust to changes in orientation or flipping, but will
-- be fastest if the coordinates are oriented with (0,0) on the upper left
-- corner.  However, because of this, it might return the wrong answer if
-- your coordinates are /not/ oriented this way and your result is
-- symmetrical: it'll always prioritize the interpretaion against (0,0)
-- upper-left orientation first.
parseLetters
    :: Set (Int, Int)       -- ^ set of points
    -> Maybe String         -- ^ result, with unknown letters replaced with "?"
parseLetters = parseLetters' defaultLetterMap

-- | A version of 'parseLettersEither'' accepting a custom 'LetterMap'
-- letterform database.
parseLettersEither'
    :: LetterMap            -- ^ database of letterforms
    -> Set (Int, Int)
    -> Maybe [Either (Set (Int, Int)) Char]
parseLettersEither' lm = (fmap . map . first . S.mapMonotonic) (\(V2 x y) -> (x, y))
                      . parseLettersEitherV2 lm
                      . S.mapMonotonic (uncurry V2)

-- | A version of 'parseLetters' returning a list of characters that were
-- either recognized or unrecognized; in the case of unrecognized
-- characters, returns the set of their coordinates but shifted to (0, 0)
-- as its upper left corner.
parseLettersEither
    :: Set (Int, Int)
    -> Maybe [Either (Set (Int, Int)) Char]
parseLettersEither = parseLettersEither' defaultLetterMap

-- | A version of 'unsafeParseLetters'' accepting a custom 'LetterMap'
-- letterform database.
unsafeParseLetters'
    :: LetterMap
    -> Set (Int, Int)
    -> String
unsafeParseLetters' lm =
      fromMaybe (error "Advent.OCR.unsafeParseLetters': Unable to parse letters")
    . parseLetters' lm

-- | A version of 'parseLetters' that will be undefined ('error') when no
-- parse is found.
unsafeParseLetters
    :: Set (Int, Int)
    -> String
unsafeParseLetters =
      fromMaybe (error "Advent.OCR.unsafeParseLetters: Unable to parse letters")
    . parseLetters

-- | Parse raw ASCII art into a set of points, usable with
-- 'parseLetters'.
parseAsciiMap
    :: Set Char             -- ^ characters to use as "on"/included
    -> String               -- ^ raw ASCII art
    -> Set (Int, Int)
parseAsciiMap c = S.mapMonotonic (\(V2 x y) -> (x, y)) . parseAsciiMapV2 c

-- | A version of 'asciiMapToLetters'' accepting a custom 'LetterMap'
-- letterform database.
asciiMapToLetters'
    :: Set Char             -- ^ characters to use as "on"/included in ASCII art
    -> LetterMap            -- ^ database of letterforms
    -> String               -- ^ raw ASCII art
    -> Maybe String
asciiMapToLetters' c lm = parseLettersV2 lm . parseAsciiMapV2 c

-- | Convenient all-in-one utility function combining 'parseAsciiMap' and
-- 'parseLetters', to directly parse ASCII art into its letters.
asciiMapToLetters
    :: Set Char             -- ^ characters to use as "on"/included in ASCII art
    -> String               -- ^ raw ASCII art
    -> Maybe String
asciiMapToLetters c = asciiMapToLetters' c defaultLetterMap
