module Advent.OCR (
  -- * Parse
    parseLetters
  , parseLettersWith
  , parseLettersEither
  , unsafeParseLetters
  , parseAsciiMap
  -- * Letter Map
  , LetterMap
  , defaultLetterMap
  , toLetterMap
  ) where

import           Advent.OCR.Internal
import           Data.Bifunctor
import           Data.Maybe
import           Data.Set            (Set)
import qualified Data.Set            as S

-- | A version of 'parseLetters' that takes a set of any type of value, as
-- long as you provide functions to access the X and Y coordinates.
parseLettersWith
    :: LetterMap
    -> (a -> Int)       -- ^ get X
    -> (a -> Int)       -- ^ get Y
    -> Set a
    -> Maybe String
parseLettersWith lm f g = parseLettersV2 lm . S.map (\x -> V2 (f x) (g x))

-- | The go-to default: given a set of point coordinates, parse it into
-- the string it represents.  Should be compatible with any Advent of Code
-- challenge from 2015 to 2019.
--
-- @
-- 'parseLetters' 'defaultLetterMap' myPoints
--
-- -- or, using Data.Default
-- 'parseLetters' 'def' myPoints
-- @
--
-- A 'Nothing' means that there were no recognized letters found.  A 'Just'
-- means that least least one recognized character was found.  Unrecognized
-- characters will be replaced with "?"; for more information, use
-- 'parseLettersEither'.
parseLetters
    :: LetterMap            -- ^ database of letterforms
    -> Set (Int, Int)       -- ^ set of points
    -> Maybe String         -- ^ result, with unknown letters replaced with "?"
parseLetters lm = parseLettersV2 lm . S.mapMonotonic (uncurry V2)

-- | A version of 'parseLetters' returning a list of characters that were
-- either recognized or unrecognized; in the case of unrecognized
-- characters, returns the set of their coordinates but shifted to (0, 0)
-- center of mass.
parseLettersEither
    :: LetterMap
    -> Set (Int, Int)
    -> Maybe [Either (Set (Int, Int)) Char]
parseLettersEither lm = (fmap . map . first . S.mapMonotonic) (\(V2 x y) -> (x, y))
                      . parseLettersEitherV2 lm
                      . S.mapMonotonic (uncurry V2)

-- | A version of 'parseLetters' that will be undefined ('error') when no
-- parse is found.
unsafeParseLetters
    :: LetterMap
    -> Set (Int, Int)
    -> String
unsafeParseLetters lm =
      fromMaybe (error "Advent.OCR.unsafeParseLetters: Unable to parse letters")
    . parseLetters lm

-- | Parse a raw ASCII map into a set of points, usable with
-- 'parseLetters'.
parseAsciiMap
    :: Set Char             -- ^ characters to use as "on"/included
    -> String               -- ^ raw map
    -> Set (Int, Int)
parseAsciiMap c = S.mapMonotonic (\(V2 x y) -> (x, y)) . parseAsciiMapV2 c

