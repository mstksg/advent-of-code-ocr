{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE CPP                        #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}

module Advent.OCR.LetterMap (
    LetterMap(..)
  , V2(..)
  , Point
  , contiguousShapes
  , contiguousShapesBy
  , parseAsciiMapV2
  , rawLetterforms1
  , rawLetterforms2
  , parseLetterMap
  , lookupLetterMap
  ) where

import           Data.Data                         (Data)
import           Data.Foldable
import           Data.Map                          (Map)
import           Data.Monoid
import           Data.Semigroup
import           Data.Set                          (Set)
import           GHC.Generics
import           Instances.TH.Lift                 ()
import           Language.Haskell.TH.Lift
import           Language.Haskell.TH.Syntax
import           Language.Haskell.TH.Syntax.Compat (liftSplice)
import           Text.Heredoc                      (here)
import qualified Data.Map                          as M
import qualified Data.Set                          as S

-- | Type used internally to represent points; useful for its 'Num' and
-- 'Applicative' instances.
data V2 a = V2 { v2x :: !a, v2y :: !a }
  deriving (Show, Functor, Eq, Ord, Generic, Data)

instance Applicative V2 where
    pure x = V2 x x
    V2 fx fy <*> V2 x y = V2 (fx x) (fy y)

instance Num a => Num (V2 a) where
    V2 x1 y1 + V2 x2 y2 = V2 (x1 + x2) (y1 + y2)
    V2 x1 y1 - V2 x2 y2 = V2 (x1 - x2) (y1 - y2)
    V2 x1 y1 * V2 x2 y2 = V2 (x1 * x2) (y1 * y2)
    negate (V2 x y) = V2 (negate x) (negate y)
    abs (V2 x y) = V2 (abs x) (abs y)
    signum (V2 x y) = V2 (signum x) (signum y)
    fromInteger x = V2 (fromInteger x) (fromInteger x)

instance Fractional a => Fractional (V2 a) where
    recip (V2 x y) = V2 (recip x) (recip y)
    V2 x1 y1 / V2 x2 y2 = V2 (x1 / x2) (y1 / y2)
    fromRational x = V2 (fromRational x) (fromRational x)

instance Lift a => Lift (V2 a) where
    lift (V2 x y) = do
      lx <- lift x
      ly <- lift y
      pure $ AppE (AppE (ConE 'V2) lx) ly
#if MIN_VERSION_template_haskell(2,16,0)
    liftTyped = liftSplice . fmap TExp . lift
#endif

-- | A point is a 2-vector of ints.
type Point = V2 Int

-- | The set of unconnected shapes, indexed by their original center of
-- mass
contiguousShapes :: Set Point -> Map (V2 Double) (Set (Set Point))
contiguousShapes s0 = M.fromListWith (<>)
    [ (com, S.singleton (S.map (subtract topCorner) s))
    | s <- allSubgraphs (S.fromList . fullNeighbs) s0
    , let com            = mean (fmap fromIntegral) s
          V2 topCorner _ = boundingBox s
    ]

allSubgraphs
    :: forall a. Ord a
    => (a -> Set a)     -- ^ Expansion
    -> Set a            -- ^ points
    -> [Set a]
allSubgraphs f = go []
  where
    go !seen !rest = case S.minView rest of
      Nothing      -> seen
      Just (x, xs) ->
        let new = floodFill (S.intersection xs . f) (S.singleton x)
        in  go (new : seen) (xs `S.difference` new)

-- | The set of unconnected shapes, sorted against some function on their
-- original center of masses.
contiguousShapesBy
    :: Ord a
    => (V2 Double -> a)
    -> Set Point
    -> [Set Point]
contiguousShapesBy f = concatMap toList . M.mapKeys f . contiguousShapes

floodFill
    :: Ord a
    => (a -> Set a)     -- ^ Expansion (be sure to limit allowed points)
    -> Set a            -- ^ Start points
    -> Set a            -- ^ Flood filled, with count of number of steps
floodFill f = go S.empty
  where
    go !innr !outr
        | S.null outr' = innr'
        | otherwise    = go innr' outr'
      where
        innr' = S.union innr outr
        outr' = foldMap f outr `S.difference` innr'

fullNeighbs :: Point -> [Point]
fullNeighbs p = [ p + V2 dx dy
                | dx <- [-1 .. 1]
                , dy <- if dx == 0 then [-1,1] else [-1..1]
                ]

boundingBox :: (Bounded a, Foldable f, Applicative g, Ord a) => f (g a) -> V2 (g a)
boundingBox = (\(Ap mn, Ap mx) -> V2 (getMin <$> mn) (getMax <$> mx))
            . foldMap (\p -> (Ap (Min <$> p), Ap (Max <$> p)))

-- | will error if empty list
mean :: (Foldable f, Fractional b) => (a -> b) -> f a -> b
mean f xs0 = sx1 / sx0
  where
    (sx0, sx1) = go 0 0 (toList xs0)
    go !x0 !x1 = \case
      []   -> (x0, x1)
      x:xs -> go (x0 + 1) (x1 + f x) xs

-- | A database associating a set of "on" points to the
-- letter they represent.
--
-- See 'Advent.OCR.Internal.defaultLetterMap' for a database compatible
-- with Advent of Code 2015-2019.
newtype LetterMap = LetterMap { getLetterMap :: Map (Set Point) Char }
  deriving (Show, Eq, Ord, Semigroup, Monoid, Generic, Data)

instance Lift LetterMap where
    lift (LetterMap x) = AppE (ConE 'LetterMap) <$> lift x
#if MIN_VERSION_template_haskell(2,16,0)
    liftTyped = liftSplice . fmap TExp . lift
#endif

-- | Lookup a set of points for the letter it represents in a 'LetterMap'.
-- The set is expected to be aligned with (0,0) as the upper left corner of
-- its obunding box.
lookupLetterMap :: Set Point -> LetterMap -> Maybe Char
lookupLetterMap k = M.lookup k . getLetterMap

-- | Given a list of characters and ASCII art for all those characters
-- (from left to right), builds the appropriate 'LetterMap'.
--
-- An example usage would be:
--
-- @
-- 'parseLetterMap' "ABC" abcArt
-- @
--
-- where @abcArt@ is:
--
-- > .##..###...##.
-- > #..#.#..#.#..#
-- > #..#.###..#...
-- > ####.#..#.#...
-- > #..#.#..#.#..#
-- > #..#.###...##.
--
-- Expects ASCII art where @#@ is the "on"/included character.
parseLetterMap :: [Char] -> String -> LetterMap
parseLetterMap ls = LetterMap
                  . M.fromList
                  . zipWith (flip (,)) ls
                  . contiguousShapesBy v2x
                  . parseAsciiMapV2 (S.singleton '#')

-- | Parse raw ASCII art into a set of points, usable with
-- 'Advent.OCR.Internal.parseLettersV2'.
parseAsciiMapV2
    :: Set Char             -- ^ characters to use as "on"/included
    -> String               -- ^ raw map ASCII art
    -> Set Point
parseAsciiMapV2 c = zipWithFold (\j -> zipWithFold (\i x ->
                        if x `S.member` c
                          then S.singleton (V2 i j)
                          else S.empty
                      ) [0..]) [0..]
                  . lines

zipWithFold
    :: Monoid m
    => (a -> b -> m)
    -> [a]
    -> [b]
    -> m
zipWithFold f xs = fold . zipWith f xs

-- | Seen in 2016 Day 8, 2019 Day 8 and 11.  4x6 glyphs.
--
-- Load using @uncurry 'parseLetterMap'@.
rawLetterforms1 :: (String, String)
rawLetterforms1 = ("ABCEFGHIJKLOPRSUYZ", filter (/= ' ') $ drop 1 [here|
.##..###...##..####.####..##..#..#.###...##.#..#.#.....##..###..###...###.#..#.#...#.####
#..#.#..#.#..#.#....#....#..#.#..#..#.....#.#.#..#....#..#.#..#.#..#.#....#..#.#...#....#
#..#.###..#....###..###..#....####..#.....#.##...#....#..#.#..#.#..#.#....#..#..#.#....#.
####.#..#.#....#....#....#.##.#..#..#.....#.#.#..#....#..#.###..###...##..#..#...#....#..
#..#.#..#.#..#.#....#....#..#.#..#..#..#..#.#.#..#....#..#.#....#.#.....#.#..#...#...#...
#..#.###...##..####.#.....###.#..#.###..##..#..#.####..##..#....#..#.###...##....#...####
|])

-- | Based on
-- <https://gist.github.com/usbpc/5fa0be48ad7b4b0594b3b8b029bc47b4>.  6x10
-- glyphs.
--
-- Seen in 2018 Day 10.
--
-- Load using @uncurry 'parseLetterMap'@.
rawLetterforms2 :: (String, String)
rawLetterforms2 = ("ABCEFGHJKLNPRXZ", filter (/= ' ') $ drop 1 [here|
..##...#####...####..######.######..####..#....#....###.#....#.#......#....#.#####..#####..#....#.######
.#..#..#....#.#....#.#......#......#....#.#....#.....#..#...#..#......##...#.#....#.#....#.#....#......#
#....#.#....#.#......#......#......#......#....#.....#..#..#...#......##...#.#....#.#....#..#..#.......#
#....#.#....#.#......#......#......#......#....#.....#..#.#....#......#.#..#.#....#.#....#..#..#......#.
#....#.#####..#......#####..#####..#......######.....#..##.....#......#.#..#.#####..#####....##......#..
######.#....#.#......#......#......#..###.#....#.....#..##.....#......#..#.#.#......#..#.....##.....#...
#....#.#....#.#......#......#......#....#.#....#.....#..#.#....#......#..#.#.#......#...#...#..#...#....
#....#.#....#.#......#......#......#....#.#....#.#...#..#..#...#......#...##.#......#...#...#..#..#.....
#....#.#....#.#....#.#......#......#...##.#....#.#...#..#...#..#......#...##.#......#....#.#....#.#.....
#....#.#####...####..######.#.......###.#.#....#..###...#....#.######.#....#.#......#....#.#....#.######
|])
