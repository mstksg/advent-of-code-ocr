{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE QuasiQuotes                #-}

module Advent.OCR.LetterMap (
    LetterMap(..)
  , V2(..)
  , Point
  , contiguousShapes
  , contiguousShapesBy
  , parseAsciiMapV2
  , rawLetterforms1
  , rawLetterforms2
  , toLetterMap
  , lookupLetterMap
  ) where

import           Data.Data                (Data)
import           Data.Foldable
import           Data.Map                 (Map)
import           Data.Monoid
import           Data.Semigroup
import           Data.Set                 (Set)
import           GHC.Generics
import           Instances.TH.Lift        ()
import           Language.Haskell.TH.Lift
import           Text.Heredoc             (here)
import qualified Data.Map                 as M
import qualified Data.Set                 as S

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

type Point = V2 Int

-- | The set of unconnected shapes, indexed by their original center of
-- mass
contiguousShapes :: Set Point -> Map (V2 Double) (Set (Set Point))
contiguousShapes s0 = M.fromListWith (<>)
    [ (com, S.singleton (S.map (subtract topCorner) s))
    | s <- S.toList . S.map flood $ s0
    , let com            = mean (fmap fromIntegral) s
          V2 topCorner _ = boundingBox s
    ]
  where
    flood = floodFill (S.fromList . filter (`S.member` s0) . fullNeighbs)
          . S.singleton

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

-- | A map of a set of "on" points (for a 4x6 grid) to the letter they
-- represent.
newtype LetterMap = LetterMap { getLetterMap :: Map (Set Point) Char }
  deriving (Show, Eq, Ord, Semigroup, Monoid, Generic, Data)

instance Lift LetterMap

lookupLetterMap :: Set Point -> LetterMap -> Maybe Char
lookupLetterMap k = M.lookup k . getLetterMap

toLetterMap :: String -> String -> LetterMap
toLetterMap ls = LetterMap
               . M.fromList
               . zipWith (flip (,)) ls
               . contiguousShapesBy v2x
               . parseAsciiMapV2 (S.singleton '#')

-- | Parse a raw ASCII map into a set of points, usable with
-- 'parseLettersV2'.
parseAsciiMapV2
    :: Set Char             -- ^ characters to use as "on"/included
    -> String               -- ^ raw map
    -> Set Point
parseAsciiMapV2 c = zipWithFold (\i -> zipWithFold (\j x ->
                        if x `S.member` c
                          then S.singleton (V2 j i)
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

-- | Seen in 2016 Day 8, 2019 Day 8 and 11.
rawLetterforms1 :: (String, String)
rawLetterforms1 = ("ABCEFGHJKLPRUYZ", drop 1 [here|
.##..###...##..####.####..##..#..#...##.#..#.#....###..###..#..#.#...#.####
#..#.#..#.#..#.#....#....#..#.#..#....#.#.#..#....#..#.#..#.#..#.#...#....#
#..#.###..#....###..###..#....####....#.##...#....#..#.#..#.#..#..#.#....#.
####.#..#.#....#....#....#.##.#..#....#.#.#..#....###..###..#..#...#....#..
#..#.#..#.#..#.#....#....#..#.#..#.#..#.#.#..#....#....#.#..#..#...#...#...
#..#.###...##..####.#.....###.#..#..##..#..#.####.#....#..#..##....#...####
|])

-- | Based on
-- <https://gist.github.com/usbpc/5fa0be48ad7b4b0594b3b8b029bc47b4>.
--
-- Seen in 2018 Day 10
rawLetterforms2 :: (String, String)
rawLetterforms2 = ("ABCEFGHJKLNPRXZ", drop 1 [here|
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
