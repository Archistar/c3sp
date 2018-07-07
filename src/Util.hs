module Util(safeRem, noOrdEq, toEither, head', dropPrefix, maybeAdjust, find', atLeast) where

import Control.Arrow
import Data.Char
import Data.Scientific

-- Verifies whether two list are the same while ignoring the order.
noOrdEq :: Eq a => [a] -> [a] -> Bool
noOrdEq x y
  | length x /= length y = False
  | otherwise = help x y
    where
      help [] [] = True
      help (a:as) b = case safeRem a b of
                        Nothing -> False
                        Just b' -> help as b'

-- Removes first occurrence of the element from list. If the element is not in
-- the result is Nothing
safeRem :: Eq a => a -> [a] -> Maybe [a]
safeRem el [] = Nothing
safeRem el (x:xs)
  | el == x   = Just xs
  | otherwise = fmap (x:) $ safeRem el xs

toEither :: b -> Maybe a -> Either b a
toEither b Nothing  = Left b
toEither _ (Just a) = Right a

head' :: [a] -> (a, [a])
head' [] = error "Empty list."
head' (x:xs) = (x, xs)

find' :: (a -> Bool) -> [a] -> Maybe (a, [a])
find' _ []     = Nothing
find' f (x:xs) = if f x
    then Just (x, xs)
    else fmap (second (x:)) $ find' f xs

dropPrefix :: Int -> String -> String
dropPrefix pl = uncurry (:) . first toLower . head' . drop pl

maybeAdjust :: Maybe (a -> a) -> a -> a
maybeAdjust Nothing  x = x
maybeAdjust (Just f) x = f x

-- Calculates probability that at lest n of the given probabilities are true at the same time.
atLeast :: (Eq a, Ord a, Num a, Num b) => a -> [b] -> b
atLeast x = sum . map product . atLeastTree x

atLeastTree :: (Eq a, Ord a, Num a, Num b) => a -> [b] -> [[b]]
atLeastTree 0 _ = [[1]]
atLeastTree c lst
  | c < 0 = error "Minimum amount of required true predicates can't be negative."
  | null lst = []
  | fromIntegral (length lst) < c = []
  | fromIntegral (length lst) == c = [lst]
  | otherwise = map (x:) (atLeastTree (c-1) xs) ++ map ((1-x):) (atLeastTree c xs)
    where x = head lst
          xs = tail lst
