module ListExtra where

import qualified Data.List as L

unfoldlL :: (a -> Maybe (a, b)) -> a -> [b]
unfoldlL f = go []
  where
    go xs z = case f z of
      Nothing -> xs
      Just (z',x) -> go (x:xs) z'

-- In base since 4.19
lookupL :: Int -> [a] -> Maybe a
lookupL i xs =
  foldr (\x k j -> if j == 0 then Just x else k (j-1)) (const Nothing) xs i

-- In base since 4.19
unsnocL :: [a] -> Maybe ([a], a)
unsnocL =
  foldr (\x -> Just . maybe ([], x) (\(~(a, b)) -> (x : a, b))) Nothing

takeEndL :: Int -> [a] -> [a]
takeEndL n xs = foldr (\_ k -> k . tail) id (L.drop n xs) xs

dropEndL :: Int -> [a] -> [a]
dropEndL n xs = L.zipWith const xs (L.drop n xs)

adjustL :: (a -> a) -> Int -> [a] -> [a]
adjustL f i xs
  | i < 0 = xs
  | (l,x:r) <- L.splitAt i xs = l ++ f x : r
  | otherwise = xs

insertAtL :: Int -> a -> [a] -> [a]
insertAtL i x xs = L.take i xs ++ [x] ++ L.drop i xs

deleteAtL :: Int -> [a] -> [a]
deleteAtL i xs = L.take i xs ++ L.drop (i+1) xs

spanEndL :: (a -> Bool) -> [a] -> ([a], [a])
spanEndL f xs = case L.span f (L.reverse xs) of
  (ys,zs) -> (L.reverse zs, L.reverse ys)

infixIndicesL :: Eq a => [a] -> [a] -> [Int]
infixIndicesL xs ys =
  [i | (i,ys') <- L.zip [0..] (L.tails ys), xs `L.isPrefixOf` ys']

chunksOfL :: Int -> [a] -> [[a]]
chunksOfL c xs = case L.splitAt c xs of
  ([], _) -> []
  (ys, zs) -> ys : chunksOfL c zs
