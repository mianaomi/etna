{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# HLINT ignore "Use infix" #-}

module BST where

import Control.Applicative
import Data.Function
import qualified Data.List as L
import Test.QuickCheck

-- Implementation and bugs from "How to Specify It".

data Tree k v
  = E
  | T (Tree k v) k v (Tree k v)
  deriving (Eq, Ord, Read, Show)

newtype Key = Key Int
  deriving (Eq, Ord, Read, Show)

newtype Val = Val Bool
  deriving (Eq, Ord, Read, Show)

type BST = Tree Key Val

instance Arbitrary BST where
  arbitrary = do
    kvs <- arbitrary :: Gen [(Key, Val)]
    return $ foldr (uncurry insert) E kvs
    where
      -- Correct implementation.
      insert :: Key -> Val -> Tree Key Val -> Tree Key Val
      insert k v E = T E k v E
      insert k v (T l k' v' r)
        | k < k' = T (insert k v l) k' v' r
        | k > k' = T l k' v' (insert k v r)
        | otherwise = T l k' v r

instance Arbitrary Key where
  arbitrary = Key <$> arbitrary

instance Arbitrary Val where
  arbitrary = Val <$> arbitrary

----------

insert :: (Ord k) => k -> v -> Tree k v -> Tree k v
insert k v E = T E k v E
insert k v (T l k' v' r)
  {-! -}
  | k < k' = T (insert k v l) k' v' r
  | k > k' = T l k' v' (insert k v r)
  | otherwise = T l k' v r

{-!! insert_1 -}
{-!
= T E k v E
-}
{-!! insert_2 -}
{-!
\| k < k' = T (insert k v l) k' v' r
\| otherwise = T l k' v r
-}
{-!! insert_3 -}
{-!
\| k < k' = T (insert k v l) k' v' r
\| k > k' = T l k' v' (insert k v r)
\| otherwise = T l k' v' r
-}

----------

delete :: (Ord k) => k -> Tree k v -> Tree k v
delete _ E = E
delete k (T l k' v' r)
  {-! -}
  | k < k' = T (delete k l) k' v' r
  | k > k' = T l k' v' (delete k r)
  | otherwise = join l r

{-!! delete_4 -}
{-!
\| k < k' = delete k l
\| k > k' = delete k r
\| otherwise = join l r
-}
{-!! delete_5 -}
{-!
\| k > k' = T (delete k l) k' v' r
\| k < k' = T l k' v' (delete k r)
\| otherwise = join l r
-}

join :: Tree k v -> Tree k v -> Tree k v
join E r = r
join l E = l
join (T l k v r) (T l' k' v' r') =
  T l k v (T (join r l') k' v' r')

----------

union :: (Ord k) => Tree k v -> Tree k v -> Tree k v
union E r = r
union l E = l
{-! -}
-- union (T l k v r) t =
--   T (union l (below k t)) k v (union r (above k t))

{-!! union_6 -}
-- {-!
union (T l k v r) (T l' k' v' r') =
  T l k v (T (union r l') k' v' r')
-- -}
{-!! union_7 -}
{-!
union (T l k v r) (T l' k' v' r')
  | k == k'   = T (union l l') k v (union r r')
  | k < k'    = T l k v (T (union r l') k' v' r')
  | otherwise = union (T l' k' v' r') (T l k v r)
-}
{-!! union_8 -}
-- !
-- union (T l k v r) (T l' k' v' r')
--   | k == k'   = T (union l l') k v (union r r')
--   | k < k'    = T (union l (below k l')) k v
--                        (union r (T (above k l') k' v' r'))
--   | otherwise = union (T l' k' v' r') (T l k v r)


below :: (Ord k) => k -> Tree k v -> Tree k v
below _ E = E
below k (T l k' v r)
  | k <= k' = below k l
  | otherwise = T l k' v (below k r)

above :: (Ord k) => k -> Tree k v -> Tree k v
above _ E = E
above k (T l k' v r)
  | k >= k' = above k r
  | otherwise = T (above k l) k' v r

-- Properties from "How to Specify It".

isBST :: (Ord k) => Tree k v -> Bool
isBST E = True
isBST (T l k _ r) =
  isBST l
    && isBST r
    && all (< k) (keys l)
    && all (> k) (keys r)

keys :: Tree k v -> [k]
keys = map fst . toList

toList :: Tree k v -> [(k, v)]
toList E = []
toList (T l k v r) =
  toList l ++ [(k, v)] ++ toList r

find :: (Ord k) => k -> Tree k v -> Maybe v
find _ E = Nothing
find k (T l k' v' r)
  | k < k' = find k l
  | k > k' = find k r
  | otherwise = Just v'

----------

-- Validity properties.

prop_InsertValid :: (BST, Key, Val) -> Property
prop_InsertValid (t, k, v) =
  isBST t ==> isBST (insert k v t)

prop_DeleteValid :: (BST, Key) -> Property
prop_DeleteValid (t, k) =
  isBST t ==> isBST (delete k t)

prop_UnionValid :: (BST, BST) -> Property
prop_UnionValid (t1, t2) =
  isBST t1 && isBST t2 ==> isBST (t1 `union` t2)

----------

-- Postcondition properties.

prop_InsertPost :: (BST, Key, Key, Val) -> Property
prop_InsertPost (t, k, k', v) =
  isBST t
    ==> find k' (insert k v t)
    == if k == k' then Just v else find k' t

prop_DeletePost :: (BST, Key, Key) -> Property
prop_DeletePost (t, k, k') =
  isBST t
    ==> find k' (delete k t)
    == if k == k' then Nothing else find k' t

prop_UnionPost :: (BST, BST, Key) -> Property
prop_UnionPost (t, t', k) =
  isBST t
    ==> find k (t `union` t')
    == (find k t <|> find k t')

----------

-- Model-based properties.

prop_InsertModel :: (BST, Key, Val) -> Property
prop_InsertModel (t, k, v) =
  isBST t
    ==> toList (insert k v t)
    == L.insert (k, v) (deleteKey k $ toList t)

prop_DeleteModel :: (BST, Key) -> Property
prop_DeleteModel (t, k) =
  isBST t
    ==> toList (delete k t)
    == deleteKey k (toList t)

prop_UnionModel :: (BST, BST) -> Property
prop_UnionModel (t, t') =
  isBST t
    && isBST t'
      ==> toList (t `union` t')
      == L.sort (L.unionBy ((==) `on` fst) (toList t) (toList t'))

deleteKey :: (Ord k) => k -> [(k, v)] -> [(k, v)]
deleteKey k = filter ((/= k) . fst)

----------

-- Metamorphic properties.

prop_InsertInsert :: (BST, Key, Key, Val, Val) -> Property
prop_InsertInsert (t, k, k', v, v') =
  isBST t
    ==> insert k v (insert k' v' t)
    =~= if k == k' then insert k v t else insert k' v' (insert k v t)

prop_InsertDelete :: (BST, Key, Key, Val) -> Property
prop_InsertDelete (t, k, k', v) =
  isBST t
    ==> insert k v (delete k' t)
    =~= if k == k' then insert k v t else delete k' (insert k v t)

prop_InsertUnion :: (BST, BST, Key, Val) -> Property
prop_InsertUnion (t, t', k, v) =
  isBST t
    && isBST t'
      ==> insert k v (t `union` t')
      =~= union (insert k v t) t'

prop_DeleteInsert :: (BST, Key, Key, Val) -> Property
prop_DeleteInsert (t, k, k', v') =
  isBST t
    ==> delete k (insert k' v' t)
    =~= if k == k' then delete k t else insert k' v' (delete k t)

prop_DeleteDelete :: (BST, Key, Key) -> Property
prop_DeleteDelete (t, k, k') =
  isBST t
    ==> delete k (delete k' t)
    =~= delete k' (delete k t)

prop_DeleteUnion :: (BST, BST, Key) -> Property
prop_DeleteUnion (t, t', k) =
  isBST t
    && isBST t'
      ==> delete k (t `union` t')
      =~= union (delete k t) (delete k t')

prop_UnionDeleteInsert :: (BST, BST, Key, Val) -> Property
prop_UnionDeleteInsert (t, t', k, v) =
  isBST t
    && isBST t'
      ==> union (delete k t) (insert k v t')
      =~= insert k v (t `union` t')

prop_UnionUnionIdem :: BST -> Property
prop_UnionUnionIdem t =
  isBST t
    ==> union t t
    =~= t

prop_UnionUnionAssoc :: (BST, BST, BST) -> Property
prop_UnionUnionAssoc (t1, t2, t3) =
  isBST t1
    && isBST t2
    && isBST t3
      ==> union (t1 `union` t2) t3
      == union t1 (t2 `union` t3)

(=~=) :: Tree Key Val -> Tree Key Val -> Bool
t1 =~= t2 = toList t1 == toList t2

----------

sizeBST :: BST -> Int
sizeBST = length . toList
