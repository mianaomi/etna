{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use infix" #-}

module RBT where

import Data.Either
import Data.List qualified as L
import Test.QuickCheck

-- Based on SmallCheck implementation (based on Okasaki 1999),
-- but in the style of How to Specify It.

data Color = R | B
  deriving (Eq, Ord, Read, Show)

data Tree k v
  = E
  | T Color (Tree k v) k v (Tree k v)
  deriving (Eq, Ord, Read, Show)

newtype Key = Key Int
  deriving (Eq, Ord, Read, Show)

newtype Val = Val Bool
  deriving (Eq, Ord, Read, Show)

type RBT = Tree Key Val

instance Arbitrary RBT where
  arbitrary = do
    kvs <- arbitrary :: Gen [(Key, Val)]
    return $ foldr (uncurry insert) E kvs
    where
      -- Correct implementation.
      insert :: (Ord k) => k -> v -> Tree k v -> Tree k v
      insert k vk s = blacken (ins k vk s)
        where
          ins x vx E = T R E x vx E
          ins x vx (T rb a y vy b)
            | x < y = balance rb (ins x vx a) y vy b
            | x > y = balance rb a y vy (ins x vx b)
            | otherwise = T rb a y vx b

      blacken :: Tree k v -> Tree k v
      blacken E = E
      blacken (T _ a k v b) = T B a k v b

      balance :: Color -> Tree k v -> k -> v -> Tree k v -> Tree k v
      balance B (T R (T R a x vx b) y vy c) z vz d = T R (T B a x vx b) y vy (T B c z vz d)
      balance B (T R a x vx (T R b y vy c)) z vz d = T R (T B a x vx b) y vy (T B c z vz d)
      balance B a x vx (T R (T R b y vy c) z vz d) = T R (T B a x vx b) y vy (T B c z vz d)
      balance B a x vx (T R b y vy (T R c z vz d)) = T R (T B a x vx b) y vy (T B c z vz d)
      balance rb a x vx b = T rb a x vx b

instance Arbitrary Key where
  arbitrary = Key <$> arbitrary

instance Arbitrary Val where
  arbitrary = Val <$> arbitrary

insert :: (Ord k) => k -> v -> Tree k v -> Either Error (Tree k v)
insert x vx s = return $ blacken (ins x vx s)
  where
    ins x vx E =
      {-! -}
      --   T R E x vx E
      {-!! miscolor_insert -}
      -- {-!
      T B E x vx E
    -- -}
    ins x vx (T rb a y vy b)
      {-! -}
      | x < y = balance rb (ins x vx a) y vy b
      | x > y = balance rb a y vy (ins x vx b)
      | otherwise = T rb a y vx b

{-!! insert_1 -}
{-!
= T R E x vx E
-}
{-!! insert_2 -}
{-!
\| x < y = balance rb (ins x vx a) y vy b
\| otherwise = T rb a y vx b
-}
{-!! insert_3 -}
{-!
\| x < y = balance rb (ins x vx a) y vy b
\| x > y = balance rb a y vy (ins x vx b)
\| otherwise = T rb a y vy b
-}
{-!! no_balance_insert_1 -}
{-!
\| x < y = T rb (ins x vx a) y vy b
\| x > y = balance rb a y vy (ins x vx b)
\| otherwise = T rb a y vx b
-}
{-!! no_balance_insert_2 -}
{-!
\| x < y = balance rb (ins x vx a) y vy b
\| x > y = T rb a y vy (insert x vx b)
\| otherwise = T rb a y vx b
-}

----------

-- Based on https://www.cs.kent.ac.uk/people/staff/smk/redblack/Untyped.hs

data Error = IE -- invariant error
  deriving (Eq, Ord, Show)

delete :: (Ord k) => k -> Tree k v -> Either Error (Tree k v)
delete x t =
  {-! -}
  blacken <$> del t
  where
    {-!! miscolor_delete -}
    {-!
    del t
    -}

    del E = return E
    del (T _ a y vy b)
      {-! -}
      | x < y = delLeft a y vy b
      | x > y = delRight a y vy b
      | otherwise = join a b
    {-!! delete_4 -}
    {-!
    \| x < y = del a
    \| x > y = del b
    \| otherwise = join a b
    -}
    {-!! delete_5 -}
    {-!
    \| x > y = delLeft a y vy b
    \| x < y = delRight a y vy b
    \| otherwise = join a b
    -}

    delLeft a@(T B _ _ _ _) y vy b = do
      a' <- del a
      balLeft a' y vy b
    delLeft a y vy b = do
      a' <- del a
      return $ T R a' y vy b

    delRight a y vy b@(T B _ _ _ _) = balRight a y vy =<< del b
    delRight a y vy b = T R a y vy <$> del b

balLeft :: Tree k v -> k -> v -> Tree k v -> Either Error (Tree k v)
balLeft (T R a x vx b) y vy c = return $ T R (T B a x vx b) y vy c
balLeft bl x vx (T B a y vy b) = return $ balance B bl x vx (T R a y vy b)
balLeft bl x vx (T R (T B a y vy b) z vz c) =
  {-! -}
  do
    c' <- redden c
    return $ T R (T B bl x vx a) y vy (balance B b z vz c')
{-!! miscolor_balLeft -}
{-!
return $ T R (T B bl x vx a) y vy (balance B b z vz c)
-}
balLeft _ _ _ _ = Left IE

balRight :: Tree k v -> k -> v -> Tree k v -> Either Error (Tree k v)
balRight a x vx (T R b y vy c) = return $ T R a x vx (T B b y vy c)
balRight (T B a x vx b) y vy bl = return $ balance B (T R a x vx b) y vy bl
balRight (T R a x vx (T B b y vy c)) z vz bl =
  {-! -}
  do
    a' <- redden a
    return $ T R (balance B a' x vx b) y vy (T B c z vz bl)
{-!! miscolor_balRight -}
{-!
return $ T R (balance B a x vx b) y vy (T B c z vz bl)
-}
balRight _ _ _ _ = Left IE

join :: Tree k v -> Tree k v -> Either Error (Tree k v)
join E a = return a
join a E = return a
join (T R a x vx b) (T R c y vy d) = do
  t' <- join b c
  case t' of
    T R b' z vz c' ->
      {-! -}
      return $ T R (T R a x vx b') z vz (T R c' y vy d)
    {-!! miscolor_join_1 -}
    {-!
    return $ T R (T B a x vx b') z vz (T B c' y vy d)
    -}
    bc -> return $ T R a x vx (T R bc y vy d)
join (T B a x vx b) (T B c y vy d) = do
  t' <- join b c
  case t' of
    T R b' z vz c' ->
      {-! -}
      return $ T R (T B a x vx b') z vz (T B c' y vy d)
    {-!! miscolor_join_2 -}
    {-!
    return $ T R (T R a x vx b') z vz (T R c' y vy d)
    -}
    bc -> balLeft a x vx (T B bc y vy d)
join a (T R b x vx c) = do
  t' <- join a b
  return $ T R t' x vx c
join (T R a x vx b) c = T R a x vx <$> join b c

----------

-- Used for insert and delete.

blacken :: Tree k v -> Tree k v
blacken E = E
blacken (T _ a x vx b) = T B a x vx b

redden :: Tree k v -> Either Error (Tree k v)
redden (T B a x vx b) = return $ T R a x vx b
redden _ = Left IE

balance :: Color -> Tree k v -> k -> v -> Tree k v -> Tree k v
{-! -}
balance B (T R (T R a x vx b) y vy c) z vz d = T R (T B a x vx b) y vy (T B c z vz d)
{-!! swap_cd -}
{-!
balance B (T R (T R a x vx b) y vy c) z vz d = T R (T B a x vx b) y vy (T B d z vz c)
-}
balance B (T R a x vx (T R b y vy c)) z vz d = T R (T B a x vx b) y vy (T B c z vz d)
{-! -}
balance B a x vx (T R (T R b y vy c) z vz d) = T R (T B a x vx b) y vy (T B c z vz d)
{-!! swap_bc -}
{-!
balance B a x vx (T R (T R b y vy c) z vz d) = T R (T B a x vx c) y vy (T B b z vz d)
-}
balance B a x vx (T R b y vy (T R c z vz d)) = T R (T B a x vx b) y vy (T B c z vz d)
balance rb a x vx b = T rb a x vx b

-- Based on SmallCheck implementation (based on Okasaki 1999),
-- but in the style of How to Specify It.

isRBT :: (Ord k) => Tree k v -> Bool
isRBT t = isBST t && consistentBlackHeight t && noRedRed t && blackRoot t

isBST :: (Ord k) => Tree k v -> Bool
isBST E = True
isBST (T _ a x _ b) =
  -- Difference from SC: don't allow repeated keys.
  every (< x) a && every (> x) b && isBST a && isBST b
  where
    every p E = True
    every p (T _ a x _ b) = p x && every p a && every p b

-- "No red node has a red parent."
noRedRed :: Tree k v -> Bool
noRedRed E = True
noRedRed (T B a _ _ b) = noRedRed a && noRedRed b
noRedRed (T R a _ _ b) = blackRoot a && blackRoot b && noRedRed a && noRedRed b
  where
    blackRoot (T R _ _ _ _) = False
    blackRoot _ = True

-- "Every path from the root to an empty node contains the same number of black nodes."
consistentBlackHeight :: Tree k v -> Bool
consistentBlackHeight = fst . go
  where
    go E = (True, 1)
    go (T rb a x _ b) =
      (aBool && bBool && aHeight == bHeight, aHeight + isBlack rb)
      where
        (aBool, aHeight) = go a
        (bBool, bHeight) = go b

        isBlack R = 0
        isBlack B = 1

blackRoot :: Tree k v -> Bool
blackRoot E = True
blackRoot (T B _ _ _ _) = True
blackRoot _ = False

toList :: Tree k v -> [(k, v)]
toList E = []
toList (T _ l k v r) =
  toList l ++ [(k, v)] ++ toList r

find :: (Ord k) => k -> Tree k v -> Maybe v
find _ E = Nothing
find x (T _ l y vy r)
  | x < y = find x l
  | x > y = find x r
  | otherwise = Just vy

----------

----------

-- Validity properties.

prop_InsertValid :: (RBT, Key, Val) -> Property
prop_InsertValid (t, k, v) =
  isRBT t ==> fromRight False (isRBT <$> insert k v t)

prop_DeleteValid :: (RBT, Key) -> Property
prop_DeleteValid (t, k) =
  isRBT t ==> fromRight False (isRBT <$> delete k t)

----------

-- Postcondition properties.

prop_InsertPost :: (RBT, Key, Key, Val) -> Property
prop_InsertPost (t, k, k', v) =
  isRBT t
    ==> (find k' <$> insert k v t)
    == return (if k == k' then Just v else find k' t)

prop_DeletePost :: (RBT, Key, Key) -> Property
prop_DeletePost (t, k, k') =
  isRBT t
    ==> (find k' <$> delete k t)
    == return (if k == k' then Nothing else find k' t)

----------

-- Model-based properties.

prop_InsertModel :: (RBT, Key, Val) -> Property
prop_InsertModel (t, k, v) =
  isRBT t
    ==> (toList <$> insert k v t)
    == return (L.insert (k, v) (deleteKey k $ toList t))

prop_DeleteModel :: (RBT, Key) -> Property
prop_DeleteModel (t, k) =
  isRBT t
    ==> (toList <$> delete k t)
    == return (deleteKey k (toList t))

deleteKey :: (Ord k) => k -> [(k, v)] -> [(k, v)]
deleteKey k = filter ((/= k) . fst)

----------

-- Metamorphic properties.

prop_InsertInsert :: (RBT, Key, Key, Val, Val) -> Property
prop_InsertInsert (t, k, k', v, v') =
  isRBT t
    ==> (insert k v =<< insert k' v' t)
    =~= if k == k' then insert k v t else insert k' v' =<< insert k v t

prop_InsertDelete :: (RBT, Key, Key, Val) -> Property
prop_InsertDelete (t, k, k', v) =
  isRBT t
    ==> (insert k v =<< delete k' t)
    =~= if k == k' then insert k v t else delete k' =<< insert k v t

prop_DeleteInsert :: (RBT, Key, Key, Val) -> Property
prop_DeleteInsert (t, k, k', v') =
  isRBT t
    ==> (delete k =<< insert k' v' t)
    =~= if k == k' then delete k t else insert k' v' =<< delete k t

prop_DeleteDelete :: (RBT, Key, Key) -> Property
prop_DeleteDelete (t, k, k') =
  isRBT t
    ==> (delete k =<< delete k' t)
    =~= (delete k' =<< delete k t)

(=~=) :: Either Error (Tree Key Val) -> Either Error (Tree Key Val) -> Bool
(Right t1) =~= (Right t2) = toList t1 == toList t2
_ =~= _ = False

----------

sizeRBT :: RBT -> Int
sizeRBT = length . toList