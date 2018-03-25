{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module LeftistHeap where

import           Heap

import           Control.DeepSeq
import           Control.Monad
import           Data.Coerce
import           Data.List
import           Data.Proxy
import           GHC.Generics

import           Criterion.Main
import           Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range


data LHeap a = E | T Int (LHeap a) a (LHeap a) deriving (Show, Eq, Generic)

instance NFData a => NFData (LHeap a)

rank :: LHeap a -> Int
rank E           = 0
rank (T i _ _ _) = i

makeT :: Ord a => a -> LHeap a -> LHeap a -> LHeap a
makeT x a b = if rankA > rankB
  then T (rankB + 1) a x b
  else T (rankA + 1) b x a
  where
    rankA = rank a
    rankB = rank b

merge :: Ord a => LHeap a -> LHeap a -> LHeap a
merge E h                               = h
merge h E                               = h
merge h1@(T _ a1 x b1) h2@(T _ a2 y b2) =
  if x <= y
  then makeT x a1 (merge b1 h2)
  else makeT y a2 (merge b2 h1)

instance Ord a => Heap LHeap a where
  hEmpty = E
  hIsEmpty E = True
  hIsEmpty _ = False
  hInsert e h = hMerge (T 1 E e E) h
  hFindMin E = Nothing
  hFindMin (T _ _ x _) = Just x
  hDeleteMin E = Nothing
  hDeleteMin (T _ a _ b) = Just $ hMerge a b
  hMerge = merge
