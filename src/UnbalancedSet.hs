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

module UnbalancedSet
  ( toStrict
  , module UnbalancedSet.Common
  ) where

import           UnbalancedSet.Common
import           UnbalancedSet.Lazy as L
import           UnbalancedSet.Strict as S

import           Control.DeepSeq
import           Control.Monad
import           Data.List
import           Data.Proxy

import           Criterion.Main
import           Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range


toStrict :: Ord a => L.Tree a -> S.TreeStrict a
toStrict L.E         = S.E
toStrict (L.T a x b) = (S.T (toStrict a) x (toStrict b))

main :: IO ()
main = do
  testResult <- do
    lr <- checkParallel L.tests
    sr <- checkParallel S.tests
    pure $ lr && sr
  when testResult $ do
    e <- Gen.sample $ Gen.int (Range.linear 0 10000)
    l <- replicateM 1000 $ do
      Gen.sample $ Gen.list (Range.linear 0 100) (Gen.int (Range.linear 0 10000))
    let
      atl = force $! L.fromList <$> l
      wtl = force $! L.fromList . sort <$> l
      ats = force $!! toStrict <$> atl
      wts = force $!! toStrict <$> wtl
    defaultMain
      [ L.memberBenchmark e atl wtl (Proxy @(L.Tree))
      , L.insertBenchmark e atl wtl (Proxy @(L.Tree))
      , S.memberBenchmarkStrict e ats wts (Proxy @(S.TreeStrict))
      , S.insertBenchmarkStrict e ats wts (Proxy @(S.TreeStrict))
      ]
