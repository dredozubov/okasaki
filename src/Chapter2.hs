{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}

module Chapter2 where

import           Chapter2.Exercise_2_1
import           Chapter2.Exercise_2_2 hiding (main)
import           Chapter2.Exercise_2_3 hiding (main)
import           Chapter2.Exercise_2_4 hiding (main)
import           Chapter2.Exercise_2_5
import           Chapter2.Exercise_2_6
import           UnbalancedSet
import           UnbalancedSet.Lazy as L
import           UnbalancedSet.Strict as S

import           Control.DeepSeq
import           Control.Monad
import           Data.Coerce
import           Data.List
import           Data.Proxy

import           Criterion.Main
import           Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range


main :: IO ()
main = do
  testResult <- checkParallel tests2_4
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
      , L.memberBenchmark e atl wtl (Proxy @(Ex_2_3))
      , S.memberBenchmarkStrict e ats wts (Proxy @(S.TreeStrict))
      , S.memberBenchmarkStrict e ats wts (Proxy @(Ex_2_3Strict))
      , L.insertBenchmark e atl wtl (Proxy @(L.Tree))
      , L.insertBenchmark e atl wtl (Proxy @(Ex_2_2))
      , L.insertBenchmark e atl wtl (Proxy @(Ex_2_4))
      , S.insertBenchmarkStrict e ats wts (Proxy @(S.TreeStrict))
      , S.insertBenchmarkStrict e ats wts (Proxy @(Ex_2_2Strict))
      , S.insertBenchmarkStrict e ats wts (Proxy @(Ex_2_4Strict))
      ]
