{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GADTs #-}

module SpatialReferenceSpec (main, spec) where

import SpatialReference

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative ((<$>), (<*>), pure)
#endif

import Data.Aeson
import Data.Maybe (fromJust)
import Data.Proxy

import GHC.Generics

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

main :: IO ()
main = hspec spec

spec :: Spec
spec = do

  describe "reify and reflect" $ do
    prop "reifyCrs c reflectCrs == c" $ \c -> reifyCrs c reflectCrs == c

  describe "type equality" $ do

    prop "sameCrs provides witness for the same term reified twice" $ \c ->
      let areSameTypes :: a -> a -> Bool
          areSameTypes _ _ = True
      in reifyCrs c $ \p1 ->
          reifyCrs c $ \p2 ->
            case sameCrs p1 p2 of
              Just Refl -> areSameTypes p1 p2
              _         -> False

    prop "sameCrs provides witness iff reified terms are equal" $ \c1 c2 ->
      reifyCrs c1 $ \p1 ->
        reifyCrs c2 $ \p2 ->
          case sameCrs p1 p2 of
            Just Refl -> c1 == c2
            _         -> c1 /= c2

  describe "geojson de/serialization" $ do

    describe "Crs" $
      prop "eitherDecode (encode c) == Right c" $ \(c :: CrsObject) ->
        eitherDecode (encode c) == Right c

    describe "WithSomeCrs" $ do
      prop "eitherDecode (encode c) == Right c" $ \(c :: WithSomeCrs Thing) ->
        eitherDecode (encode c) == Right c

  describe "unWithSomeCrs" $ do
    prop "returns Just iff crs matches" $ \(thing :: WithSomeCrs Thing) crs ->
      reifyCrs crs $ \(Proxy :: Proxy crs) ->
        case unWithSomeCrs thing :: Maybe (Thing crs) of
          Just _  -> crs == getCrs thing
          Nothing -> crs /= getCrs thing

-- A wrapper because aeson requires a toplevel object and a NoCrs must be
-- encoded as a null
newtype CrsObject = CrsObject Crs deriving (Eq, Show, Arbitrary)

instance ToJSON CrsObject where
  toJSON (CrsObject c) = object ["crs" .= c]

instance FromJSON CrsObject where
  parseJSON = withObject "expected an object" (\o -> CrsObject <$> o .: "crs")

data Thing crs = Thing { thingA :: Int, thingB :: String}
  deriving (Eq, Show, Generic)
instance ToJSON (Thing crs)
instance FromJSON (Thing crs)

instance Arbitrary (Thing  crs) where
  arbitrary = Thing <$> arbitrary <*> arbitrary

instance Arbitrary (WithSomeCrs Thing) where
  arbitrary = do
    crs <- arbitrary
    reifyCrs crs $ \(_ :: Proxy crs) -> do
      thing :: Thing crs <- arbitrary
      return (WithSomeCrs thing)

instance Arbitrary Crs where
  arbitrary = oneof [
      namedCrs    <$> arbitrary
    , fromJust    <$> (codedCrs <$> arbitrary <*> (getPositive <$> arbitrary))
    , linkedCrs   <$> arbitrary <*> arbitrary
    , pure noCrs
    ]
