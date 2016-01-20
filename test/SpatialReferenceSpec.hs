{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE GADTs #-}

module SpatialReferenceSpec (main, spec) where

import SpatialReference

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative ((<$>), (<*>), pure)
#endif

import Data.Aeson
import Data.Maybe (fromJust)

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
    prop "eitherDecode (encode c) == Right c" $ \(c :: CrsObject) ->
      eitherDecode (encode c) == Right c

-- A wrapper because aeson requires a toplevel object and a NoCrs must be
-- encoded as a null
newtype CrsObject = CrsObject Crs deriving (Eq, Show, Arbitrary)

instance ToJSON CrsObject where
  toJSON (CrsObject c) = object ["crs" .= c]

instance FromJSON CrsObject where
  parseJSON = withObject "expected an object" (\o -> CrsObject <$> o .: "crs")



instance Arbitrary Crs where
  arbitrary = oneof [
      namedCrs    <$> arbitrary
    , fromJust    <$> (codedCrs <$> arbitrary <*> (getPositive <$> arbitrary))
    , linkedCrs   <$> arbitrary <*> arbitrary
    , pure noCrs
    ]
