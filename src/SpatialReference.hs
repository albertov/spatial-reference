{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}

module SpatialReference (
    KnownCrs

  , Crs
  , Named
  , Coded
  , Linked
  , NoCrs
  , Epsg
  , SrOrg

  , CrsView (..)

  , crs

  , namedCrs
  , codedCrs
  , linkedCrs
  , noCrs

  , reifyCrs
  , reflectCrs
  , sameCrs

  -- Re-exports
  , (:~:)(Refl)
) where


import Data.Aeson         ( ToJSON(toJSON), FromJSON(parseJSON)
                          , Value(Null,Object), withText, withObject
                          , withScientific, object, (.=), (.:), (.:?))
import Data.Proxy         (Proxy(Proxy))
import Data.Scientific    (floatingOrInteger)
import Data.Text          (Text, unpack)
import Data.Type.Equality ((:~:)(Refl))
import Control.Monad      (mzero)
import GHC.TypeLits       ( KnownNat, KnownSymbol, Nat, Symbol
                          , SomeNat(SomeNat), SomeSymbol(SomeSymbol)
                          , natVal, symbolVal, someNatVal, someSymbolVal)
import Unsafe.Coerce      (unsafeCoerce)


--
-- > Type level 'Crs' tags for use as phantom types. Requires the 'DataKinds'
--   extension for type-level 'Symbol's and 'Nat's-
--

-- | A named 'Crs' type. The name is expected to be a OGC CRS URNs such as
--   "urn:ogc:def:crs:OGC:1.3:CRS84"
--
-- >>> Proxy :: Proxy (Named "urn:ogc:def:crs:OGC:1.3:CRS84")
data Named     (name  :: Symbol)

-- | A coded (EPSG, SR-ORG, etc..) 'Crs' type.
--
-- >>> Proxy :: Proxy (Coded "epsg" 23030)
data Coded     (type_ :: Symbol)       (code  :: Nat)

-- | A linked 'Crs' type.
--
-- >>> Proxy :: Proxy (Linked ('Just "proj4") "....")
data Linked    (type_ :: Maybe Symbol) (href :: Symbol)

-- | The absence of a spatial reference
--
-- >>> Proxy :: Proxy NoCrs
data NoCrs

-- | A type synonym of a EPSG coded 'Crs'
--
-- >>> Proxy :: Proxy (EPSG 23030)
type Epsg  code = Coded "epsg"   code

-- | A type synonym of a SR-ORG coded 'Crs'
--
-- >>> Proxy :: Proxy (SrOrg 35)
type SrOrg code = Coded "sr-org" code

-- | The term level 'Crs'
--
newtype Crs = Crs { crs :: CrsView }
  deriving (Eq, Ord)

instance Show Crs where show = show . crs


data CrsView
  = Named     !String
  | Coded     !String         !Int
  | Linked    !(Maybe String) !String
  | NoCrs
  deriving (Eq, Show, Ord)
-- TODO: Implement a Show instance that normalizes so equivalent CRSs compare
--       equal

type Href = String


--
-- Smart constructors
--

namedCrs :: String -> Crs
namedCrs = Crs . Named
{-# INLINE namedCrs #-}

codedCrs :: String -> Int -> Maybe Crs
codedCrs type_ code | code >= 0 = Just (Crs (Coded type_ code))
codedCrs _    _                 = Nothing
{-# INLINE codedCrs #-}

linkedCrs :: Maybe String -> Href -> Crs
linkedCrs type_ href = Crs (Linked type_ href)
{-# INLINE linkedCrs #-}

noCrs :: Crs
noCrs = Crs NoCrs
{-# INLINE noCrs #-}


--
-- Reification of term levels to types and reflection from types to terms
--

class KnownCrs (c :: *) where
  _reflectCrs :: proxy c -> Crs


instance KnownSymbol name => KnownCrs (Named name) where
  _reflectCrs _ = Crs (Named (symbolVal (Proxy :: Proxy name)))

instance ( KnownNat code
         , KnownSymbol type_
         ) => KnownCrs (Coded type_ code) where
  _reflectCrs _ = Crs (Coded (symbolVal (Proxy :: Proxy type_))
                             (fromIntegral (natVal (Proxy :: Proxy code))))

instance ( KnownSymbol href
         , KnownSymbol type_
         ) => KnownCrs (Linked ('Just type_) href) where
  _reflectCrs _ = Crs (Linked  (Just (symbolVal (Proxy :: Proxy type_)))
                               (symbolVal (Proxy :: Proxy href)))

instance KnownSymbol href => KnownCrs (Linked 'Nothing href) where
  _reflectCrs _ = Crs (Linked Nothing (symbolVal (Proxy :: Proxy href)))


instance KnownCrs NoCrs where
  _reflectCrs _ = Crs NoCrs


reflectCrs :: KnownCrs c => proxy c -> Crs
reflectCrs = _reflectCrs
{-# INLINE reflectCrs #-}

reifyCrs :: forall a. Crs -> (forall c. KnownCrs c => Proxy c -> a) -> a
reifyCrs (Crs c) f = case c of
  Named name ->
    case someSymbolVal name of
      SomeSymbol (Proxy :: Proxy name) -> f (Proxy :: Proxy (Named name))

  Coded type_ code ->
    case someSymbolVal type_ of
      SomeSymbol (Proxy :: Proxy type_) ->
        case someNatVal (fromIntegral (code)) of
          Just (SomeNat (Proxy :: Proxy code)) ->
            f (Proxy :: Proxy (Coded type_ code))
          _ -> error "reflectCrs: negative epsg code. this should never happen"

  Linked mType href ->
    case someSymbolVal href of
      SomeSymbol (Proxy :: Proxy href) ->
        case mType of
          Just type_ ->
            case someSymbolVal type_ of
              SomeSymbol (Proxy :: Proxy type_) ->
                f (Proxy :: Proxy (Linked ('Just type_) href))
          Nothing ->
                f (Proxy :: Proxy (Linked 'Nothing href))

  NoCrs -> f (Proxy :: Proxy NoCrs)
{-# INLINE reifyCrs #-}

sameCrs :: (KnownCrs a, KnownCrs b) => Proxy a -> Proxy b -> Maybe (a :~: b)
sameCrs a b | reflectCrs a == reflectCrs b = Just (unsafeCoerce Refl)
sameCrs _ _                                = Nothing
{-# INLINE sameCrs #-}




instance ToJSON Crs where
  toJSON (Crs crs_) = case crs_ of
    Named s ->
      object [ "type"       .= ("name" :: Text)
             , "properties" .= object ["name" .= s]]
    Coded t c ->
      object [ "type"       .= t
             , "properties" .= object ["code" .= c]]
    Linked (Just t) h ->
      object [ "type"       .= ("link" :: Text)
             , "properties" .= object ["href" .= h, "type" .= t]]
    Linked Nothing h ->
      object [ "type"       .= ("link" :: Text)
             , "properties" .= object ["href" .= h]]
    NoCrs -> Null

instance FromJSON Crs where
  parseJSON (Object o) = withProperties $ \props -> withType $ \case
    "name" -> namedCrs  <$> props .:  "name"
    "link" -> linkedCrs <$> props .:? "type" <*> props .: "href"
    typ    -> do
      code <- props .: "code"
      flip (withScientific "crs: expected an integeral code") code $
          maybe (fail "crs: expected a non-negative code") return
        . codedCrs (unpack typ) . either (round :: Double -> Int) id
        . floatingOrInteger
    where
      withProperties f =
        o .: "properties" >>= withObject "properties must be an object" f
      withType f =
        o .: "type" >>= withText "crs: type must be a string" f
  parseJSON Null = return noCrs
  parseJSON _    = mzero
