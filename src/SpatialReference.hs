{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE GADTs #-}

module SpatialReference (
    KnownCrs

  , Crs
  , Named
  , Coded
  , Linked
  , NoCrs
  , Epsg
  , SrOrg

  , DiscardCrs (..)
  , HasCrs (..)
  , WithSomeCrs (..)
  , WithCrs (..)
  , withCrs
  , untagCrs

  , pattern Named
  , pattern Coded
  , pattern Epsg
  , pattern SrOrg
  , pattern Linked
  , pattern NoCrs

  , namedCrs
  , codedCrs
  , linkedCrs
  , noCrs
  , srOrgCrs
  , epsgCrs

  , reifyCrs
  , reflectCrs
  , sameCrs

  -- Re-exports
  , (:~:)(Refl)
) where


#if !MIN_VERSION_base(4,8,0)
import           Control.Applicative ((<$>), (<*>))
#endif
import           Data.Aeson          ( ToJSON(toJSON), FromJSON(parseJSON)
                                     , Value(Null,Object), withText, withObject
                                     , withScientific, object, (.=), (.:)
                                     , (.:?))
import qualified Data.HashMap.Strict as HM
import           Data.Proxy          (Proxy(Proxy))
import           Data.Scientific     (floatingOrInteger)
import           Data.Text           (Text, unpack)
import           Data.Type.Equality  ((:~:)(Refl))
import           Control.Monad       (mzero)
import           GHC.TypeLits        ( KnownNat, KnownSymbol, Nat, Symbol
                                     , SomeNat(SomeNat), SomeSymbol(SomeSymbol)
                                     , natVal, symbolVal, someNatVal
                                     , someSymbolVal)
import Unsafe.Coerce                 (unsafeCoerce)


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


-- | The term level 'Crs'. Use the smart constructors to build them.
data Crs
  = MkNamed   !String
  | MkCoded   !String         !Int
  | MkLinked  !(Maybe String) !String
  | MkNoCrs
  deriving (Eq, Show, Ord)

-- TODO: Implement a Show instance that normalizes so equivalent CRSs compare
--       equal

-- | A url for use with 'linkedCrs'
type Href = String

-- | A text describing the type of crs for use with 'linkedCrs'
type LinkType = String

--
-- Pattern synonyms to hide implementation details
--

-- | Pattern match on a 'namedCrs'
pattern Named  a   <- MkNamed a

-- | Pattern match on a 'codedCrs'
pattern Coded  a b <- MkCoded a b

-- | Pattern match on a 'linkedCrs'
pattern Linked a b <- MkLinked a b

-- | Pattern match on a 'noCrs'
pattern NoCrs      <- MkNoCrs

-- | Pattern match on a 'epsgCrs'
pattern Epsg     a <- MkCoded "epsg"   a

-- | Pattern match on a 'srOrgCrs'
pattern SrOrg    a <- MkCoded "sr-org" a


--
-- Smart constructors
--

-- | A named 'Crs' constructor
namedCrs :: String -> Crs
namedCrs = MkNamed
{-# INLINE namedCrs #-}

-- | A coded 'Crs' smart constructor. The code must be non-negative
codedCrs :: String -> Int -> Maybe Crs
codedCrs type_ code | code >= 0 = Just (MkCoded type_ code)
codedCrs _    _                 = Nothing
{-# INLINE codedCrs #-}

-- | A linked 'Crs' constructor.
linkedCrs :: Maybe LinkType -> Href -> Crs
linkedCrs = MkLinked
{-# INLINE linkedCrs #-}

-- | A null 'Crs' constructor
noCrs :: Crs
noCrs = MkNoCrs
{-# INLINE noCrs #-}

-- | A EPSG code 'Crs' constructor
epsgCrs :: Int -> Maybe Crs
epsgCrs = codedCrs "epsg"
{-# INLINE epsgCrs #-}

-- | A SR-ORG code 'Crs' constructpr
srOrgCrs :: Int -> Maybe Crs
srOrgCrs = codedCrs "sr-org"
{-# INLINE srOrgCrs #-}

--
-- Reification of term levels to types and reflection from types to terms
--

-- | The class of 'Crs's that can be reified to the type level and reflected
--   back
class KnownCrs (c :: *) where
  _reflectCrs :: proxy c -> Crs


instance KnownSymbol name => KnownCrs (Named name) where
  _reflectCrs _ = namedCrs (symbolVal (Proxy :: Proxy name))

instance ( KnownNat code
         , KnownSymbol type_
         ) => KnownCrs (Coded type_ code) where
  _reflectCrs _ = MkCoded (symbolVal (Proxy :: Proxy type_))
                          (fromIntegral (natVal (Proxy :: Proxy code)))

instance ( KnownSymbol href
         , KnownSymbol type_
         ) => KnownCrs (Linked ('Just type_) href) where
  _reflectCrs _ = linkedCrs (Just (symbolVal (Proxy :: Proxy type_)))
                            (symbolVal (Proxy :: Proxy href))

instance KnownSymbol href => KnownCrs (Linked 'Nothing href) where
  _reflectCrs _ = linkedCrs Nothing (symbolVal (Proxy :: Proxy href))


instance KnownCrs NoCrs where
  _reflectCrs _ = noCrs


-- | Reflect a 'Proxy' of a 'KnownCrs' back into a 'Crs' term.
reflectCrs :: KnownCrs c => proxy c -> Crs
reflectCrs = _reflectCrs
{-# INLINE reflectCrs #-}

-- | Reify a 'Crs' to a 'Proxy' of a 'KnownCrs' which can be used as a
--   phantom type for geo-spatial objects.
reifyCrs :: forall a. Crs -> (forall c. KnownCrs c => Proxy c -> a) -> a
reifyCrs c f = case c of
  MkNamed name ->
    case someSymbolVal name of
      SomeSymbol (Proxy :: Proxy name) -> f (Proxy :: Proxy (Named name))

  MkCoded type_ code ->
    case someSymbolVal type_ of
      SomeSymbol (Proxy :: Proxy type_) ->
        case someNatVal (fromIntegral (code)) of
          Just (SomeNat (Proxy :: Proxy code)) ->
            f (Proxy :: Proxy (Coded type_ code))
          _ -> error "reflectCrs: negative epsg code. this should never happen"

  MkLinked mType href ->
    case someSymbolVal href of
      SomeSymbol (Proxy :: Proxy href) ->
        case mType of
          Just type_ ->
            case someSymbolVal type_ of
              SomeSymbol (Proxy :: Proxy type_) ->
                f (Proxy :: Proxy (Linked ('Just type_) href))
          Nothing ->
                f (Proxy :: Proxy (Linked 'Nothing href))

  MkNoCrs -> f (Proxy :: Proxy NoCrs)
{-# INLINE reifyCrs #-}

-- | Provides a witness of the equality of two 'KnownCrs' types.
--   Pattern-match on the 'Just Refl' and the compiler will know that
--   the 'KnownCrs's carried by the proxies is the same type.
sameCrs :: (KnownCrs a, KnownCrs b) => Proxy a -> Proxy b -> Maybe (a :~: b)
sameCrs a b | reflectCrs a == reflectCrs b = Just (unsafeCoerce Refl)
sameCrs _ _                                = Nothing
{-# INLINE sameCrs #-}



class DiscardCrs o a | o -> a where
  discardCrs :: o -> a

class HasCrs o where
  crs :: o -> Crs


-- | A wrapper for something with an associated 'Crs' at the term level
data WithSomeCrs a = WithSomeCrs Crs a
  deriving (Eq, Show)

instance DiscardCrs (WithSomeCrs a) a where
  discardCrs (WithSomeCrs _ a) = a
  {-# INLINE discardCrs #-}

instance HasCrs (WithSomeCrs a) where
  crs (WithSomeCrs c _) = c
  {-# INLINE crs #-}




-- | A newtype wrapper for something with an associated 'KnownCrs' at the
--   type level
newtype WithCrs crs a = WithCrs { unWithCrs :: a }
  deriving (Eq, Show)

-- | Converts something 'WithCrs' at the type level to a 'WithSomeCrs' at the
--   term level
untagCrs :: forall a crs. KnownCrs crs
         => WithCrs crs a -> WithSomeCrs a
untagCrs t = WithSomeCrs (reflectCrs (Proxy :: Proxy crs)) (unWithCrs t)
{-# INLINE untagCrs #-}

-- | Reifies something 'WithSomeCrs' to a 'WithCrs' at the type level
withCrs
  :: forall a b.
  WithSomeCrs a -> (forall crs. KnownCrs crs => WithCrs crs a -> b) -> b
withCrs (WithSomeCrs c a) f =
  reifyCrs c (\(Proxy :: Proxy crs) -> f (WithCrs a :: WithCrs crs a))
{-# INLINE withCrs #-}

instance DiscardCrs (WithCrs crs a) a where
  discardCrs = unWithCrs
  {-# INLINE discardCrs #-}

instance KnownCrs crs => HasCrs (WithCrs crs a) where
  crs _ = reflectCrs (Proxy :: Proxy crs)
  {-# INLINE crs #-}

--
-- GeoJSON de/serialization
--

instance ToJSON Crs where
  toJSON = \case
    MkNamed s ->
      object [ "type"       .= ("name" :: Text)
             , "properties" .= object ["name" .= s]]
    MkCoded t c ->
      object [ "type"       .= t
             , "properties" .= object ["code" .= c]]
    MkLinked (Just t) h ->
      object [ "type"       .= ("link" :: Text)
             , "properties" .= object ["href" .= h, "type" .= t]]
    MkLinked Nothing h ->
      object [ "type"       .= ("link" :: Text)
             , "properties" .= object ["href" .= h]]
    MkNoCrs -> Null

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


instance ToJSON a => ToJSON (WithSomeCrs a) where
  toJSON (WithSomeCrs c a) =
    case toJSON a of
      Object o -> Object (HM.insert "crs" (toJSON c) o)
      o        -> o

instance FromJSON a => FromJSON (WithSomeCrs a) where
  parseJSON = withObject "FromJSON(WithSomeCrs): expected an object" $ \o -> do
    WithSomeCrs <$> o .: "crs" <*> parseJSON (Object o)


instance (KnownCrs crs, ToJSON a) => ToJSON (WithCrs crs a) where
  toJSON = toJSON . untagCrs

instance (KnownCrs crs, FromJSON a) => FromJSON (WithCrs crs a) where
  parseJSON o = do
    t <- parseJSON o
    withCrs t $ \(ret :: WithCrs crs2 a) ->
      case sameCrs (Proxy :: Proxy crs) (Proxy :: Proxy crs2) of
        Just Refl -> return ret
        Nothing   -> fail "FromJSON(WithCrs): crs mismatch"
