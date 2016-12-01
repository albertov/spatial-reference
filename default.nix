{ mkDerivation, aeson, base, deepseq, hashable, hspec, QuickCheck
, scientific, stdenv, text, unordered-containers
}:
mkDerivation {
  pname = "spatial-reference";
  version = "0.1.1";
  src = ./.;
  libraryHaskellDepends = [
    aeson base deepseq hashable scientific text unordered-containers
  ];
  testHaskellDepends = [ aeson base hspec QuickCheck ];
  homepage = "http://github.com/albertov/spatial-reference";
  description = "Spatial reference types for use in GIS applications";
  license = stdenv.lib.licenses.bsd3;
}
