{ mkDerivation, aeson, base, bytestring, protolude, stdenv, text
, unordered-containers, vector
}:
mkDerivation {
  pname = "react-docgen-types";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    aeson base bytestring protolude text unordered-containers vector
  ];
  description = "Data types and Aeson instances for parsing output of react-docgen";
  license = stdenv.lib.licenses.bsd3;
}
