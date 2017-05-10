{ mkDerivation, aeson, base, bytestring, lens, lens-aeson
, protolude, stdenv, text, unordered-containers
}:
mkDerivation {
  pname = "react-docgen-types";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    aeson base bytestring lens lens-aeson protolude text
    unordered-containers
  ];
  description = "Data types and Aeson instances for parsing output of react-docgen";
  license = stdenv.lib.licenses.bsd3;
}
