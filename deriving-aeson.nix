{ mkDerivation, aeson, base, bytestring, fetchgit, stdenv }:
mkDerivation {
  pname = "deriving-aeson";
  version = "0.1.2";
  src = fetchgit {
    url = "https://github.com/fumieval/deriving-aeson";
    sha256 = "05ck444pw450idip4rni813fiy39iibx2z7bi054hp4z0as3zjjg";
    rev = "87dadfb6d3feff014944b2cfe625142b4ae2fe3c";
    fetchSubmodules = true;
  };
  libraryHaskellDepends = [ aeson base ];
  testHaskellDepends = [ aeson base bytestring ];
  description = "Type driven generic aeson instance customisation";
  license = stdenv.lib.licenses.bsd3;
}
