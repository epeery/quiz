{ mkDerivation, aeson, base, bytestring, containers, hpack, mtl
, optparse-generic, polysemy, polysemy-plugin, polysemy-zoo
, servant, servant-server, stdenv, text, warp
}:
mkDerivation {
  pname = "quiz";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson base bytestring containers mtl optparse-generic polysemy
    polysemy-plugin polysemy-zoo servant servant-server text warp
  ];
  libraryToolDepends = [ hpack ];
  executableHaskellDepends = [
    aeson base bytestring containers mtl optparse-generic polysemy
    polysemy-plugin polysemy-zoo servant servant-server text warp
  ];
  prePatch = "hpack";
  license = stdenv.lib.licenses.bsd3;
}
