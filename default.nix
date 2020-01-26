{ mkDerivation, aeson, base, bytestring, containers, hpack, mtl
, optparse-generic, polysemy, polysemy-plugin, polysemy-zoo
, servant, servant-server, stdenv, text, wai, wai-cors, wai-extra
, warp
}:
mkDerivation {
  pname = "quiz";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson base bytestring containers mtl optparse-generic polysemy
    polysemy-plugin polysemy-zoo servant servant-server text wai
    wai-cors wai-extra warp
  ];
  libraryToolDepends = [ hpack ];
  executableHaskellDepends = [
    aeson base bytestring containers mtl optparse-generic polysemy
    polysemy-plugin polysemy-zoo servant servant-server text wai
    wai-cors wai-extra warp
  ];
  prePatch = "hpack";
  license = stdenv.lib.licenses.bsd3;
}
