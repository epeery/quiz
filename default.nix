{ mkDerivation, aeson, base, containers, hpack, optparse-generic
, polysemy, polysemy-plugin, servant, servant-server, stdenv, text
, warp
}:
mkDerivation {
  pname = "quiz";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson base containers optparse-generic polysemy polysemy-plugin
    servant servant-server text warp
  ];
  libraryToolDepends = [ hpack ];
  executableHaskellDepends = [
    aeson base containers optparse-generic polysemy polysemy-plugin
    servant servant-server text warp
  ];
  prePatch = "hpack";
  license = stdenv.lib.licenses.bsd3;
}
