{ mkDerivation, aeson, base, blaze-html, blaze-markup, bytestring
, containers, hpack, JuicyPixels, mtl, optparse-generic, polysemy
, polysemy-plugin, polysemy-zoo, servant, servant-JuicyPixels
, servant-server, stdenv, text, turtle, uuid, wai, wai-cors
, wai-extra, warp
}:
mkDerivation {
  pname = "quiz";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson base blaze-html blaze-markup bytestring containers
    JuicyPixels mtl optparse-generic polysemy polysemy-plugin
    polysemy-zoo servant servant-JuicyPixels servant-server text turtle
    uuid wai wai-cors wai-extra warp
  ];
  libraryToolDepends = [ hpack ];
  executableHaskellDepends = [
    aeson base blaze-html blaze-markup bytestring containers
    JuicyPixels mtl optparse-generic polysemy polysemy-plugin
    polysemy-zoo servant servant-JuicyPixels servant-server text turtle
    uuid wai wai-cors wai-extra warp
  ];
  prePatch = "hpack";
  license = stdenv.lib.licenses.bsd3;
}
