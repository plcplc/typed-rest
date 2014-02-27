{ aeson, cabal, dataDefault, httpClient, httpTypes, text, typedRestTypes }:
cabal.mkDerivation (self: {
  pname = "typed-rest-client";
  version = "0.0.0.1";
  buildDepends = [ aeson dataDefault httpClient httpTypes text typedRestTypes ];
  src = ./.;
  meta = {
    homepage = "https://github.com/plcplc/typed-rest";
    description = "A typesafe client interface to REST resources";
    license = self.stdenv.lib.licenses.gpl3;
    platforms = self.ghc.meta.platforms;
  };
})
