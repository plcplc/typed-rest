{ aeson, cabal, dataDefault, httpTypes, typedRestTypes, wai, warp }:
cabal.mkDerivation (self: {
  pname = "typed-rest-server";
  version = "0.0.0.1";
  buildDepends = [ aeson dataDefault httpTypes typedRestTypes wai warp ];
  src = ./.;
  meta = {
    homepage = "https://github.com/plcplc/typed-rest";
    description = "A typesafe server interface to REST resources";
    license = self.stdenv.lib.licenses.gpl3;
    platforms = self.ghc.meta.platforms;
  };
})
