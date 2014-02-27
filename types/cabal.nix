{ cabal, text }:
cabal.mkDerivation (self: {
  pname = "typed-rest-types";
  version = "0.0.0.1";
  buildDepends = [ text ];
  src = ./.;
  meta = {
    homepage = "https://github.com/plcplc/typed-rest";
    description = "Typed REST resources - Common definitions for server and client";
    license = self.stdenv.lib.licenses.gpl3;
    platforms = self.ghc.meta.platforms;
  };
})
