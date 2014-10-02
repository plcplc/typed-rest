# This nix-expression defines our local overrides to the nixpkgs haskellPackages.
{ pkgs } : self : super : with self;
  let cabalNix = src : cabalFile : super.callPackage (pkgs.nixGenCabal src cabalFile).outPath {};
  in {
    typedRestTypes  = cabalNix ../types "typed-rest-types.cabal";
    typedRestClientHttpClient = cabalNix ../client-http-client "typed-rest-client-http-client.cabal";
    typedRestServerWai = cabalNix ../server-wai "typed-rest-server-wai.cabal";
    typedRestEncodingJson = cabalNix ../encoding-json "typed-rest-encoding-json.cabal";
  }
