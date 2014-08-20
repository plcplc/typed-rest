# This nix-expression defines our local overrides to the nixpkgs haskellPackages.
{ pkgs } : self : super : with self;
  let cabalNix = src : cabalFile : super.callPackage (pkgs.nixGenCabal src cabalFile).outPath {};
  in {
    typedRestTypes  = cabalNix ../types "typed-rest-types.cabal";
    typedRestClient = cabalNix ../client "typed-rest-client.cabal";
    typedRestServer = cabalNix ../server "typed-rest-server.cabal";
  }
