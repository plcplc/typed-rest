let
  pkgs = (import <nixpkgs>) {};
  hsScope = pkgs.newScope pkgs.haskellPackages;
in
rec {
  typedRestTypes = hsScope ./types/cabal.nix {};
  typedRestClient = hsScope ./client/cabal.nix {inherit typedRestTypes;};
  typedRestServer = hsScope ./server/cabal.nix {inherit typedRestTypes;};
}
