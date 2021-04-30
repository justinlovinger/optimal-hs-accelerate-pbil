{ pkgs ? import ./pkgs.nix { }, compiler ? "ghc8104" }:
(import ./default.nix { inherit pkgs compiler; }).env.overrideAttrs (oldAttrs: {
  buildInputs = oldAttrs.buildInputs ++ (with pkgs; [
    cabal-install
    haskellPackages.apply-refact
    haskellPackages.brittany
    haskellPackages.haskell-language-server
    hlint
  ]);
})
