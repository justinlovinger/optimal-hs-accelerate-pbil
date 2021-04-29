{ pkgs ? import ./pkgs.nix { }, compiler ? "ghc8103" }:
(import ./default.nix { inherit pkgs compiler; }).env.overrideAttrs (oldAttrs: {
  buildInputs = oldAttrs.buildInputs ++ (with pkgs; [
    haskellPackages.apply-refact
    haskellPackages.brittany
    haskellPackages.haskell-language-server
    hlint
  ]);
})
