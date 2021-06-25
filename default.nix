{ pkgs ? import ./pkgs.nix { }, compiler ? "ghc8104" }:
(pkgs.haskell.packages.${compiler}.override {
  overrides = with pkgs.haskell.lib; self: super: {
    sfc-random-accelerate = self.callCabal2nix "sfc-random-accelerate"
      (pkgs.fetchFromGitHub {
        owner = "tmcdonell";
        repo = "sfc-random-accelerate";
        rev = "abc294f6bdd982b61de38efe90d79d9a7c649dc9";
        sha256 = "1cbb57sm5h5kja31y2ajbnbi5n4gahv12s1nnjmx0rys47li0r0n";
        fetchSubmodules = true;
      }) { };
  };
}).callCabal2nix "optimal-hs-accelerate-pbil" ./. { }
