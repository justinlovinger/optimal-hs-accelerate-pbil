{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-21.11";
  };

  outputs = { self, nixpkgs, ...}: let
    nixpkgsFor = forAllSystems (system: import nixpkgs {
      inherit system;
      overlays = [ self.overlay ];
    });
    forAllSystems = f: nixpkgs.lib.genAttrs supportedSystems (system: f system);
    supportedSystems = [ "x86_64-linux" ];
  in rec {
    overlay = final: prev: {
      optimal-hs-accelerate-pbil = (final.haskellPackages.override {
        overrides = self: super: {
          optimal-hs-accelerate-binary = self.callCabal2nix "optimal-hs-accelerate-binary"
            (final.fetchFromGitHub {
              owner = "JustinLovinger";
              repo = "optimal-hs-accelerate-binary";
              rev = "v1.1.0.0";
              sha256 = "1q82jghbjcfk6yxm6nmzkr8cp6blzq1jr80m1f4ysva3w264fiy0";
            }) { };

          sfc-random-accelerate = self.callCabal2nix "sfc-random-accelerate"
            (final.fetchFromGitHub {
              owner = "tmcdonell";
              repo = "sfc-random-accelerate";
              rev = "abc294f6bdd982b61de38efe90d79d9a7c649dc9";
              sha256 = "1cbb57sm5h5kja31y2ajbnbi5n4gahv12s1nnjmx0rys47li0r0n";
            }) { };
        };
      }).callCabal2nix "optimal-hs-accelerate-pbil" ./. { };
    };

    packages = forAllSystems (system: {
      optimal-hs-accelerate-pbil = nixpkgsFor.${system}.optimal-hs-accelerate-pbil;
    });

    defaultPackage = forAllSystems (system: self.packages.${system}.optimal-hs-accelerate-pbil);

    checks = self.packages;

    devShell = forAllSystems (system: let
      pkgs = nixpkgsFor.${system};
    in pkgs.haskellPackages.shellFor {
      packages = p: [self.packages.${system}.optimal-hs-accelerate-pbil];
      withHoogle = true;
      buildInputs = with pkgs; [
        cabal-install
        haskellPackages.apply-refact
        haskellPackages.brittany
        haskellPackages.haskell-language-server
        hlint
      ];
    });
  };
}
