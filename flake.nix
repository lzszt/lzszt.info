{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };
  outputs =
    inputs@{ self, flake-utils, ... }:
    flake-utils.lib.eachDefaultSystem (
      system:
      let
        pkgs = import ./nix/pkgs.nix { inherit inputs system; };
        packageName = "static-site-generator";
      in
      {
        packages = {
          ${packageName} = pkgs.haskellPackages.static-site-generator;
          default = pkgs.stdenv.mkDerivation {
            name = "lzszt.info";
            src = pkgs.lib.cleanSource ./.;
            installPhase = ''
              mkdir $out
              ${
                pkgs.haskell.lib.justStaticExecutables self.packages.${system}.${packageName}
              }/bin/static-site-generator $out
            '';
          };
        };

        devShells = {
          default = pkgs.haskellPackages.shellFor {
            packages = p: [ p.static-site-generator ];
            nativeBuildInputs = with pkgs; [
              haskellPackages.cabal-install
              haskellPackages.ghc
              # haskellPackages.hlint
              haskellPackages.ghcid
              haskellPackages.haskell-language-server
              haskellPackages.fourmolu
              haskellPackages.cabal-fmt
            ];
          };
        };
      }
    );
}
