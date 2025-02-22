{ inputs }:
let
  customHaskellPackages = self: super: {
    haskellPackages = super.haskellPackages.override {
      overrides =
        hself: hsuper:
        let
          dontCheck = super.haskell.lib.dontCheck;
          dontHaddock = super.haskell.lib.dontHaddock;
          doJailbreak = super.haskell.lib.doJailbreak;

          polysemy-utils = hself.callCabal2nix "polysemy-utils" inputs.polysemy-utilsSrc { };

          hspec-junit-formatter =
            hself.callCabal2nix "hspec-junit-formatter" inputs.hspec-junit-formatterSrc
              { };

          distribution-nixpkgs = dontCheck hsuper.distribution-nixpkgs;
          postgresql-libpq = doJailbreak hsuper.postgresql-libpq;
          polysemy-plugin = doJailbreak hsuper.polysemy-plugin;
          uuid = doJailbreak hsuper.uuid;
          serialise = doJailbreak hsuper.serialise;

          static-site-generator-src = self.nix-gitignore.gitignoreSource [
            "*.git"
            "dist"
            "dist-newstyle"
          ] ../.;
          static-site-generator = dontHaddock (
            hself.callCabal2nix "static-site-generator" static-site-generator-src { }
          );
        in
        {
          # We add ourselves to the set of haskellPackages.
          inherit static-site-generator;
          inherit polysemy-utils hspec-junit-formatter;
          inherit
            distribution-nixpkgs
            postgresql-libpq
            polysemy-plugin
            uuid
            serialise
            ;
        };
    };
  };
in
[
  (final: prev: { haskellPackages = prev.haskell.packages.ghc910; })
  customHaskellPackages
]
