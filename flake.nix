{
  description = "Flake using Input Output HK infrastructure";
  inputs = {
    haskellNix.url = "github:input-output-hk/haskell.nix/29433c04fa097b50be67c5b799eb7a87982cd900";
    flake-utils.url = "github:numtide/flake-utils/3cecb5b042f7f209c56ffd8371b2711a290ec797";
    nixpkgs.follows = "haskellNix/nixpkgs-unstable";
  };
  outputs = { self, nixpkgs, flake-utils, haskellNix }:
    flake-utils.lib.eachSystem [ "x86_64-linux" "x86_64-darwin" ] (system:
    let
      overlays = [ haskellNix.overlay
        (final: prev: {
          # This overlay adds our project to pkgs
          opticsExercises =
            final.haskell-nix.project' {
              src = ./.;
              compiler-nix-name = "ghc8107";
            };
        })
      ];
      pkgs = import nixpkgs { inherit system overlays; };
      flake = pkgs.opticsExercises.flake {};
    in flake // {
      # Built by `nix build .`
      defaultPackage = flake.packages."lib:optics-exercises";

      # This is used by `nix develop .` to open a shell for use with
      # `cabal`, `hlint` and `haskell-language-server`
      devShell = pkgs.opticsExercises.shellFor {
        withHoogle = true;
        tools = {
          cabal = "latest";
          hlint = "latest";
          haskell-language-server = "latest";
          hspec-discover = "latest";
        };
      };
    });
}
