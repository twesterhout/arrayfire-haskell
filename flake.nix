{
  description = "Haskell bindings to ArrayFire";
  inputs.haskellNix.url = "github:input-output-hk/haskell.nix";
  inputs.nixpkgs.follows = "haskellNix/nixpkgs-unstable";
  inputs.flake-utils.url = "github:numtide/flake-utils";
  inputs.arrayfire.url = "github:twesterhout/nix-for-science?dir=arrayfire";

  outputs = { self, nixpkgs, flake-utils, haskellNix, arrayfire }:
    flake-utils.lib.eachSystem [ "x86_64-linux" "x86_64-darwin" ] (system:
    let
      overlays = [ haskellNix.overlay
        (final: prev: {
          af = arrayfire.packages.${system}.pkg;
          project =
            final.haskell-nix.project' {
              src = ./.;

              compiler-nix-name = "ghc902";

              # This is used by `nix develop .` to open a shell for use with
              # `cabal`, `hlint` and `haskell-language-server`
              shell.tools = {
                cabal = {};
                # hlint = {};
                # haskell-language-server = {};
              };
              # Non-Haskell shell tools go here
              shell.buildInputs = with pkgs; [
                nixpkgs-fmt
              ];
            };
        })
      ];
      pkgs = import nixpkgs { inherit system overlays; inherit (haskellNix) config; };
      flake = pkgs.project.flake {};
    in flake // {
      # Built by `nix build .`
      defaultPackage = flake.packages."arrayfire:exe:main";
    });
}
