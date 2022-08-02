{
  description = "Bla-bla-bla";
  inputs.nixpkgs.url = "github:nixos/nixpkgs";
  inputs.flake-utils.url = "github:numtide/flake-utils";

  outputs = { self, nixpkgs, flake-utils }:
  flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        t = pkgs.lib.trivial;
        hl = pkgs.haskell.lib;
        name = "arrayfire-haskell";

        project = devTools: # [1]
          let addBuildTools = (t.flip hl.addBuildTools) devTools;
          in pkgs.haskellPackages.developPackage {
            root = pkgs.lib.sourceFilesBySuffices ./. [ ".cabal" ".hs" ];
            name = name;
            returnShellEnv = !(devTools == [ ]); # [2]

            modifier = (t.flip t.pipe) [
              addBuildTools
              hl.dontHaddock
              hl.enableStaticLibraries
              hl.justStaticExecutables
              hl.disableLibraryProfiling
              hl.disableExecutableProfiling
            ];
          };

      in {
        packages.pkg = project [ ]; # [3]

        defaultPackage = self.packages.${system}.pkg;

        devShell = project [ # [4]
          # System dependencies
          # pkgs.arrayfire
          # Haskell build tools
          pkgs.haskellPackages.cabal-fmt
          pkgs.haskellPackages.cabal-install
        ];
      });
      # let
      #   pkgs = nixpkgs.legacyPackages.${system};
      # in
      # {
      #   devShell = pkgs.mkShell {
      #     buildInputs = [
      #       pkgs.arrayfire
      #       pkgs.petsc
      #     ];
      #   
      #     shellHook = ''
      #       export ARRAYFIRE=${pkgs.arrayfire}
      #       export PETSC=${pkgs.petsc}
      #     '';
      #   };
      # }
      # );
}
