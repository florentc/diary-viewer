{
  inputs = {
    nixpkgs.url = "github:nixoS/nixpkgs/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };
  outputs = {self, nixpkgs, flake-utils} :
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        haskellpkgs = pkgs.haskellPackages;
      in {
        formatter = pkgs.nixfmt;
        devShells.default = pkgs.mkShell {
          buildInputs = [
            haskellpkgs.ghc
            haskellpkgs.haskell-language-server
            pkgs.cabal-install
            pkgs.ormolu
            pkgs.hpack
            pkgs.zlib
          ];
        };
      }
    );
}
