let
  inherit (import <nixpkgs> {}) pkgs;
in
pkgs.mkShell {
  name = "advent-of-code";
  buildInputs = [
    (pkgs.haskellPackages.ghcWithPackages(p: [p.QuickCheck p.nonempty-containers p.text p.attoparsec p.pretty-simple]))
  ];
}
