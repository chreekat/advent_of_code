{
  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-23.05";
  outputs = { self, nixpkgs }: 
    let pkgs = import nixpkgs { system = "x86_64-linux"; }; in
    {
      devShells.x86_64-linux.default = pkgs.haskellPackages.ghcWithPackages (p: [p.advent-of-code-api]);
    };
}
