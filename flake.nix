{
  description = "XMonad Configuration";

  inputs.nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";

  outputs =
    { nixpkgs, self, ... }:
    let
      system = "x86_64-linux";
      overlays = [ self.overlays.default ];
      pkgs = import nixpkgs { inherit system overlays; };
    in
    {
      devShells.${system} = pkgs.callPackage ./nix/shell.nix { };
      overlays.default = import ./nix/overlays.nix;
      packages.${system} = pkgs.callPackage ./nix/packages.nix { };
    };
}
