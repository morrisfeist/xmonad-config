{ pkgs, ... }:

rec {
  default = pkgs.mkShell {
    inputsFrom = [
      haskell
      nix
    ];
  };

  haskell = pkgs.haskellPackages.shellFor {
    packages = hpkgs: with hpkgs; [ xmonad-config ];
    withHoogle = true;
    nativeBuildInputs = [
      pkgs.haskellPackages.cabal-install
      pkgs.haskellPackages.ghcid
      pkgs.haskellPackages.haskell-language-server
      pkgs.haskellPackages.hpack
      pkgs.haskellPackages.implicit-hie

      pkgs.ghciwatch
    ];
  };

  nix = pkgs.mkShell {
    buildInputs = with pkgs; [
      nixd
      nixfmt
    ];
  };
}
