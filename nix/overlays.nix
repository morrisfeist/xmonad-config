final: prev:

let
  inherit (final.nix-gitignore) gitignoreSource;

  xmonad-config-src = gitignoreSource [ ] ../.;
  xmonad-config = final.haskellPackages.callCabal2nix "xmonad-config" xmonad-config-src { };

  haskellOverrides = { inherit xmonad-config; };
  extendHaskellPackages = haskellPackages: haskellPackages.extend (_: _: haskellOverrides);
in

{
  haskell = prev.haskell // {
    packages = builtins.mapAttrs (_: extendHaskellPackages) prev.haskell.packages;
  };
  haskellPackages = extendHaskellPackages prev.haskellPackages;
  inherit xmonad-config;
}
