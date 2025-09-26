{ pkgs, ... }:

rec {
  inherit (pkgs) xmonad-config;

  default = xmonad-config;
}
