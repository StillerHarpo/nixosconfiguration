{ ... }:

{
  imports = [ ../../home/configuration.nix ];

  xsession.windowManager.xmonad.config = ../../../haskell/xmonad-desktop/xmonad.hs;

  home.stateVersion = "24.11";
}
