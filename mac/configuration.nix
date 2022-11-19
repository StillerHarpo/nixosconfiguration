{ config, lib, pkgs, home-manager, ... }:

{
  imports = [ ../configuration.nix ];

  environment.systemPackages =
    with pkgs; ([ git niv pass bashInteractive_5 haskell-language-server ]);

  users.users.florianengel = {
    name = "florianengel";
    home = "/Users/florianengel";
  };

  home-manager.users.florianengel = import ./home.nix;

  programs = { zsh.enable = true; };

  system.stateVersion = 4;
}
