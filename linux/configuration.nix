{ config, lib, home-manager, pkgs, private, ... }:

{
  imports = [ ../configuration.nix ];

  age.secrets = {
    github-token.file = ./secrets/github-token.age;
  };
  # Select internationalisation properties.
  i18n = {
    defaultLocale = "en_US.UTF-8";
    supportedLocales = [ "de_DE.UTF-8/UTF-8" "en_US.UTF-8/UTF-8" ];
  };

  # Set your time zone.
  time.timeZone = "Europe/Berlin";


  services.snowflake-proxy.enable = true;

  users.mutableUsers = false;

  nix = {
    settings = {
      substituters = lib.mkAfter [
        "https://nix-community.cachix.org?priority=50"
      ];
      trusted-public-keys = [
        "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
      ];
      trusted-users = [ "root" ];
      auto-optimise-store = true;
    };
    extraOptions = ''
      keep-outputs = true
      keep-derivations = true
      !include ${config.age.secrets.github-token.path}
    '';
    gc = {
      persistent = true;
      automatic = true;
      dates = "weekly";
      options = "--delete-older-than 7d";
    };
  };

  home-manager = {
    useGlobalPkgs = true;
    useUserPackages = true;
  };

}
