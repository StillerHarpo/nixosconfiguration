{ config, lib, home-manager, pkgs, private, ... }:

{
  imports = [ ../configuration.nix ./dnscrypt.nix ];

  # Select internationalisation properties.
  console.keyMap = "us";
  i18n.defaultLocale = "en_US.UTF-8";

  # Set your time zone.
  time.timeZone = "Europe/Berlin";

  xserver.enable = true;
  wayland.enable = true;

  services = {
    dbus.packages = [ pkgs.dconf ];

    pipewire = {
      enable = true;
      alsa = {
        enable = true;
        support32Bit = true;
      };
      pulse.enable = true;
    };
  };

  programs = {
    steam.enable = true;
    gamemode.enable = true;
  };

  users = {
    mutableUsers = false;
    users = {
      florian = {
        isNormalUser = true;
        extraGroups = [ "audio" "video" "wireshark" ];
      };
    };
  };

  programs.wireshark = {
    enable = true;
    package = pkgs.wireshark;
  };

  services.snowflake-proxy.enable = true;

  age.identityPaths = [ "/root/.ssh/id_rsa" ];

  nix = {
    settings = {
      substituters = lib.mkAfter [
        "https://nix-community.cachix.org?priority=50"
        "ssh://nix-ssh@${private.serverIP}?ssh-key=/etc/ssh/id_rsa.pub&priority=60"
      ];
      trusted-public-keys = [
        "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
        "ssh-serve:WltAxNNiDoufj5yg7k9tJHKoN7D5PgIZUyqDOMBOaGM="
      ];
      trusted-users = [ "root" "florian" ];
      auto-optimise-store = true;
    };
    extraOptions = ''
      keep-outputs = true
      keep-derivations = true
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

  dnscrypt.enable = true;
  specialisation.normalInternet.configuration = {
    dnscrypt.enable = lib.mkForce false;
    virtualisation.docker.enable = lib.mkForce false;
  };

  # The NixOS release to be compatible with for stateful data such as databases.
  system.stateVersion = "21.05";
}
