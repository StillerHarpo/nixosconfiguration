{ config, lib, home-manager, pkgs, agenix, private, ... }:

{
  imports = [ ../configuration.nix ./dnscrypt.nix ];

  # Select internationalisation properties.
  console.keyMap = "us";
  i18n.defaultLocale = "en_US.UTF-8";

  # Set your time zone.
  time.timeZone = "Europe/Berlin";

  # List packages installed in system profile. To search by name, run:
  # $ nix-env -qaP | grep wget

  services = {
    dbus.packages = [ pkgs.dconf ];
    # Enable the X11 windowing system.
    xserver = {
      enable = true;
      layout = "us";
      xkbOptions = "eurosign:e, caps:escape, grp:alt_shift_toggle";
      xkbVariant = "altgr-intl";
      monitorSection = ''Option "DPMS" "false"'';
      serverLayoutSection = ''
        Option          "BlankTime"     "0"
        Option          "StandbyTime"   "0"
        Option          "SuspendTime"   "0"
        Option          "OffTime"       "0"
      '';
      # Enable XMonad
      windowManager = {
        xmonad = {
          enable = true;
          enableContribAndExtras = true;
        };
      };
      displayManager = {
        defaultSession = "none+xmonad";
        autoLogin = {
          enable = true;
          user = "florian";
        };
      };
    };

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
        extraGroups = [ "audio" "video" ];
      };
    };
  };

  services.snowflake-proxy.enable = true;

  age.identityPaths = [ "/root/.ssh/id_rsa" ];

  nix = {
    settings = {
      substituters = [
        "https://cache.nixos.org/"
        "https://nix-community.cachix.org"
        "ssh://nix-ssh@${private.serverIP}?ssh-key=/etc/ssh/id_rsa.pub"
      ];
      trusted-public-keys = [
        "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
        "${private.serverIP}:GlSZMfUPHG/R3qSDpEaY6cO5CGLunhtOCimhmYJvhCo="
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
    services.mullvad-vpn.enable = lib.mkForce false;
  };

  # The NixOS release to be compatible with for stateful data such as databases.
  system.stateVersion = "21.05";
}
