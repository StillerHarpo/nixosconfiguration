{ config, home-manager, pkgs, agenix, ... }:

{
  imports = [
    ../configuration.nix
  ];

  boot = {
    # Use the systemd-boot EFI boot loader.
    loader = {
      systemd-boot.enable = true;
      efi.canTouchEfiVariables = true;
    };
    kernel.sysctl."kernel.yama.ptrace_scope" = 1;
  };

  # Select internationalisation properties.
  console.keyMap = "us";
  i18n.defaultLocale = "en_US.UTF-8";

  # Set your time zone.
  time.timeZone = "Europe/Berlin";

  # List packages installed in system profile. To search by name, run:
  # $ nix-env -qaP | grep wget

  services = {
    dbus.packages = [ pkgs.gnome3.dconf ];
    # Enable the X11 windowing system.
    xserver = {
      enable = true;
      layout = "us";
      xkbOptions = "eurosign:e, caps:escape, grp:alt_shift_toggle";
      xkbVariant = "altgr-intl";
      monitorSection = ''Option "DPMS" "false"'';
      serverLayoutSection =
      ''
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

  hardware.opengl = {
    driSupport32Bit = true;
    extraPackages32 = with pkgs.pkgsi686Linux; [ libva ];
  };

  age.identityPaths = [ "/root/.ssh/id_rsa" ];

  nix = {
    binaryCaches = [
      "https://cache.nixos.org/"
      "https://hie-nix.cachix.org"
    ];
    binaryCachePublicKeys = [
      "hie-nix.cachix.org-1:EjBSHzF6VmDnzqlldGXbi0RM3HdjfTU3yDRi9Pd0jTY="
    ];
    trustedUsers = [ "root" "florian" ];
    extraOptions = ''
      keep-outputs = true
      keep-derivations = true
    '';
  };

  home-manager = {
    useGlobalPkgs = true;
    useUserPackages = true;
  };

  # The NixOS release to be compatible with for stateful data such as databases.
  system.stateVersion = "21.05";
}
