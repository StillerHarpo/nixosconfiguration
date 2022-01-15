# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, pkgs-unstable, pkgs-master, ... }:

{
  imports = [
    ./pia-openvpn.nix
    ./common.nix
    (with (import ./apparmor.nix); generate [
      {
        pkgs = with pkgs; [
          sqlite
          gimp
          pamixer
          aria
          imagemagick
          pavucontrol
          arandr
          networkmanagerapplet
          mtpfs
          wget
          cachix
          vim
          shellcheck
          ##################### Games ###############
          multimc                 # minecraft launcher
          openjdk                 # java
          sshfs
          dzen2
          chromium
          w3m
          passff-host
          neomutt
          mu
          toxic
          poppler
          tuir
          xsel
          ag
          zathura
          spotify
          mpv
          rlwrap
          you-get
          xosd
          pandoc
          (texlive.combine {inherit (texlive) scheme-full pygmentex pgf collection-basic;})
          python37Packages.pygments
          bc
          feh
          anki
          nix-prefetch-git
          pkgs-master.youtube-dl
          libnotify
          unzip
          rofi
          wmctrl
          unclutter-xfixes
          psmisc
          cabal2nix
          pkgs-master.haskellPackages.implicit-hie
          pkgs-master.niv
          pkgs-unstable.stack
          #(haskellPackages.ghcWithPackages (self : with self;
          #  [ hlint hindent QuickCheck parsec megaparsec optparse-applicative
          #    adjunctions Agda ]))
          networkmanager_openvpn networkmanager_dmenu
          git-crypt
          slack
        ];
        profile = defaultProfile;
      }
      {
        pkgs = [ pkgs.pass ];
        profile = ''
          file,
          deny rw /home/florian/Dokumente/**,
          deny rw /home/florian/.ssh/**,
        '';
      }
      {
        pkgs = [ pkgs.firefox ];
        profile = ''
          file,
          network,
    deny rw /root/.ssh/**,
    deny rw /root/.ssh,
    deny rw /root/.gnupg/**,
    deny rw /root/.gnupg,
    deny rw /home/florian/Dokumente/**,
    deny rw /home/florian/Dokumente,
    deny rw /home/florian/.ssh/**,
    deny rw /home/florian/.ssh,
    deny rw /home/florian/.gnupg/**,
    deny rw /home/florian/.gnupg,
    deny rw /home/florian/.password-store/**,
    deny rw /home/florian/.password-store,
        '';
      }
    ])
  ];

  environment.systemPackages = [ pkgs-master.signal-desktop pkgs.sudo ];
  security.apparmor.policies.signal-desktop = {
    profile = ''${pkgs-master.signal-desktop}/bin/signal-desktop {
          capability,
          ${(import ./apparmor.nix).defaultProfile}
        }
        '';
  };
  home-manager = {
    useGlobalPkgs = true;
    useUserPackages = true;
    users.florian = import ./homeLinuxSpecific.nix;
  };

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

  environment = {
    pathsToLink = [ "/share/agda" "/share/zsh" ];
  };

  fonts.fonts = [ pkgs.terminus_font ];

  location = import ./cords.nix;

  # List services that you want to enable:

  # Enable the OpenSSH daemon.
  # services.openssh.enable = true;

  # Enable CUPS to print documents.
  # services.printing.enable = true;
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
          extraPackages = haskellPackages: with haskellPackages; [MissingH protolude];
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
    unclutter-xfixes.enable = true;

    hoogle.enable = true;
    redshift = {
      enable = true;
      brightness = {
        day = "0.8";
        night = "0.7";
      };
      temperature.night = 1501;
    };
    picom = {
      enable = true;
      inactiveOpacity = 0.8;
      opacityRules = [ "100:name = 'Dmenu'" "100:name = 'Rofi'" "100:class_g ?= 'Rofi'" "100:name = 'Notification'" ];
    };
    pipewire = {
      enable = true;
      media-session.config.bluez-monitor.properties.bluez5.msbc-support = true;
      alsa = {
        enable = true;
        support32Bit = true;
      };
      pulse.enable = true;
    };
  };

  programs = {
    steam.enable = true;
    slock.enable = true;
    adb.enable = true;
    fuse.userAllowOther = true;
    gnupg.dirmngr.enable = true;
  };

  # Define a user account. Don't forget to set a password with ‘passwd’.
  users.extraUsers.florian= {
    isNormalUser = true;
    uid = 1000;
    createHome = true;
    extraGroups = [ "adbusers" "wheel" "networkmanager" "audio" "docker" "video" "scan" "lp"];
  };

  # passwordless sudo
  security.sudo.wheelNeedsPassword = false;

  hardware.opengl = {
    driSupport32Bit = true;
    extraPackages32 = with pkgs.pkgsi686Linux; [ libva ];
  };


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

  # The NixOS release to be compatible with for stateful data such as databases.
  system.stateVersion = "21.05";
}
