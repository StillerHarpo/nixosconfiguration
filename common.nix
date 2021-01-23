# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, ... }:

let
  all-hies = import (builtins.fetchTarball "https://github.com/infinisil/all-hies/tarball/master") {};
in
{
  imports = [./home.nix
             ./pia-nm.nix
            ];

  boot = {
    # Use the systemd-boot EFI boot loader.
    loader = {
      systemd-boot.enable = true;
      efi.canTouchEfiVariables = true;
    };
  };

  # networking.hostName = "nixos"; # Define your hostname.

  # Select internationalisation properties.
  console.keyMap = "us";
  i18n.defaultLocale = "en_US.UTF-8";

  # Set your time zone.
  time.timeZone = "Europe/Berlin";

  # List packages installed in system profile. To search by name, run:
  # $ nix-env -qaP | grep wget

  environment = {
    systemPackages = with pkgs; ([
      pavucontrol
      arandr
      networkmanagerapplet
      mtpfs
      clojure
      leiningen
      wget
      cachix
      # emacs
      (import ./vim.nix)
      #   (import ./emacs.nix)
      shellcheck
      local.signal-desktop
      ##################### Games ###############
      lutris                  # play windows games
      multimc                 # minecraft launcher
      local.steam             # play games
      openjdk                 # java
      vulkan-tools            # needed for lutris
      ##################### Voice chats #########
      jitsi
      local.teamspeak_client
      local.discord
      local.element-desktop
      local.slack
      local.zoom-us
      ###########################################
      sshfs
      sudo
      dzen2
      firefox
      qutebrowser
      local.mattermost-desktop
      w3m
      pass
      passff-host
      pinentry-curses
      neomutt
      mu
      toxic
      newsboat
      poppler
      rtv
      xsel
      ag
      zathura
      spotify
      mpv
      rlwrap
      translate-shell
      you-get
      xosd
      pandoc
      (texlive.combine {inherit (texlive) scheme-full pygmentex pgf collection-basic;})
      python37Packages.pygments
      bc
      feh
      anki
      eclipses.eclipse-sdk
      nix-prefetch-git
      # torbrowser
      dunst
      local.youtube-dl
      # local.youtube-viewer
      # local.pipe-viewer
      libnotify
      unzip
      rofi
      wmctrl
      idris
      idea.idea-community
      unclutter-xfixes
      psmisc
      aspell
      aspellDicts.de
      aspellDicts.en
      aspellDicts.en-computers
      aspellDicts.en-science
      coq
      # unstable.racket
      # haskell
      # ghc
      cabal2nix
      # cabal-install
      local.haskellPackages.implicit-hie
      local.niv
      # stack2nix
      # stack
      haskellPackages.Agda
      agdaPackages.standard-library
      #(haskellPackages.ghcWithPackages (self : with self;
      #  [ hlint hindent QuickCheck parsec megaparsec optparse-applicative
      #    adjunctions Agda ]))
      networkmanager_openvpn networkmanager_dmenu
    ]);
    pathsToLink = [ "/share/agda" "/share/zsh" ];
  };

  nixpkgs.config =
  {
    # Allow proprietary packages
    allowUnfree = true;

    # Create an alias for the unstable channel
    packageOverrides = pkgs:
    {
      unstable = import <nixos-unstable>
      {
        # pass the nixpkgs config to the unstable alias
        # to ensure `allowUnfree = true;` is propagated:
        config = config.nixpkgs.config;
      };
      local = import ../nixpkgs
      {
        # pass the nixpkgs config to the unstable alias
        # to ensure `allowUnfree = true;` is propagated:
        config = config.nixpkgs.config;
      };
    };
  };

  fonts = {
    enableFontDir = true;
    fonts = with pkgs; [
      terminus_font
      dejavu_fonts
      source-code-pro
    ];
  };

  location = import ./cords.nix;

  # List services that you want to enable:

  # Enable the OpenSSH daemon.
  # services.openssh.enable = true;

  # Enable CUPS to print documents.
  # services.printing.enable = true;
  services = {
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

    printing.enable = true;
    avahi = {
      enable = true;
      nssmdns = true;
    };
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
  };

  programs = {
    slock.enable = true;
    adb.enable = true;
    fuse.userAllowOther = true;
  };

  # Define a user account. Don't forget to set a password with ‘passwd’.
  users.extraUsers.florian= {
    isNormalUser = true;
    uid = 1000;
    createHome = true;
    extraGroups = [ "adbusers" "wheel" "networkmanager" "audio" "docker" "video" ];
  };

  # passwordless sudo
  security.sudo.wheelNeedsPassword = false;

  # docker
  virtualisation = {
    docker.enable=true;
    virtualbox = {
      guest.enable = true;
      host.enable = true;
    };
  };

  hardware = {
    pulseaudio = {
      enable = true;
      package = pkgs.pulseaudioFull;
      support32Bit = true;
      extraConfig = ''
        unload-module module-role-cork
        load-module module-native-protocol-tcp auth-ip-acl=127.0.0.1
        load-module module-switch-on-connect
      '';
    };
    opengl = {
     driSupport32Bit = true;
     extraPackages32 = with pkgs.pkgsi686Linux; [ libva ];
    };
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
  system.stateVersion = "20.09";
}
