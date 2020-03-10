# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, ... }:

let
  all-hies = import (fetchTarball "https://github.com/infinisil/all-hies/tarball/master") {};
in
{
  imports = [ ./environments.nix
              ./zsh.nix
              ./pia-nm.nix
              ];
  boot = {
    # Use the systemd-boot EFI boot loader.
    loader = {
      systemd-boot.enable = true;
      efi.canTouchEfiVariables = true;
    };
    resumeDevice = "/dev/disk/by-label/swap";
    initrd.postDeviceCommands = "sleep 5";
  };

  # networking.hostName = "nixos"; # Define your hostname.

  # Select internationalisation properties.
  i18n = {
    consoleKeyMap = "us";
    defaultLocale = "en_US.UTF-8";
  };

  # Set your time zone.
  time.timeZone = "Europe/Berlin";

  # List packages installed in system profile. To search by name, run:
  # $ nix-env -qaP | grep wget

  environment.systemPackages = with pkgs; ([
    mtpfs
    clojure
    leiningen
    wget
    direnv
    cachix
    # emacs
    (import ./vim.nix)
#   (import ./emacs.nix)
    shellcheck
    local.signal-desktop
    local.steam
    local.minecraft
    sshfs
    sudo
    dzen2
    firefox
    qutebrowser
    local.mattermost-desktop
    w3m
    git
    pass
    passff-host
    gnupg
    # termite
    alacritty
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
    bc
    feh
    anki
    eclipses.eclipse-sdk
    nix-prefetch-git
    torbrowser
    dunst
    local.youtube-dl
    local.youtube-viewer
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
    ghc
    cabal2nix cabal-install
    # stack2nix
    stack
    (all-hies.unstableFallback.selection { selector = p: p; })
    #(haskellPackages.ghcWithPackages (self : with self;
    #  [ hlint hindent QuickCheck parsec megaparsec optparse-applicative
    #    adjunctions Agda ]))
    networkmanager_openvpn networkmanager_dmenu
  ]);

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
        default = "xmonad";
        xmonad = {
          enable = true;
          enableContribAndExtras = true;
          extraPackages = haskellPackages: with haskellPackages; [MissingH protolude];
        };
      };
      displayManager.auto = {
        enable = true;
        user = "florian";
      };
    };
    unclutter-xfixes.enable = true;
    emacs = {
      enable=true;
      defaultEditor=true;
      #  package = import ./emacs.nix;
    };
    hoogle.enable = true;
    redshift = with import ./cords.nix; {
      enable = true;
      brightness = {
        day = "0.8";
        night = "0.7";
      };
      temperature.night = 1500;
      inherit latitude longitude;
    };
    compton = {
      enable = true;
      inactiveOpacity = "0.8";
    };
  };

  # Define a user account. Don't forget to set a password with ‘passwd’.
  users.extraUsers.florian= {
    isNormalUser = true;
    uid = 1000;
    createHome = true;
    extraGroups = [ "wheel" "networmanager" "audio" "docker" "video" ];
  };

  # passwordless sudo
  security.sudo.wheelNeedsPassword = false;

  # docker
  virtualisation.docker.enable=true;

  hardware = {
    pulseaudio = {
      enable = true;
      package = pkgs.pulseaudioFull;
      support32Bit = true;
    };
    opengl.driSupport32Bit = true;
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
  };

  # The NixOS release to be compatible with for stateful data such as databases.
  system.stateVersion = "19.03";
}
