# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, ... }:

{
  imports = [ ./environments.nix
  ./zsh.nix
  ../nixpkgs/nixos/modules/services/databases/monetdb.nix ];
  boot = {
    resumeDevice = "/dev/disk/by-label/swap";
    initrd.postDeviceCommands = "sleep 5";

  };

  # networking.hostName = "nixos"; # Define your hostname.

  # Select internationalisation properties.
  i18n = {
    consoleFont = "Lat2-Terminus16";
    consoleKeyMap = "us";
    defaultLocale = "en_US.UTF-8";
  };

  # Set your time zone.
  time.timeZone = "Europe/Berlin";

  # List packages installed in system profile. To search by name, run:
  # $ nix-env -qaP | grep wget

  environment.systemPackages = with pkgs; ([
    wget
    (import ./vim.nix)
#   (import ./emacs.nix)
    shellcheck
    sudo
    firefox
    qutebrowser
    w3m
    git
    pass
    gnupg
    termite
    neomutt
    toxic
    newsboat
    unstable.rtv
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
    nix-prefetch-git
    calcurse
    google-chrome
    torbrowser
    dunst
    libnotify
    dmenu
    wmctrl
    weechat
    idea.idea-community
    unclutter-xfixes
    psmisc
    ispell
    # haskell
    # cabal2nix cabal-install
    # stack2nix stack
    (haskellPackages.ghcWithPackages (self : with self; [ hlint hindent QuickCheck megaparsec]))
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

  # List services that you want to enable:

  # Enable the OpenSSH daemon.
  # services.openssh.enable = true;

  # Enable CUPS to print documents.
  # services.printing.enable = true;

  services = {
    # Enable the X11 windowing system.
    xserver = {
      enable = true;
      layout = "us,de";
      xkbOptions = "eurosign:e, caps:escape, grp:alt_shift_toggle";
      monitorSection = ''Option "DPMS" "false"'';
      serverLayoutSection =
      ''
        Option          "BlankTime"     "0"
        Option          "StandbyTime"   "0"
        Option          "SuspendTime"   "0"
        Option          "OffTime"       "0"
     '';
      # Enable XMonad
      windowManager.xmonad = {
        enable = true;
        enableContribAndExtras = true;
      };
    };
    unclutter-xfixes.enable = true;
    emacs = {
      enable=true;
      #  package = import ./emacs.nix;
    };
    hoogle.enable = true;
    postgresql = {
      enable = true;
      package = pkgs.postgresql100;
    };
    monetdb = {
      enable = true;
      user = "florian";
      group = "users";
      package = pkgs.local.monetdb;
      dataDir = "/home/florian/.var/lib";
    };
  };

  # Define a user account. Don't forget to set a password with ‘passwd’.
  users.extraUsers.florian= {
    isNormalUser = true;
    uid = 1000;
    createHome = true;
    extraGroups = [ "wheel" "networmanager" "audio" "docker" ];
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

  # The NixOS release to be compatible with for stateful data such as databases.
  system.stateVersion = "18.03";
}
