# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, ... }:

{
  imports =
    [ # Include the results of the hardware scan.
      /etc/nixos/hardware-configuration.nix
    ];

  # luks encryption
  boot = {
    initrd = {
      luks.devices = [ 
        { name = "luksroot"; device = "/dev/sda2"; preLVM = true; } 
      ];
      postDeviceCommands = "sleep 5";
    };

    # Use the GRUB 2 boot loader.
    loader.grub = {
      enable = true;
      version = 2;

      # Define on which hard drive you want to install Grub.
      device = "/dev/sda"; # or "nodev" for efi only
    };
    extraModprobeConfig = ''
      options snd slots=snd-hda-intel 
      options snd_hda_intel enable=1,1
    '';
    blacklistedKernelModules = [ "snd_pcsp" ];
    resumeDevice = "/dev/disk/by-label/swap";
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
    sudo
    firefox
    git
    pass
    gnupg
    termite
    mutt
    termite
    toxic
    rtv
    xsel
    python
    python3
    zathura
    spotify
    mpv
    rlwrap
    translate-shell
    you-get
    pandoc
    (texlive.combine {inherit (texlive) scheme-full pygmentex pgf collection-basic;})
    stack
    #busybox
    unclutter
    nix-repl
    steam
    bc
    anki
    cabal2nix
    nix-prefetch-git
    cabal-install 
    unstable.google-chrome
  ] 
  ++ (with haskellPackages; [
    xmonad-contrib
    xmonad-extras
    ncurses
    hdevtools
    hlint
  ]));

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
    };
  };

  fonts = {
    enableFontDir = true;
    fonts = with pkgs; [
      terminus_font
      dejavu_fonts
    ];
  };

  # List services that you want to enable:

  # Enable the OpenSSH daemon.
  # services.openssh.enable = true;

  # Enable CUPS to print documents.
  # services.printing.enable = true;
   
  # Go in hibernate at lid
  # powerManagement.enable = false;
  services = {
    logind.extraConfig = ''
      HandleLidSwitch=hibernate
      HandleLidSwitchDocked=hibernate
      HandlePowerKey=hibernate
    '';
    # Enable the X11 windowing system.
    xserver = {
      enable = true;
      layout = "us";
      xkbOptions = "eurosign:e, caps:escape";
      synaptics = { 
        enable = true;
        twoFingerScroll = true;
      };
      # Enable XMonad
      windowManager.xmonad = {
        enable = true;
        enableContribAndExtras = true;
      };
    }; 
  }; 
  programs = {
    # zsh
    zsh.enable = true;
  };
  users = {  
    defaultUserShell = "/run/current-system/sw/bin/zsh";

    # Define a user account. Don't forget to set a password with ‘passwd’.
    extraUsers.florian= {
      isNormalUser = true;
      uid = 1000;
      createHome = true;
      extraGroups = [ "wheel" "networmanager" "audio" ];
    };
  };

  security.sudo.wheelNeedsPassword = false;

  # Bluetooth sound
  hardware = { 
    pulseaudio = { 
      enable = true; 
      package = pkgs.pulseaudioFull;
      support32Bit = true;
    }; 
    opengl.driSupport32Bit = true;
    bluetooth.enable = true;
    bumblebee.enable = true;
  };
  
  networking.wireless.enable = true;  

  system = {
  # The NixOS release to be compatible with for stateful data such as databases.
    stateVersion = "17.03";
    activationScripts.wpa_supplicant="ln -sfn /etc/nixos/wpa_supplicant.conf /etc/wpa_supplicant.conf";
  };
}
