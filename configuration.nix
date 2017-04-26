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
    consoleKeyMap = "de";
    defaultLocale = "en_US.UTF-8";
  };

  # Set your time zone.
  time.timeZone = "Europe/Berlin";

  # List packages installed in system profile. To search by name, run:
  # $ nix-env -qaP | grep wget
  nixpkgs.config.allowUnfree = true;
  environment.systemPackages = with pkgs; [
    wget
    (vimHugeX.override { python = python3; })
    sudo
    firefox
    git
    pass
    gnupg
    termite
    mutt
    haskellPackages.xmonad-contrib
    haskellPackages.xmonad-extras
    termite
    toxic
    rtv
    xsel
    python
    python3
    python27Packages.pip
    zathura
    spotify
    mpv
    rlwrap
    translate-shell
    you-get
    pandoc
    texlive.combined.scheme-full
  ];
  

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
  powerManagement.enable = false;
  services = {
    logind.extraConfig = "HandleLidSwitch=ignore"; 
    acpid = { 
      enable = true;
      # only hibernate ad lid-close, not after lid-open
      lidEventCommands = 
        ''
          if [[ $(cat /var/local/lid) == "1" ]]
          then
            echo 0 > /var/local/lid
            systemctl hibernate
          else
            echo 1 > /var/local/lid
          fi
        '';
    };

    # Enable the X11 windowing system.
    xserver = {
      enable = true;
      layout = "de";
      xkbOptions = "eurosign:e";
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

  # zsh
  programs.zsh.enable = true;
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

  # Bluetooth sound
  hardware = { 
    pulseaudio = { 
      enable = true; 
      package = pkgs.pulseaudioFull;
    }; 
    bluetooth.enable = true;
  };
  
  networking.wireless.enable = true;  

  system = {
  # The NixOS release to be compatible with for stateful data such as databases.
    stateVersion = "17.03";
    activationScripts.wpa_supplicant="ln -sfn /etc/nixos/wpa_supplicant.conf /etc/wpa_supplicant.conf";
  };
}
