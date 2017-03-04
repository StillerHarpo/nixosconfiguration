# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, ... }:

{
  imports =
    [ # Include the results of the hardware scan.
      ./hardware-configuration.nix
      ./network-configuration.nix
    ];

  # luks encryption
  boot.initrd.luks.devices = [ 
    { name = "luksroot"; device = "/dev/sda2"; preLVM = true; } 
  ];


  # Use the GRUB 2 boot loader.
  boot.loader.grub.enable = true;
  boot.loader.grub.version = 2;
  # Define on which hard drive you want to install Grub.
  boot.loader.grub.device = "/dev/sda"; # or "nodev" for efi only

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
  environment.systemPackages = with pkgs; [
    wget
    vim
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
  ];


  # List services that you want to enable:

  # Enable the OpenSSH daemon.
  # services.openssh.enable = true;

  # Enable CUPS to print documents.
  # services.printing.enable = true;

  # Enable the X11 windowing system.
  services.xserver = {
  enable = true;
  layout = "de";
  xkbOptions = "eurosign:e";
  synaptics.enable = true;
  synaptics.twoFingerScroll = true;

  # Enable XMonad
  windowManager.xmonad.enable = true;
  windowManager.xmonad.enableContribAndExtras = true;
 }; 

  # Enable the KDE Desktop Environment.
  # services.xserver.displayManager.kdm.enable = true;
  # services.xserver.desktopManager.kde4.enable = true;
  
  # Define a user account. Don't forget to set a password with ‘passwd’.
  users.extraUsers.florian= {
    isNormalUser = true;
    uid = 1000;
    createHome = true;
    extraGroups = [ "wheel" "networmanager" ];
  };

  # zsh

  programs.zsh.enable = true;
  users.defaultUserShell = "/run/current-system/sw/bin/zsh";
  # The NixOS release to be compatible with for stateful data such as databases.
  system.stateVersion = "16.09";

}
