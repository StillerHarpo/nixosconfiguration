# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, ... }:

{
  imports =
    [ # Include the results of the hardware scan.
      ./hardware-configuration.nix
    ];

  # luks encryption
  boot.initrd.luks.devices = [ 
    { name = "luksroot"; device = "/dev/sda2"; preLVM = true; } 
  ];

# # File systems
# fileSystems."/" = {
#   mountPoint = "/";
#   device = "/dev/mapper/MyVol-root";
# }; 

# fileSystems."/boot" = {
#   mountPoint = "/boot";
#   device = "/dev/sda1";
# }; 

  # Use the GRUB 2 boot loader.
  boot.loader.grub.enable = true;
  boot.loader.grub.version = 2;
  # boot.loader.grub.efiSupport = true;
  # boot.loader.grub.efiInstallAsRemovable = true;
  # boot.loader.efi.efiSysMountPoint = "/boot/efi";
  # Define on which hard drive you want to install Grub.
  boot.loader.grub.device = "/dev/sda"; # or "nodev" for efi only

  # networking.hostName = "nixos"; # Define your hostname.
  # networking.wireless.enable = true;  # Enables wireless support via wpa_supplicant.

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
  ];

  # List services that you want to enable:

  # Enable the OpenSSH daemon.
  # services.openssh.enable = true;

  # Enable CUPS to print documents.
  # services.printing.enable = true;

  # Enable the X11 windowing system.
  services.xserver.enable = true;
  services.xserver.layout = "de";
  services.xserver.xkbOptions = "eurosign:e";

  # Enable XMonad
  services.xserver.windowManager.xmonad.enable = true;

  # Enable the KDE Desktop Environment.
  # services.xserver.displayManager.kdm.enable = true;
  # services.xserver.desktopManager.kde4.enable = true;


  # Define a user account. Don't forget to set a password with ‘passwd’.
  users.extraUsers.florian= {
    isNormalUser = true;
    uid = 1000;
    home = "/home/florian";
    extraGroups = [ "wheel" "networmanager" ];
  };

  # zsh

  programs.zsh.enable = true;
  users.defaultUserShell = "/run/current-system/sw/bin/zsh";
  # The NixOS release to be compatible with for stateful data such as databases.
  system.stateVersion = "16.09";

}
