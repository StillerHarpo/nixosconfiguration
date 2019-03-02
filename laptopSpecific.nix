# here are every configs that are used on my laptop but not on my workstation

{ config, pkgs, ... }:

{
  # luks encryption
  boot.initrd.luks.devices.luksroot.device = "/dev/disk/by-uuid/6d8ca465-1ff7-45a5-88d3-9aa0b4807cb7";

  # powerManagement.enable = false;
  services = {
    # Go in hibernate at lid
    logind.extraConfig = ''
      HandlePowerKey=ignore
      HandleLidSwitch=hibernate
      HandleLidSwitchDocked=hibernate
    '';
    # mouse pad
    xserver = {
      dpi = 180;
      synaptics = {
        enable = true;
        twoFingerScroll = true;
      };
    };
  };
  # Bluetooth sound
  hardware.bluetooth.enable = true;

  # wifi
  networking.wireless.enable = true;
  system.activationScripts.wpa_supplicant=
   "ln -sfn /etc/nixos/wpa_supplicant.conf /etc/wpa_supplicant.conf";

  # big font for high resolution
  i18n.consoleFont = "sun12x22";
}
