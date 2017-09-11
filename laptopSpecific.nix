# here are every configs that are used on my laptop but not on my workstation

{ config, pkgs, ... }:

{
  boot = {
    # luks encryption
    initrd.luks.devices.luksroot.device = "/dev/sda2";

    # Use the GRUB 2 boot loader.
    loader.grub = {
      enable = true;
      version = 2;
    };

    # Define on which hard drive you want to install Grub.
    loader.grub.device = "/dev/sda"; # or "nodev" for efi only

    # sound
    extraModprobeConfig = ''
      options snd slots=snd-hda-intel 
      options snd_hda_intel enable=1,1
    '';
    blacklistedKernelModules = [ "snd_pcsp" ];
  };
  
  # powerManagement.enable = false;
  services = { 
    # Go in hibernate at lid
    logind.extraConfig = ''
      HandleLidSwitch=hibernate
      HandleLidSwitchDocked=hibernate
      HandlePowerKey=hibernate
    '';
    # mouse pad
    xserver.synaptics = { 
      enable = true;
      twoFingerScroll = true;
    };
  };

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

  # wifi
  networking.wireless.enable = true;  
  system.activationScripts.wpa_supplicant=
   "ln -sfn /etc/nixos/wpa_supplicant.conf /etc/wpa_supplicant.conf";
}
