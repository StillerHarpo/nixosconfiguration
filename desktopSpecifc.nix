{ config, pkgs, ... }:

{
  imports = [ ./pci-passthrough.nix ];
  environment.systemPackages = with pkgs; [ 
    lm_sensors 
    hdparm ];

  boot = {
    # Use the systemd-boot EFI boot loader.
    loader = {
      systemd-boot.enable = true;
      efi.canTouchEfiVariables = true;
    }; 
    # luks encryption
    initrd.luks.devices.luksroot.device = "/dev/sdd2";
    kernelPackages = pkgs.linuxPackages_4_12;
    kernelModules = [ "it87" ];
  };

  systemd.services= { 
    fancontrol = {
      description = "fancontrol daemon";
      wantedBy = [ "multi-user.target" ];
      script = "${pkgs.lm_sensors}/sbin/fancontrol /etc/nixos/fancontrol";
    };

    # stop unused hdds
    hdparm = {
      description = "stop unused hdds";
      wantedBy = [ "multi-user.target" ];
      script = "${pkgs.hdparm}/sbin/hdparm -Y /dev/sda /dev/sdc /dev/sr0";
    };
  };

  # Supposedly better for the SSD.
  fileSystems."/".options = [ "noatime" "nodiratime" "discard" ];
}
