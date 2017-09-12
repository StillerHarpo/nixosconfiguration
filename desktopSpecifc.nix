{ config, pkgs, ... }:

{

  environment.systemPackages = with pkgs; [ 
    lm_sensors 
    hdparm ];

  # Use the systemd-boot EFI boot loader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;
  
  # luks encryption
  boot.initrd.luks.devices.luksroot.device = "/dev/sdd2";
  
  # fancontrol
  boot.kernelModules = [ "it87" ];
  systemd.services.fancontrol = {
    description = "fancontrol daemon";
    wantedBy = [ "multi-user.target" ];
    script = "${pkgs.lm_sensors}/sbin/fancontrol /etc/nixos/fancontrol";
  };

  # stop unused hdds
  systemd.services.hdparm = {
    description = "stop unused hdds";
    wantedBy = [ "multi-user.target" ];
    script = "${pkgs.hdparm}/sbin/hdparm -Y /dev/sda /dev/sdc";
  };
}
