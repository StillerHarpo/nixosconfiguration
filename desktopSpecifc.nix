{ config, pkgs, ... }:

{

  # Use the systemd-boot EFI boot loader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;
  
  # luks encryption
  boot.initrd.luks.devices.luksroot.device = "/dev/sdd2";
  
  # fancontrol
  boot.kernelModules = [ "it87" ];
  environment.systemPackages = [ pkgs.lm_sensors ];
  systemd.services.fancontrol = {
    description = "fancontrol daemon";
    wantedBy = [ "multi-user.target" ];
    script = "${pkgs.lm_sensors}/sbin/fancontrol /etc/nixos/fancontrol";
  };
}