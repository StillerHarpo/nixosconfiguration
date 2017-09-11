{ config, pkgs, ... }:

{

  # Use the systemd-boot EFI boot loader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;
  
  # luks encryption
  boot.initrd.luks.devices.luksroot.device = "/dev/sdd2";


}
