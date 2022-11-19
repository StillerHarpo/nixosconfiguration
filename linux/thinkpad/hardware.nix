{ config, lib, pkgs, ... }:

{
  boot = {
    initrd = {
      # luks encryption
      luks.devices.luksroot.device =
        "/dev/disk/by-uuid/6d8ca465-1ff7-45a5-88d3-9aa0b4807cb7";
      availableKernelModules = [ "xhci_pci" "nvme" "usb_storage" "sd_mod" ];
    };
    kernelModules = [ "kvm-intel" ];
  };

  fileSystems = {
    "/" = {
      device = "/dev/disk/by-uuid/f97f5f90-314e-434a-8585-42694d5cf202";
      fsType = "ext4";
    };

    "/boot" = {
      device = "/dev/disk/by-uuid/C998-C706";
      fsType = "vfat";
    };
  };

  swapDevices =
    [{ device = "/dev/disk/by-uuid/f1429c26-3127-4932-8051-face01ca9ac8"; }];

}
