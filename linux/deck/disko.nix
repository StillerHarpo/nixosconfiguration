{ ... }:
{
  disko.devices = {
    disk = {
      mydisk = {
        type = "disk";
        device = (import ../../variables.nix).nixosDeckDisk;
        content = {
          type = "gpt";
          partitions = {
            ESP = {
              type = "EF00";
              size = "600M";
              content = {
                type = "filesystem";
                format = "vfat";
                mountpoint = "/boot";
              };
            };
            root = {
              size = "100%";
              content = {
                  type = "filesystem";
                  format = "ext4";
                  mountpoint = "/";
              };
            };
          };
        };
      };
    };
  };
}
