{ ... }:
{
  disko.devices = {
    disk = {
      mydisk = {
        type = "disk";
        device = (import ../../variables.nix).nixosRpi4Disk;
        content = {
          type = "gpt";
          partitions = {
            encrypted = {
              size = "100G";
              content = {
                type = "luks";
                name = "crypted";
                keyFile = "/tmp/secret.key";
                content = {
                  type = "filesystem";
                  format = "ext4";
                  mountpoint = "/home/florian/encrypted";
                };
              };
            };
          };
        };
      };
    };
  };
}
