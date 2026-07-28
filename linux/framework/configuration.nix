{
  config,
  ...
}:
{
  imports = [
    ./disko.nix
    ../laptop/configuration.nix
  ];

  age.secrets = {
    syncthingKey.file = ./secrets/syncthingKey.age;
    syncthingCert.file = ./secrets/syncthingCert.age;
  };

  networking.hostName = "nixosFramework";

  syncthing = {
    key = config.age.secrets.syncthingKey.path;
    cert = config.age.secrets.syncthingCert.path;
    otherLaptopId = "GS7BXPV-7IXKM6U-MF2SCHO-XSBUAKM-WMQ7XQT-2P6NW3C-H6Y2VW4-X7XP6AI";
  };
  networkmanager.lanInterface = "enp195s0f0u2u3";

  microvm.work = {
    sshKeys = import ./sshKeys.nix;
    externalInterfaces = [
      "wlp191s0"
      "enp195s0f0u2u3"
    ];
    mem = 49152;
    vcpu = 20;
  };

  # The NixOS release to be compatible with for stateful data such as databases.
  system.stateVersion = "26.05";
  home-manager.users.florian = { config, ... }: {
    home = {
      file = {
        ".private-home/.config/zsh/.zshenv".source = config.home.file.".config/zsh/.zshenv".source;
        ".private-home/.config/zsh/.zshrc".source = config.home.file.".config/zsh/.zshrc".source;
      };
      stateVersion = "26.05";
    };
  };

  services.batteryNotifier.device = "BAT1";

  age.identityPaths = [ "/root/.ssh/id_ed25519" ];
}
