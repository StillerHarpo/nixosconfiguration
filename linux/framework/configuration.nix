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
    nixCachePriv.file = ./secrets/nixCachePrivKey.age;
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

  # https://github.com/NixOS/nixpkgs/issues/477924
  hardware.enableRedistributableFirmware = true;

  # https://github.com/NixOS/nixpkgs/issues/409934
  systemd.services = {
    systemd-suspend.serviceConfig.Environment = "SYSTEMD_SLEEP_FREEZE_USER_SESSIONS=0";
    systemd-hibernate.serviceConfig.Environment = "SYSTEMD_SLEEP_FREEZE_USER_SESSIONS=0";
  };

  services = {
    batteryNotifier.device = "BAT1";
    nix-serve = {
      enable = true;
      openFirewall = true;
      secretKeyFile = config.age.secrets.nixCachePriv.path;
    };
  };

  wayland.screen = {
    id = "BOE NE160QDM-NZ6 Unknown";
    mode = "2560x1600@165.000";
  };

  systemd.services.disable-wake = {
    description = "disable NHI1 wake";
    script = "echo NHI1 > /proc/acpi/wakeup";
    wantedBy = [ "multi-user.target" ];
  };

  age.identityPaths = [ "/root/.ssh/id_ed25519" ];
}
