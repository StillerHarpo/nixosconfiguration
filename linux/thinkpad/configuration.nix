{
  config,
  ...
}:
{
  imports = [
    ../laptop/configuration.nix
  ];
  age.secrets = {
    syncthingKey.file = ./secrets/syncthingKey.age;
    syncthingCert.file = ./secrets/syncthingCert.age;
  };

  networking.hostName = "nixosThinkpad";

  services.tlp.enable = true;

  syncthing = {
    key = config.age.secrets.syncthingKey.path;
    cert = config.age.secrets.syncthingCert.path;
    otherLaptopId = "PXLXTKW-HHFWKWM-MVO37BR-KR53C2Z-BN7GGZE-MVYM633-BC4FRTC-KORMAAK";
  };
  networkmanager.lanInterface = "enp0s31f6";

  microvm.work = {
    sshKeys = import ./sshKeys.nix;
    externalInterfaces = [
      "wlp61s0"
      "enp0s20f0u4u4c2"
      "enp0s31f6"
    ];
  };

  # The NixOS release to be compatible with for stateful data such as databases.
  system.stateVersion = "24.11";
  home-manager.users.florian = { config, ... }: {
    home = {
      file = {
        ".private-home/.zshrc".source = config.home.file."./.zshrc".source;
        ".private-home/.zshenv".source = config.home.file."./.zshenv".source;
      };
      stateVersion = "24.11";
    };
  };
  myHardware.enable = true;

  age.identityPaths = [ "/root/.ssh/id_rsa" ];
}
