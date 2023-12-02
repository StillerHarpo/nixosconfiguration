{ config, pkgs, lib, inputs, private, ... }:

let listenPort = 51820;
in {
  options.wireguard.enable = lib.mkEnableOption (lib.mdDoc "Wether to use wireguard");

  config = lib.mkIf config.wireguard.enable {
    networking = {
      firewall = {
        allowedUDPPorts = [ listenPort ];
      };
      wireguard.interfaces.wgHome =
        {
          ips = [ "10.100.0.2/24" ];
          inherit listenPort;
          privateKeyFile = config.age.secrets.thinkpadWireguardPrivate.path;
          peers = [
            {
              publicKey = "obpVqe+eZULOcAqlZk+jUhqFXIjFtzhQG9jgbDUCvQU=";
              allowedIPs = ["192.168.178.53/32"];
              endpoint = "${private.dyndns}:51820";
              persistentKeepalive = 25;
            }
          ];
        };
    };
  };
}
