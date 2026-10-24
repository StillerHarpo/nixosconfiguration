{
  config,
  pkgs,
  lib,
  private,
  ...
}:
let
  cfg = config.wireguard;
in
{
  options.wireguard.rpi4.enable = lib.mkEnableOption (
    lib.mdDoc "Wether to use wireguard to connect to rpi4"
  );
  options.wireguard.vpn.enable = lib.mkEnableOption (
    lib.mdDoc "Wether to use wireguard to connect to vpn"
  );

  config = lib.mkMerge [
    {
      networking =
        let
          listenPort = 51821;
        in
        lib.mkIf cfg.rpi4.enable {
          firewall = {
            allowedUDPPorts = [ listenPort ];
          };
          wireguard.interfaces.wgHome = {
            ips = [ "10.100.0.2/24" ];
            inherit listenPort;
            privateKeyFile = config.age.secrets.thinkpadWireguardPrivate.path;
            peers = [
              {
                publicKey = "obpVqe+eZULOcAqlZk+jUhqFXIjFtzhQG9jgbDUCvQU=";
                allowedIPs = [ "192.168.178.53/32" ];
                endpoint = "${private.dyndns}:51820";
                persistentKeepalive = 25;
              }
            ];
          };
        };
    }
    (lib.mkIf cfg.vpn.enable (
      let
        runInNetworkNamespace = "${pkgs.util-linux}/bin/nsenter --net=/var/run/netns/airvpnns /run/wrappers/bin/sudo -H -u florian -g users --preserve-env=WAYLAND_DISPLAY,XDG_RUNTIME_DIR,DBUS_SESSION_BUS_ADDRESS";
        librewolfAirvpn = pkgs.writeShellScriptBin "librewolf-airvpn" ''
          ${runInNetworkNamespace} ${lib.getExe pkgs.librewolf} -P private --new-window ''${1}
        '';
        firefoxAirvpn = pkgs.writeShellScriptBin "firefox-airvpn" ''
          ${runInNetworkNamespace} ${lib.getExe pkgs.firefox} -P private --private-window ''${1}
        '';
      in
      {

        networking = {
          wireguard.interfaces.airvpn = {
            preSetup = ''
              ip netns add airvpnns || true
              ip -n airvpnns link set dev lo up
            '';
            postSetup = ''
              ip netns exec airvpnns wg set airvpn listen-port 0
              ip netns exec airvpnns wg set airvpn fwmark 0
              ip -n airvpnns link set dev airvpn mtu 1320 up
            '';
            interfaceNamespace = "airvpnns";
            ips = [
              "10.147.121.110"
              "fd7d:76ee:e68f:a993:a7f4:8d2b:fb6:5733"
            ];
            privateKeyFile = config.age.secrets.vpnPrivateKey.path;
            mtu = 1320;
            peers = [
              {
                publicKey = "PyLCXAQT8KkM4T+dUsOQfn+Ub3pGxfGlxkIApuig+hk=";
                presharedKeyFile = config.age.secrets.vpnPresharedKey.path;
                endpoint = "213.152.187.217:1637";
                allowedIPs = [
                  "0.0.0.0/0"
                  "::0/0"
                ];
                persistentKeepalive = 15;
              }
            ];
          };
          wireguard.interfaces.work = {
            ips = [ "192.168.42.6/32" ];
            privateKeyFile = config.age.secrets.vpnWorkPrivateKey.path;
            peers = [
              {
                publicKey = "YIoEH1PX+9bXW28Cyhcy5UEO13sgyMIgvCJ24bSMbSo=";
                presharedKeyFile = config.age.secrets.vpnWorkPresharedKey.path;
                endpoint = private.workEndpoint;
                allowedIPs = [
                  "192.168.1.122/32"
                ];
              }
            ];
          };
        };
        environment.etc."netns/airvpnns/resolv.conf".text = "nameserver 10.128.0.1";

        environment.systemPackages = [
          librewolfAirvpn
          firefoxAirvpn
        ];
        apparmor.systemPackages = [
          {
            pkgs = [ pkgs.firefox ];
            rules = ''
              network,
              capability sys_admin,
              capability sys_chroot,
              capability sys_ptrace,
              ${lib.apparmor.generateFileRules {
                allows = [ "firefox" ];
                inherit config;
              }}
            '';
          }
        ];
        security.sudo = {
          extraRules = [
            {
              commands = [
                {
                  command = "/run/current-system/sw/bin/librewolf-airvpn";
                  options = [
                    "NOPASSWD"
                  ];
                }
                {
                  command = "/run/current-system/sw/bin/firefox-airvpn";
                  options = [
                    "NOPASSWD"
                  ];
                }
              ];
              runAs = "root";
              users = [ "florian" ];
            }
          ];
          extraConfig = ''
            Defaults!/run/current-system/sw/bin/librewolf-airvpn env_keep += "WAYLAND_DISPLAY XDG_RUNTIME_DIR DBUS_SESSION_BUS_ADDRESS"
            Defaults!/run/current-system/sw/bin/firefox-airvpn env_keep += "WAYLAND_DISPLAY XDG_RUNTIME_DIR DBUS_SESSION_BUS_ADDRESS"
          '';
        };
        home-manager.users.florian = {
          librewolf = {
            enable = true;
            additionalProfiles.private.id = 2;
          };
          firefox = {
            enable = true;
            additionalProfiles.private.id = 2;
          };
        };
      }
    ))
  ];
}
