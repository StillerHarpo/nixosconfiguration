let
  sshKeys = import ../linux/thinkpad/sshKeys.nix;
in

{ inputs, ... }:
{

  systemd.network.enable = true;

  systemd.network.networks."10-lan" = {
    matchConfig.Name = [
      "enp0s31f6"
      "wlp61s0"
      "vm-*"
    ];
    networkConfig = {
      Bridge = "br0";
    };
  };

  systemd.network.netdevs."br0" = {
    netdevConfig = {
      Name = "br0";
      Kind = "bridge";
    };
  };

  systemd.network.networks."10-lan-bridge" = {
    matchConfig.Name = "br0";
    networkConfig = {
      Address = [
        "192.168.1.2/24"
        "2001:db8::a/64"
      ];
      Gateway = "192.168.1.1";
      DNS = [ "192.168.1.1" ];
      IPv6AcceptRA = true;
    };
    linkConfig.RequiredForOnline = "routable";
  };

  microvm.vms = {
    work = {
      autostart = false;
      config =
        { pkgs, ... }:
        {
          microvm = {
            registerWithMachined = true;

            shares = [
              {
                source = "/nix/store";
                mountPoint = "/nix/.ro-store";
                tag = "ro-store";
                proto = "virtiofs";
              }
              # {
              #   proto = "virtiofs";
              #   tag = "emacs";
              #   # Source path can be absolute or relative
              #   # to /var/lib/microvms/$hostName
              #   source = "/home/florian/emacs-work";
              #   mountPoint = "/run/user/1000/emacs";
              # }
            ];

            interfaces = [
              {
                type = "tap";
                id = "vm-tap0";
                mac = "02:00:00:00:00:01";
              }
            ];
          };

          services.qemuGuest.enable = true;
          systemd.services."qemu-guest-agent".wantedBy = [ "multi-user.target" ];

          users.users.root.openssh.authorizedKeys.keys = sshKeys;

          services.openssh = {
            enable = true;
            settings = {
              PermitRootLogin = "yes";
              PasswordAuthentication = false;
            };
          };

          systemd.network.enable = true;

          systemd.network.networks."20-lan" = {
            matchConfig.Type = "ether";
            networkConfig = {
              Address = [
                "192.168.1.3/24"
                "2001:db8::b/64"
              ];
              Gateway = "192.168.1.1";
              DNS = [ "192.168.1.1" ];
              IPv6AcceptRA = true;
              DHCP = "no";
            };
          };

          system.stateVersion = "25.11";
          # home-manager.users.florian.emacs.enable = true;
          # users.users.florian.isNormalUser = true;
          # services.getty = {
          #   autologinUser = "florian";
          #   autologinOnce = true;
          # };
          services.emacs = {
            enable = true;
            package = pkgs.emacs-unstable;
          };

          nixpkgs.overlays = [
            inputs.emacs-overlay.overlay
          ];

        };

    };
  };
}
