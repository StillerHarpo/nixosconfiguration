{ config, pkgs, lib, inputs, private, outputs, ... }:

let sshKeys = import ../thinkpad/sshKeys.nix; in

{

  imports = [
    ./disko.nix
    ../configuration.nix
  ];
  age = {
    identityPaths = [ "/root/.ssh/rpi4" ];
    secrets = {
      rpi4WireguardPrivate.file = ./secrets/rpi4WireguardPrivate.age;
      flake-update.file = ./secrets/flake-update.age;
      syncthingKey.file = ./secrets/syncthingKey.age;
      syncthingCert.file = ./secrets/syncthingCert.age;
    };
  };

  # Select internationalisation properties.
  console.keyMap = "us";
  i18n.defaultLocale = "en_US.UTF-8";

  # Set your time zone.
  time.timeZone = "Europe/Berlin";

  boot = {
    # doesn't work, don't now why
    # there is nothing in the systemd log
    initrd = {
      enable = true;
      availableKernelModules = [ "genet" "brcmfmac" ];
      network = {
        enable = true;
        ssh = {
          enable = true;
          port = 2222;
          hostKeys = [ "/boot/initrd-ssh-key" ];
          authorizedKeys = sshKeys;
        };
        postCommands = ''
          echo 'cryptsetup-askpass' >> /root/.profile
        '';
      };
    };

    loader = {
      efi.canTouchEfiVariables = true;
      systemd-boot.enable = true;
    };
  };

  environment.systemPackages = with pkgs; [
    vim pciutils lshw ripgrep bat fd bottom busybox
    (haskellPackages.ghcWithPackages (hpkgs: [ hpkgs.net-mqtt ]))
    (python3.withPackages (py: [ py.zigpy-znp ]))
  ];

  nix = {
    registry.nixpkgs.flake = inputs.nixpkgs;
    nixPath = [ "nixpkgs=${inputs.nixpkgs.outPath}" ];
  };

  nixpkgs.overlays = builtins.attrValues outputs.overlays;

  services = {
    tailscale.enable = true;
    radicale = {
      enable = true;
      settings.server.hosts = [ "0.0.0.0:5232" ];
    };
    snowflake-proxy.enable = true;
    openssh = {
      enable = true;
      settings = {
        PermitRootLogin = "yes";
        PasswordAuthentication = false;
      };
    };
    adguardhome = {
      enable = true;
      openFirewall = true;
      settings = {};
    };
    mosquitto = {
      enable = true;
      listeners = [
        {
          acl = [ "pattern readwrite #" ];
          omitPasswordAuth = true;
          settings.allow_anonymous = true;
        }
  ];
    };
    zigbee2mqtt = {
      enable = true;
      settings =
        let
          devices = {
            lights_kitchen_one = "0x680ae2fffe68e0d1";
            lights_kitchen_two = "0x680ae2fffe6a2c41";
            lights_kitchen_three = "0x680ae2fffe6b7851";
            lights_livingRoom = "0x001788010b236902";
            lights_dinerRoom = "0x881a14fffe8eb8f9";
            lights_hallway = "0x881a14fffea09b0f";
            lights_bedroom = "0x881a14fffe8ed90c";
            switch_hallway = "0x0017880109abe4d3";
            switch_kitchen = "0x980c33fffefae5f3";
            switch_livingRoom = "0x980c33fffefa99a2";
            switch_bedroom = "0x60b647fffecf33dd";
          };
        in { 
          permit_join = true;
          homeassistant = config.services.home-assistant.enable;
          serial = {
            port = "/dev/serial/by-id/usb-Silicon_Labs_slae.sh_cc2652rb_stick_-_slaesh_s_iot_stuff_00_12_4B_00_23_93_30_07-if00-port0";
            disable_led = true;
          };

          passlist = lib.attrValues devices; 
          devices =
            lib.concatMapAttrs
              (name: device: { ${device}.friendly_name = builtins.replaceStrings ["_"] ["/"] name; })
              devices;
          groups = {
            "1" = {
              friendly_name = "lights/kitchen/all";
              devices = with devices; [ lights_kitchen_one lights_kitchen_two lights_kitchen_three];
            };
            "2" = {
              friendly_name = "lights/kitchenAndDinerRoom";
              devices = with devices; [ lights_kitchen_one lights_kitchen_two lights_kitchen_three lights_dinerRoom];
            };
            "3" = {
              friendly_name = "lights/all";
              devices = with devices; [ lights_kitchen_one lights_kitchen_two lights_kitchen_three lights_hallway lights_livingRoom lights_dinerRoom lights_bedroom];
            };
          };
        };
    };
    home-assistant = {
      enable = true;
      extraComponents = [
        "default_config"
        "esphome"
        "met"
        "radio_browser"
        "mqtt"
      ];
      openFirewall = true;
      config = {
        default_config = {};

        homeassistant = {
          name = "Home";
          unit_system = "metric";
        } // private.cords;
      };
    };
    syncthing = {
      enable = true;
      key = config.age.secrets.syncthingKey.path;
      cert = config.age.secrets.syncthingCert.path;
      settings = {
        devices = {
          "android".id =
            "VWFGCVO-56ZMY6L-5N7MQ5F-GB4TJFS-AHAGT5L-WYN4WTS-TQJHEVN-NBBOOAS";
          "thinkpad".id =
            "GS7BXPV-7IXKM6U-MF2SCHO-XSBUAKM-WMQ7XQT-2P6NW3C-H6Y2VW4-X7XP6AI";
        };
        folders = {
          "android-backups" = {
            path = "/root/android/backups";
            devices = [ "android" "thinkpad" ];
          };
          "org-roam" = {
            path = "/root/org-roam";
            devices = [ "android" "thinkpad" ];
            versioning = {
              type = "simple";
              params.keep = "5";
            };
          };
        };
      };
    };
  };
  
  users.users.root.openssh.authorizedKeys.keys = sshKeys;

  networking = {
    nat = {
      enable = true;
      externalInterface = "enabcm6e4ei0";
      internalInterfaces = [ "wgHome" ];
    };
    firewall.allowedUDPPorts = [ 51820 53 ];
    firewall.allowedTCPPorts = [ 5232 ];

    wireguard = {
      enable = false;
      interfaces = {
        wgHome = {
          ips = [ "10.100.0.1/24" ];

          listenPort = 51820;

          privateKeyFile = config.age.secrets.rpi4WireguardPrivate.path;

          postSetup = ''
         ${pkgs.iptables}/bin/iptables -t nat -A POSTROUTING -s 10.100.0.0/24 -o enabcm6e4ei0 -j MASQUERADE
       '';

          postShutdown = ''
         ${pkgs.iptables}/bin/iptables -t nat -D POSTROUTING -s 10.100.0.0/24 -o enabcm6e4ei0 -j MASQUERADE
       '';

          peers = [
            { name = "thinkpad";
              publicKey = "2uKX2MDYjMUHghbb+csZJ5MZwqRnIQCVSBt6hYJzNRQ=";
              allowedIPs = [ "10.100.0.2/32" ];
            }
            {
              name = "fairphone";
              publicKey = "wANts2w3caH/+zjsoNbMRz1SCGDd5IyFObLXXvCaMDE=";
              allowedIPs = [ "10.100.0.3/32" ];
            }
          ];
        };
      };
    };
  };

  systemd.services.lightAutomation = {
      enable = true;
      description = "Automatically change color";
      wantedBy = [ "zigbee2mqtt.service" "mosquitto.service" ];
      after = [ "zigbee2mqtt.service" "mosquitto.service" ];
      partOf = [ "zigbee2mqtt.service" "mosquitto.service" ];
      bindsTo = [ "zigbee2mqtt.service" "mosquitto.service" ];
      serviceConfig = {
        ExecStart = pkgs.writers.writeHaskell
          "light-automation"
          {
            libraries = with pkgs.haskellPackages;
              [ aeson net-mqtt network-uri home-automation unbounded-delays];
          }
          ../../haskell/home-automation/Main.hs;
        CapabilityBoundingSet = "";
        LockPersonality = true;
        MemoryDenyWriteExecute = false;
        NoNewPrivileges = true;
        PrivateDevices = false;
        PrivateUsers = true;
        PrivateTmp = true;
        ProtectClock = true;
        ProtectControlGroups = true;
        ProtectHome = true;
        ProtectHostname = true;
        ProtectKernelLogs = true;
        ProtectKernelModules = true;
        ProtectKernelTunables = true;
        ProtectProc = "invisible";
        ProcSubset = "pid";
        ProtectSystem = "strict";
        RemoveIPC = true;
        RestrictAddressFamilies = [
          "AF_INET"
          "AF_INET6"
        ];
        RestrictNamespaces = true;
        RestrictRealtime = true;
        RestrictSUIDSGID = true;
        SystemCallArchitectures = "native";
        SystemCallFilter = [
          "@system-service @pkey"
          "~@privileged @resources"
        ];
        UMask = "0077";
      };
  };


  flake-remote-update = {
    enable = true;
    remote = "https://gitlab.com/StillerHarpo/nixosconfiguration";
    credentials = {
      user = "StillerHarpo";
      passwordFile = config.age.secrets.flake-update.path;
    };
    updateBranch = "update";
    mainBranch = "master";
    dates = "Monday 01:00";
  };

  system.stateVersion = "23.11";

}
