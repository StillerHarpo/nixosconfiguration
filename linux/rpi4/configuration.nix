{ config, pkgs, lib, inputs, private, ... }:

let sshKeys = import ../thinkpad/sshKeys.nix; in

{

  imports = [ ./disko.nix ];
  age = {
    identityPaths = [ "/root/.ssh/rpi4" ];
    secrets.rpi4WireguardPrivate.file = ./secrets/rpi4WireguardPrivate.age;
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
    package = pkgs.nixUnstable;
    extraOptions = ''
      experimental-features = nix-command flakes
      keep-outputs = true
      keep-derivations = true
    '';
    registry.nixpkgs.flake = inputs.nixpkgs;
    nixPath = [ "nixpkgs=${inputs.nixpkgs.outPath}" ];
    settings = {
      substituters = lib.mkAfter [
        "https://nix-community.cachix.org?priority=50"
      ];
      trusted-public-keys = [
        "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
        "ssh-serve:WltAxNNiDoufj5yg7k9tJHKoN7D5PgIZUyqDOMBOaGM="
      ];
      trusted-users = [ "root" "florian" ];
      auto-optimise-store = true;
    };
    gc = {
      persistent = true;
      automatic = true;
      dates = "weekly";
      options = "--delete-older-than 7d";
    };
  };

  services = {
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
            switch = "0x0017880109abe4d3";
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
              friendly_name = "lights/all";
              devices = with devices; [ lights_kitchen_one lights_kitchen_two lights_kitchen_three lights_livingRoom];
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
        } // import ../cords.nix;
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
    firewall.allowedUDPPorts = [ 51820 ];
    firewall.allowedTCPPorts = [ 5232 ];

    wireguard.interfaces = {
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

  systemd.services.lightAutomation = {
      enable = true;
      description = "Automatically change color";
      wants = [ "zigbee2mqtt.service" "mosquitto.service" ];
      after = [ "zigbee2mqtt.service" "mosquitto.service" ];
      serviceConfig = {
        ExecStart = pkgs.writers.writeHaskell
          "light-automation"
          {
            libraries = with pkgs.haskellPackages; [ aeson net-mqtt network-uri ];
          }
          ../../haskell/home-automation/automation.hs;
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

  system.stateVersion = "23.11";

}
