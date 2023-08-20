{ config, pkgs, lib, inputs, private, ... }:

let sshKeys = 
      [
        "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABgQDIc5ug7o0Jqe6SNNrjW5BAyTWqZGVvehJ7GOZrT7DFiiop174CdDlRo4GZlvAGzFEaWr5/4knS0p8kErDPgcgdfC0IL2BYClPEna8agHhyqvmZISxfFDk48Bg/yGo37iPpuGxT7g6VIqI46PnTqgF3nfX1J3crPDD1tDUv5Nq+LH3qlwnpRA3rMBTym/QPkPAM8jQGB4DtyhI1s6UBEQK5vvljhYBG/P54ILQUokYqIsUirQKpBW7Z3sY+zezJpOc+Y6DRZ0rm9dRa6HsOFQ1DQ6u3FkBcyq+vkr4KWmxDdRO0acAV6o0c+1dqyhdaKfklO1E9ZOScTG9Wur9p17qMPsd1zJ8OZ5S7NDMMFi2wcUkxQO9QNqndo+opOBYVMxrz2Hc2Ch2vzuSlVwUxKE60qfARFZ5ZbVOJ8Ate+vghrldgyRF7Sg5yid8Rv6RHv4nvJZpEFjmtkluWzNwhoaF9ifNdB2y7MZeDgu1n1v9xmexSYs97cCB+sprSLYrqgsk= florian@nixos-thinkpad"
        "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABAQCg3kPf2YxtGrfckExx/ckZrsXH1sa0mDGAAVaK6VOpLF4s0fnWJmXrBSfeGgOVPHRCNF3dfQR34u5PwO3gtK8FQr8XvVRKP+EcMi3ztKHcawnThJyB2Zi3D8yTGDYnKRXoiDvitTWtDDmZta91QJZsK65R0SeOGSk1lG6MOYyEhTRQS4rV1Ij8qEqAMB7R7/yUIkdoyGDbcDZ05Bs8/NCfBLJ/pv+pRKp+ZFIjqmKNVYznZ4OT7ywbbQaTNUCoQO4Hcm+ujlOw2jV7Xqb9842uBRQKNKik3hGw1DlAxKrbK/s3Uu0Vs/cUrEoH68+tKZdCzoX3YoYI9cyMRS2+LEkb florian@florian"
        "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABgQDJjjBxd9De7YH9ZCzSLmmBTXYmMOdPaLuToZc5zFe/FUz8V8NhQCmHnPP/wcL2/3gz8jz19PcXKN4jc204Nx+XrFvXPO2BMuaeztwyhLd7o5LLmWEm66PImXq4BPLUH/QTlSumIDPwnehUnorUVEcso2VSBidZ4hor2FTIvP3x3DT19xw+cH19fh6xjSEx3bYk8aQTbXpxQAduou8Q424fmOuaQcCO9f3odYmuhVhmN6hjtCK/NhcKQRQ25C1Ftw+NcC++e2c196J4VLTB3XybKU4BYPI9A1Lq8QHNS16JapGWcvoaamx5o8Br+zxmmBlTJvhMSEUga02QXMrVPkTaDDAwtzSBl1mSD9lx5haCySCH9vsjfR5aoc5Axm1qPiO+kxlLSCmdd7CxexWPR0WbmCt7toPHXGoeVHxY2p6AdeZDdgdr1ccf2iLcE7Y37cwvEBiQx0KCAN3kkYAdSBxFStUDUMtb+cbrnDAqCztg5LIr4GoT5PeXtPW2CCUl74c= root@nixos-thinkpad"
      ]; in

{

  imports = [ ./disko.nix ];

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
      # no clue why this is failing
      # openFirewall = true;
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
            kitchen_one = "0x680ae2fffe68e0d1";
            kitchen_two = "0x680ae2fffe6a2c41";
            kitchen_three = "0x680ae2fffe6b7851";
            livingRoom = "0x001788010b236902";
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
              friendly_name = "kitchen/all";
              devices = with devices; [ kitchen_one kitchen_two kitchen_three];
            };
            "2" = {
              friendly_name = "lights/all";
              devices = with devices; [ kitchen_one kitchen_two kitchen_three livingRoom];
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
  # TODO https://nixos.wiki/wiki/WireGuard
  system.stateVersion = "23.11";
}
