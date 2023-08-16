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

  environment.systemPackages = with pkgs; [ vim pciutils lshw ripgrep bat fd bottom ];


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
    # TODO replace with mosquitto for better scripting
    home-assistant = {
      enable = true;
      extraComponents = [
        "default_config"
        "esphome"
        "met"
        "radio_browser"
        "zha"
      ];
      openFirewall = true;
      config = let
        kitchen1 = "light.ikea_of_sweden_tradfri_bulb_gu10_cws_345lm_light";
        kitchen2 = "light.ikea_of_sweden_tradfri_bulb_gu10_cws_345lm_light_2";
        kitchen3 = "light.ikea_of_sweden_tradfri_bulb_gu10_cws_345lm_light_3";
      in {
        default_config = {};
        group.Kitchen.entities = [
          "light.Kitchen_Lights"
          kitchen1
          kitchen2
          kitchen3
        ];
        script."'1633813394508'" = {
          sequence = [
            { type = "toggle";
              device_id = "83fdcb46128aa492811c9f20ae9e61d6";
              entity_id = kitchen1;
              domain = "light";
            }
            { type = "toggle";
              device_id = "70c862bc02e6a0bf238c77eac05777b6";
              entity_id = kitchen2;
              domain = "light";
            }
            { type = "toggle";
              device_id = "0da9bbd3750d5330ba82115549a889a0";
              entity_id = kitchen3;
              domain = "light";
            }
          ];
          mode = "single";
          alias = "kitchen light toggle";
        };

        homeassistant = {
          name = "Home";
          unit_system = "metric";
          customize = {
            "${kitchen1}".friendly_name = "kitchen1";
            "${kitchen2}".friendly_name = "kitchen2";
            "${kitchen3}".friendly_name = "kitchen3";
          };
        } // import ../cords.nix;

        light = [
          { platform = "group";
            name = "Kitchen Lights";
            entities = [
              kitchen1
              kitchen2
              kitchen3
            ];
          }
          { platform = "group";
            name = "All Lights";
            entities = [
              kitchen1
              kitchen2
              kitchen3
              "light.signify_netherlands_b_v_lta009_light"
            ];
          }];

      };
    };
  };
  
  users.users.root.openssh.authorizedKeys.keys = sshKeys;
  # TODO https://nixos.wiki/wiki/WireGuard
  system.stateVersion = "23.11";
}
