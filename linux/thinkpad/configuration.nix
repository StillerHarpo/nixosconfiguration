# here are every configs that are used on my laptop but not on my workstation

{ config, pkgs, lib, inputs, private, ... }:
{
  imports = [
    ./wireguard.nix
    ./work-container.nix
    ./hardware.nix
    ./hibernate.nix
    ../configuration.nix
    ./backup.nix
  ];

  config = lib.mkMerge [
    (lib.mkIf config.isRealSystem {
      boot = {
        # Use the systemd-boot EFI boot loader.
        loader = {
          systemd-boot.enable = true;
          efi.canTouchEfiVariables = true;
        };
        kernel.sysctl."kernel.yama.ptrace_scope" = 1;
        # we need this for deploy-rs together with import-from-derivation (e.g. callCabal2nix)
        binfmt.emulatedSystems = [ "aarch64-linux" ];
      };
      age = {
        identityPaths = [ "/root/.ssh/id_rsa" ];
        secrets = {
          florian.file = ./secrets/florian.age;
          vpn.file = ./secrets/vpn.age;
          thinkpadWireguardPrivate.file = ./secrets/thinkpadWireguardPrivate.age;
        };
      };
      myHardware.enable = true;
      backup.enable = true;
      wireguard.enable = true;
      networking.wg-quick.interfaces.vpn.configFile = config.age.secrets.vpn.path;

      location = import ../cords.nix;

      # openvnp
      environment.etc."office.ovpn".source = ./office.ovpn;

      services = {
        openvpn.servers.officeVPN = {
          config = "config /etc/office.ovpn ";
          autoStart = false;
        };
        batteryNotifier.enable = true;
      };

      specialisation = {
        normalInternet.configuration = {
          networking.wg-quick.interfaces.vpn.autostart = false;
        };
      };
    })
    (lib.mkIf (!config.isRealSystem) {
      age = {
        identityPaths =
          [ "${./test-secrets/id_rsa}" ];

        secrets = {
          florian.file =
            ./test-secrets/passwordHash.age;
        };
      };

      location = {
        latitude = 20.0;
        longitude = 20.0;
      };
    })
    {

      zramSwap.enable = true;


      fonts.packages = with pkgs; [ terminus_font nerdfonts ];

      systemd.packages = [ pkgs.dconf ];

      home-manager = {
        users.florian = import ./home/configuration.nix ;
        extraSpecialArgs.private = private;
      };

      environment = { pathsToLink = [ "/share/agda" "/share/zsh" ]; };

      security.rtkit.enable = true;
      apparmor = {
        enable = true;
        systemPackages =
          [
            {
              pkgs = with pkgs; [
                xsane
                (writers.writeHaskellBin "scan" {
                  libraries = with haskellPackages; [ turtle extra ];
                } ../../haskell/scans/Scan.hs)
                csvkit
              ];
              rules = lib.apparmor.generateFileRules [ "docs" ];
            }
            {
              pkgs = with pkgs; [
                nixpkgs-review
                nixpkgs-hammering
                gnome3.adwaita-icon-theme
                ical2orgpy
                nixfmt
                libreoffice
                prismlauncher
                minetest
                textcleaner
                file
                element-desktop
                ### screenshots ###
                pinta
                ### screenshots ###
                inputs.nix-alien.packages."x86_64-linux".nix-alien
                languagetool
                remmina
                sqlite
                pamixer
                pavucontrol
                networkmanagerapplet
                mtpfs
                wget
                vim
                shellcheck
                ######## Games ###############
                airshipper
                superTuxKart
                openarena
                xonotic
                #############
                sshfs
                chromium
                passff-host
                neomutt
                mu
                poppler
                tuir
                (mpv.override { scripts = [ mpvScripts.mpris ]; })
                rlwrap
                you-get
                xosd
                pandoc
                mytexlive
                python3Packages.pygments
                anki
                nix-prefetch-git
                youtube-dl
                libnotify
                unzip
                rofi
                unclutter-xfixes
                cabal2nix
                niv
                git-crypt
                ### chat ###
                signal-desktop
                mumble
                ### chat ###
                fzf
                ## better rust tools ###
                procs
                delta
                du-dust
                grex
                ## better rust tools ###
                tor-browser-bundle-bin
                docker-compose
              ];
              rules = lib.apparmor.defaultRules;
            }
            {
              pkgs = with pkgs; [ psmisc bandwhich bottom ];
              rules = ''
                ptrace,
                unix (getattr),
                signal,
                ${lib.apparmor.defaultRules}
              '';
            }
            {
              # FIXME only allow usage of program. Not access.
              pkgs = with pkgs; [
                go-jira
                (pass.override {
                  dmenu = writeScriptBin "dmenu" ''${rofi}/bin/rofi -dmenu "$@"'';
                })
              ];

              rules = ''
                ${lib.apparmor.generateFileRules [ "pass" "gnupg" ]}
              '';
            }
            {
              pkgs = [ pkgs.rpi4-install ];
              rules = ''
                ${lib.apparmor.generateFileRules [ "pass" "gnupg" "ssh" ]}
              '';
            }
            {
              pkgs = [ pkgs.firefox ];
              rules = ''
                network,
                ${lib.apparmor.generateFileRules [ "firefox" ]}
              '';
            }
            {
              pkgs = [ inputs.agenix.packages.x86_64-linux.agenix ];
              rules = ''
                mount,
                capability,
                ${lib.apparmor.generateFileRules [ "ssh" ]}
              '';
            }
            {
              pkgs = [ inputs.deploy-rs.defaultPackage."x86_64-linux" pkgs.tmux-xpanes ];
              rules = ''
                ${lib.apparmor.generateFileRules [ "ssh" ]}
              '';
            }
            {
              pkgs = with pkgs; [
                imagemagick
                poppler_utils
                eza
                tesseract5
                zathura
                xournal
                feh
                litecli
                csvs-to-sqlite
              ];
              rules = ''
                ${lib.apparmor.generateFileRules [ "docs" ]}
              '';
            }
            {
              pkgs = with pkgs; [
                ripgrep
                bat
                fd
              ];
              rules = ''
                ${lib.apparmor.generateFileRules [ "docs" "notmuch" ]}
              '';
            }
            {
              pkgs = [ pkgs.steam ];
              rules = lib.apparmor.defaultRules;
            }
          ];
      };

      powerManagement.cpuFreqGovernor = lib.mkDefault "powersave";

      networking = {
        hostName = "nixosThinkpad";
        firewall.allowedTCPPorts = [
          24800
          8100 # checkpad sync server
        ];
      };

      users = {
        users = {
          florian = {
            hashedPasswordFile = config.age.secrets.florian.path;
            description = "Florian Engel";
            extraGroups =
              [ "adbusers" "wheel" "networkmanager" "docker" "scan" "lp" ];
          };
          playground = { isNormalUser = true; };
        };
      };

      wayland = {
        enable = true;
        autoLogin = true;
      };

      # powerManagement.enable = false;
      services = {
        tlp.enable = true;

        # Go in hibernate at lid
        logind = {
          lidSwitch = "hibernate";
          extraConfig = ''
        HandlePowerKey=hibernate
        RuntimeDirectorySize=30%
      '';
        };

        blueman.enable = true;

        hoogle.enable = true;
        syncthing = {
          enable = true;
          user = "florian";
          dataDir = "/home/florian/.syncthing";
          settings = {
            devices = {
              "android".id =
                "VWFGCVO-56ZMY6L-5N7MQ5F-GB4TJFS-AHAGT5L-WYN4WTS-TQJHEVN-NBBOOAS";
              "macos".id =
                "SEEGNGR-RV3PPXZ-AYPLTV3-VAEUOAO-IACRN32-Z4IEBCO-NECN453-FUF6OA3";
            };
            folders = {
              "android-photos" = {
                path = "/home/florian/android/photos";
                devices = [ "android" ];
              };
              "android-pictures" = {
                path = "/home/florian/android/pictures";
                devices = [ "android" ];
              };
              "android-org" = {
                path = "/home/florian/android/org";
                devices = [ "android" ];
              };
              "android-backups" = {
                path = "/home/florian/android/backups";
                devices = [ "android" ];
              };
              "org-roam" = {
                path = "/home/florian/Dokumente/org-roam";
                devices = [ "android" ];
                versioning = {
                  type = "simple";
                  params.keep = "5";
                };
              };
              "music" = {
                path = "/home/florian/Music";
                devices = [ "android" ];
              };
              "macos" = {
                path = "/home/florian/syncthing-macos";
                devices = [ "macos" ];
              };
            };
          };

        };
      };

      # Bluetooth sound
      systemd.user.services.telephony_client.enable = false;
      hardware = {
        bluetooth.enable = true;
        sane = {
          enable = true;
          extraBackends = with pkgs; [ epkowa sane-airscan hplipWithPlugin ];
          drivers.scanSnap = { enable = true; };
        };
      };

      # wifi
      networking.networkmanager = {
        enable = true;
        dispatcherScripts = [{
          source = let
            nmcli = "${pkgs.networkmanager}/bin/nmcli";
            lanInterface = "enp0s31f6";
          in pkgs.writeText "wlan_auto_toggle" ''
        if [ "$1" = "${lanInterface}" ]; then
            case "$2" in
                up)
                    ${nmcli} radio wifi off
                    ;;
                down)
                    ${nmcli} radio wifi on
                    ;;
                *)
                    if [ "$(${nmcli} -g GENERAL.STATE device show ${lanInterface})" = "20 (unavailable)" ]; then
                        echo "Lan is $2 (not up/down)"
                        echo "Enabeling wifi"
                        ${nmcli} radio wifi on
                    fi
                    ;;
            esac
        elif [ "$(${nmcli} -g GENERAL.STATE device show ${lanInterface})" = "20 (unavailable)" ]; then
            ${nmcli} radio wifi on
        fi
      '';
          type = "basic";
        }];
      };

      # big font for high resolution
      console.font = "sun12x22";

      services = {
        printing.enable = true;
        avahi = {
          enable = true;
          nssmdns = true;
          # for a WiFi printer
          openFirewall = true;
        };
      };

      programs = {
        light.enable = true;
        ssh.knownHosts.tim = {
          hostNames = [ private.serverIP ];
          publicKeyFile = ./backup.pub;
        };
        adb.enable = true;
        fuse.userAllowOther = true;
        gnupg.dirmngr.enable = true;
        nix-ld.enable = true;
        captive-browser = {
          enable = true;
          interface = "wlp61s0";
        };
      };

      nix.envVars.SSH_AUTH_SOCK = "/run/user/1000/gnupg/S.gpg-agent.ssh";

      systemd = {
        services = let
          targets = [
            "hibernate.target"
            "hybrid-sleep.target"
            "suspend.target"
            "sleep.target"
            "suspend-then-hibernate.target"
          ];
          serviceConfig = {
            Type = "oneshot";
            User = "florian";
            Group = "users";
          };
          environment = { DISPLAY = ":0"; };
        in {
          "my-post-resume" = {
            description = "Post-Resume Actions";
            after = targets;
            wantedBy = targets;
            script = ''
          if [ "$(${pkgs.networkmanager}/bin/nmcli -g GENERAL.STATE device show enp0s31f6)" = "20 (unavailable)" ]; then
            echo "Enabeling wifi"
            ${pkgs.networkmanager}/bin/nmcli radio wifi on
          fi
        '';
            inherit serviceConfig environment;
            enable = true;
          };
        };
      };

      programs.firejail.enable = true;
      nix = {
        buildMachines = [{
          hostName = "192.168.178.24";
          system = "x86_64-linux";
          sshUser = "root";
          maxJobs = 8;
          speedFactor = 2;
          supportedFeatures = [ "nixos-test" "benchmark" "big-parallel" "kvm" ];
          mandatoryFeatures = [ ];
        }];
        distributedBuilds = false;
      };

      virtualisation.docker.enable = true;

      nixpkgs.hostPlatform = "x86_64-linux";

    }
  ];
}
