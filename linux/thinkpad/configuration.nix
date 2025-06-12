# here are every configs that are used on my laptop but not on my workstation

{ config, pkgs, lib, inputs, private, ... }:
let
  key = "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABgQCzyDziAfoNLt5Y6ZX/cVUtOFst8pZkeQOlCB9QNPkAJDreQwPGpoTpxSL3U8Z03a7WcWrlcYNMzNOfDQp15P1TwGDb49PVwnXHgmOafthDyIZMJ3Vvr4clMHYGOt2bz/tYuNtUF7X5+i6O2VVhRcn5jEmGBmOubxSwaHo+3LVpZN+95iSyiN9Pt2rsP4BvL9ZXcfUT/DrUt8YglX78BanoKPCyQ2E7HdEc/NoRdTgOhws1AAVOB4sQjEjD07oGY2WSHRyoMzu1ErVfgloB8AG0E74A7bln7Eets8R5022BktyBofD2v2v7NuAAorzlb6JqyesyNnFIreKXQ/I+jN1emCTBBcR3i7yoXQ00Zeaa55dOGKtQ9+RTN/Pmd3iRw/Wi3By+XYFPKm8OydV9cMdjGqkVwA3iR5yjcqI0dF7n8jOlw/RkjWkce9nzKCR4KCHz8AnWk/NFCUIKwHtVAeWVc+sG+jfrIrDKOFEYxdkDZP2zoSsgC1RtM92YoIIQW8s="; 
  mkFakeAgeSecret = path: pkgs.runCommand "secret.age" {} ''
      ${lib.getExe pkgs.rage} --recipient "${key}" -o $out < ${path}
    '';
in {
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
      dnscrypt.enable = true;
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
          thinkpadWireguardPrivate.file = ./secrets/thinkpadWireguardPrivate.age;
          vpnPrivateKey.file = ./secrets/vpnPrivateKey.age;
          vpnPresharedKey.file = ./secrets/vpnPresharedKey.age;
          officeOvpn.file = ./secrets/office.ovpn.age;
          bookmarks.file = ./secrets/my-bookmarks.json.age;
          notmuchTags.file = ./secrets/notmuchTags.age;
          syncthingKey.file = ./secrets/syncthingKey.age;
          syncthingCert.file = ./secrets/syncthingCert.age;
        };
      };
      myHardware.enable = true;
      backup.enable = true;
      wireguard = {
        rpi4.enable = false;
        vpn.enable = true;
      };

      location = private.cords;

      services = {
        tailscale.enable = true;
        openvpn.servers.officeVPN = {

          config = "config ${config.age.secrets.officeOvpn.path}";
          autoStart = false;
        };
        batteryNotifier.enable = true;

        syncthing = {
          enable = true;
          key = config.age.secrets.syncthingKey.path;
          cert = config.age.secrets.syncthingCert.path;
          user = "florian";
          dataDir = "/home/florian/.syncthing";
          settings = {
            devices = {
              "android".id =
                "VWFGCVO-56ZMY6L-5N7MQ5F-GB4TJFS-AHAGT5L-WYN4WTS-TQJHEVN-NBBOOAS";
              "macos".id =
                "SEEGNGR-RV3PPXZ-AYPLTV3-VAEUOAO-IACRN32-Z4IEBCO-NECN453-FUF6OA3";
              "pi".id = "D5UJHLR-NVAXNBN-7HD6XXP-7IOAKHA-6VT4B2Y-IBYPHKU-2KFFDPW-PUSX4QV";
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
                devices = [ "android" "pi" ];
              };
              "android-share" = {
                path = "/home/florian/android/share";
                devices = [ "android" ];
              };
              "org-roam" = {
                path = "/home/florian/Dokumente/org-roam";
                devices = [ "android" "pi" ];
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
    })
    (lib.mkIf (!config.isRealSystem) {
      location = {
        latitude = 20.0;
        longitude = 20.0;
      };

      age = {
        identityPaths = [ "${./test-secrets/id_rsa}" ];
        secrets = {
          florian.file = (mkFakeAgeSecret "${./test-secrets/florian}").outPath;
          bookmarks.file = (mkFakeAgeSecret "${./test-secrets/my-bookmarks.json}").outPath;
          notmuchTags.file = (mkFakeAgeSecret "${./test-secrets/notmuchTags}").outPath;
        };
      };
    })
    {
      age.secrets = {
        bookmarks = {
          path = "/home/florian/.my-bookmarks.json";
          owner = "florian";
          group = "users";
          mode = "400";
        };
        notmuchTags = {
          owner = "florian";
          group = "users";
          mode = "400";
        };
      };

      zramSwap.enable = true;

      fonts.packages = [
        pkgs.terminus_font
      ] ++ builtins.filter lib.attrsets.isDerivation (builtins.attrValues pkgs.nerd-fonts);

      systemd.packages = [ pkgs.dconf ];

      home-manager = {
        users.florian = import ./home/configuration.nix ;
        extraSpecialArgs.private = private;
      };

      environment = { pathsToLink = [ "/share/agda" "/share/zsh" ]; };

      security.rtkit.enable = true;
      environment.systemPackages  = [pkgs.sshfs];
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
                inetutils
                magic-wormhole
                mediathekview
                librespeed-cli
                mqttx
                nil
                difftastic
                nixpkgs-review
                nixpkgs-hammering
                adwaita-icon-theme
                ical2orgpy
                nixfmt-classic
                libreoffice
                textcleaner
                file
                element-desktop
                ### screenshots ###
                pinta
                ### screenshots ###
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
                # airshipper
                # superTuxKart
                # openarena
                # xonotic
                # minetest
                #############
                chromium
                # passff-host
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
                yt-dlp
                libnotify
                unzip
                cabal2nix
                git-crypt
                ### chat ###
                signal-desktop
                mumble
                slack
                ### chat ###
                fzf
                ## better rust tools ###
                procs
                delta
                du-dust
                grex
                ## better rust tools ###
                tor-browser-bundle-bin
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
              pkgs = [
                inputs.deploy-rs.packages.x86_64-linux.deploy-rs
                pkgs.tmux
                pkgs.tmux-xpanes
              ];
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
                xournalpp
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
        groups.fuse = {};
        users = {
          florian = {
            isNormalUser = true;
            hashedPasswordFile = config.age.secrets.florian.path;
            description = "Florian Engel";
            extraGroups =
              [ "audio" "video" "wireshark" "adbusers" "wheel" "networkmanager" "scan" "lp" "fuse" ];
          };
          playground = { isNormalUser = true; };
        };
      };

      wayland = {
        enable = true;
        autoLogin = true;
      };

      services.dbus.packages = [ pkgs.gcr pkgs.dconf ];

      # powerManagement.enable = false;
      services = {

        pipewire = {
          enable = true;
          alsa = {
            enable = true;
            support32Bit = true;
          };
          pulse.enable = true;
        };
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
      };

      # Bluetooth sound
      systemd.user.services.telephony_client.enable = false;
      hardware = {
        bluetooth.enable = true;
        sane = {
          enable = true;
          # 2025-05-25: epkowa build was failing. epkowa is only necessary for epson scanner.
          extraBackends = with pkgs; [ sane-airscan hplipWithPlugin ];
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
          nssmdns4 = true;
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
        wireshark = {
          enable = true;
          package = pkgs.wireshark;
        };
        steam = {
          enable = true;
          remotePlay.openFirewall = true;
        };
        gamemode.enable = true;
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

      nixpkgs.hostPlatform = "x86_64-linux";

      docker.enable = true;

      specialisation.normalInternet.configuration = {
        dnscrypt.enable = lib.mkForce false;
        docker.enable = lib.mkForce false;
      };

      nix = {
        settings.trusted-users = [ "florian" ];
        registry.nixpkgs.flake = inputs.nixpkgs;
        nixPath = [ "nixpkgs=${inputs.nixpkgs.outPath}" ];
      };

      # The NixOS release to be compatible with for stateful data such as databases.
      system.stateVersion = "24.11";

      fileSystems."/home/florian/server" = {
        device = "tim@server:/var/www/files.haering.dev";
        fsType = "fuse.sshfs";
        options = [
          "identityfile=/home/florian/.ssh/id_ed25519"
          "KexAlgorithms=curve25519-sha256"
          "idmap=user"
          "x-systemd.automount"
          "allow_other"
          "user"
          "uid=1000"
          "gid=1000"
        ];
      };
      programs.ssh.kexAlgorithms = [ "curve25519-sha256" ];
      boot.supportedFilesystems."fuse.sshfs" = true;
      youtube-tui.enable = true;
    }
  ];
}
