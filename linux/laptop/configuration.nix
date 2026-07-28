# here are every configs that are used on my laptop but not on my workstation

{
  config,
  pkgs,
  lib,
  inputs,
  private,
  ...
}:
let
  key = "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABgQCzyDziAfoNLt5Y6ZX/cVUtOFst8pZkeQOlCB9QNPkAJDreQwPGpoTpxSL3U8Z03a7WcWrlcYNMzNOfDQp15P1TwGDb49PVwnXHgmOafthDyIZMJ3Vvr4clMHYGOt2bz/tYuNtUF7X5+i6O2VVhRcn5jEmGBmOubxSwaHo+3LVpZN+95iSyiN9Pt2rsP4BvL9ZXcfUT/DrUt8YglX78BanoKPCyQ2E7HdEc/NoRdTgOhws1AAVOB4sQjEjD07oGY2WSHRyoMzu1ErVfgloB8AG0E74A7bln7Eets8R5022BktyBofD2v2v7NuAAorzlb6JqyesyNnFIreKXQ/I+jN1emCTBBcR3i7yoXQ00Zeaa55dOGKtQ9+RTN/Pmd3iRw/Wi3By+XYFPKm8OydV9cMdjGqkVwA3iR5yjcqI0dF7n8jOlw/RkjWkce9nzKCR4KCHz8AnWk/NFCUIKwHtVAeWVc+sG+jfrIrDKOFEYxdkDZP2zoSsgC1RtM92YoIIQW8s=";
  mkFakeAgeSecret =
    path:
    pkgs.runCommand "secret.age" { } ''
      ${lib.getExe pkgs.rage} --recipient "${key}" -o $out < ${path}
    '';
in
{
  imports = [
    ./wireguard.nix
    ./work-container.nix
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
      age.secrets = {
        florian.file = ./secrets/florian.age;
        thinkpadWireguardPrivate.file = ./secrets/thinkpadWireguardPrivate.age;
        vpnPrivateKey.file = ./secrets/vpnPrivateKey.age;
        vpnPresharedKey.file = ./secrets/vpnPresharedKey.age;
        vpnWorkPrivateKey.file = ./secrets/vpnWorkPrivateKey.age;
        vpnWorkPresharedKey.file = ./secrets/vpnWorkPresharedKey.age;
        officeOvpn.file = ./secrets/office.ovpn.age;
        bookmarks.file = ./secrets/my-bookmarks.json.age;
        notmuchTags.file = ./secrets/notmuchTags.age;
      };
      backup.enable = true;
      wireguard = {
        rpi4.enable = false;
        vpn.enable = true;
      };

      location = private.cords;

      syncthing.enable = true;

      services = {
        tailscale.enable = true;
        openvpn.servers.officeVPN = {

          config = "config ${config.age.secrets.officeOvpn.path}";
          autoStart = false;
        };
        batteryNotifier.enable = true;
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

      fileSystems."/".options = [
        "x-systemd.device-timeout=infinity"
      ];

      services.envfs.enable = true;

      mail.enable = true;

      zramSwap.enable = true;

      fonts.packages = [
        pkgs.terminus_font
      ]
      ++ builtins.filter lib.attrsets.isDerivation (builtins.attrValues pkgs.nerd-fonts);

      systemd.packages = [ pkgs.dconf ];

      home-manager = {
        users.florian = import ./home/configuration.nix;
        extraSpecialArgs.private = private;
      };

      environment = {
        pathsToLink = [
          "/share/agda"
          "/share/zsh"
        ];
      };

      security.rtkit.enable = true;
      environment.systemPackages = [ pkgs.sshfs ];
      apparmor = {
        enable = true;
        systemPackages = [
          {
            pkgs = with pkgs; [
              xsane
              (writers.writeHaskellBin "scan" {
                libraries = with haskellPackages; [
                  turtle
                  extra
                ];
              } ../../haskell/scans/Scan.hs)
              csvkit
            ];
            rules = lib.apparmor.generateFileRules {
              allows = [ "docs" ];
              inherit config;
            };
          }
          {
            pkgs = with pkgs; [
              ## comic reader
              yacreader
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
              nixfmt
              libreoffice
              textcleaner
              file
              ### screenshots ###
              pinta
              ### screenshots ###
              languagetool
              remmina
              sqlite
              pamixer
              pavucontrol
              mtpfs
              wget
              vim
              shellcheck
              tuir
              (mpv.override { scripts = [ mpvScripts.mpris ]; })
              rlwrap
              you-get
              xosd
              pandoc
              python3Packages.pygments
              anki
              nix-prefetch-git
              yt-dlp
              libnotify
              unzip
              cabal2nix
              git-crypt
              ### chat ###
              mumble
              ### chat ###
              fzf
              ## better rust tools ###
              procs
              delta
              dust
              grex
              ## better rust tools ###
              tor-browser
              # TODO use emacs module and remove this
              emacs-lsp-booster
              orca-slicer
              android-tools
            ];
            rules = lib.apparmor.defaultRules config;
          }
          # electron apps
          {
            pkgs = with pkgs; [
              chromium
              inputs.nixpkgs-unstable.legacyPackages."x86_64-linux".signal-desktop
              inputs.nixpkgs-unstable.legacyPackages."x86_64-linux".deltachat-desktop
              slack
              element-desktop
            ];
            rules = ''
              capability sys_admin,
              capability sys_chroot,
              capability sys_ptrace,
              ${lib.apparmor.defaultRules config}
            '';
          }
          {
            pkgs = with pkgs; [
              psmisc
              bandwhich
              bottom
            ];
            rules = ''
              ptrace,
              unix (getattr),
              signal,
              ${lib.apparmor.defaultRules config}
            '';
          }
          {
            pkgs = [ pkgs.pass-wayland ];
            rules = ''
              ${lib.getExe pkgs.gnupg} Cx -> gnupg,
              profile gnupg {
                ${lib.apparmor.generateFileRules {
                  allows = [
                    "gnupg"
                    "pass"
                  ];
                  inherit config;
                }}
              }
              ${lib.apparmor.generateFileRules {
                allows = [ "pass" ];
                inherit config;
              }}
            '';
          }
          {
            pkgs = [ pkgs.rpi4-install ];
            rules = ''
              ${lib.getExe pkgs.pass-wayland} Px,
              ${lib.getExe pkgs.openssh} Px,
              ${lib.apparmor.generateFileRules { inherit config; }}
            '';
          }
          {
            pkgs = [ pkgs.librewolf ];
            rules = ''
              network,
              capability sys_admin,
              capability sys_chroot,
              capability sys_ptrace,
              ${lib.apparmor.generateFileRules {
                allows = [ "librewolf" ];
                inherit config;
              }}
            '';
          }
          {
            pkgs = [ inputs.agenix.packages.x86_64-linux.agenix ];
            rules = ''
              mount,
              capability,
              # age reads ssh directly
              ${lib.apparmor.generateFileRules {
                allows = [ "ssh" ];
                inherit config;
              }}
            '';
          }
          {
            pkgs = [
              # Aug 10 10:31:46 nixosThinkpad audit[58390]: AVC apparmor="DENIED" operation="signal" class="signal" profile="/nix/store/08rwmwxz8z3kwalxiwigwnxys0x5arr5-deploy-rs-0.1.0/bin/*//openssh" pid=58390 comm="nix" requested_mask="receive" denied_mask="receive" signal=kill peer="/nix/store/08rwmwxz8z3kwalxiwigwnxys0x5arr5-deploy-rs-0.1.0/bin/*"
              pkgs.tmux
              pkgs.tmux-xpanes
            ];
            rules = ''
              # Allow unconfined processes to send us signals by default
              signal (receive) peer=unconfined,
              ${lib.getExe pkgs.openssh} Px,
              ${lib.apparmor.generateFileRules { inherit config; }}
            '';
          }
          {
            pkgs = [
              inputs.deploy-rs.packages.x86_64-linux.deploy-rs
            ];
            rules = ''
              ${lib.getExe pkgs.openssh} Cx -> sshRecive,
              ${lib.apparmor.generateFileRules { inherit config; }}
              signal (send) set=(kill) peer=${inputs.deploy-rs.packages.x86_64-linux.deploy-rs}/bin/**//sshRecive,
              profile sshRecive {
                ${lib.apparmor.generateFileRules {
                  allows = [ "ssh" ];
                  inherit config;
                }}
                signal (receive) set=(kill) peer=${inputs.deploy-rs.packages.x86_64-linux.deploy-rs}/bin/**,
                network,
                capability,
              }
            '';
          }
          {
            pkgs = with pkgs; [
              imagemagick
              poppler-utils
              eza
              tesseract5
              zathura
              xournalpp
              feh
              litecli
              csvs-to-sqlite
              mytexlive
            ];
            rules = ''
              ${lib.apparmor.generateFileRules {
                allows = [ "docs" ];
                inherit config;
              }}
            '';
          }
          {
            pkgs = with pkgs; [
              ripgrep
              bat
              fd
            ];
            rules = ''
              ${lib.apparmor.generateFileRules {
                allows = [
                  "docs"
                  "notmuch"
                ];
                inherit config;
              }}
            '';
          }
        ];
      };

      networking = {
        firewall.allowedTCPPorts = [
          24800
          8100 # checkpad sync server
        ];
      };

      users = {
        groups.fuse = { };
        users.florian = {
          isNormalUser = true;
          hashedPasswordFile = config.age.secrets.florian.path;
          description = "Florian Engel";
          extraGroups = [
            "audio"
            "video"
            "wireshark"
            "adbusers"
            "wheel"
            "networkmanager"
            "scan"
            "lp"
            "fuse"
          ];
        };
      };

      wayland = {
        enable = true;
        autoLogin = true;
      };

      services.dbus.packages = [
        pkgs.gcr
        pkgs.dconf
      ];

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

        # Go in hibernate at lid
        logind.settings.Login = {
          HandleLidSwitch = "hibernate";
          HandlePowerKey = "hibernate";
          RuntimeDirectorySize = "30%";
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
          extraBackends = with pkgs; [
            sane-airscan
            hplipWithPlugin
          ];
          drivers.scanSnap = {
            enable = true;
          };
        };
      };

      # wifi
      networkmanager.enable = true;

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
        ssh.knownHosts.tim = {
          hostNames = [ private.serverIP ];
          publicKeyFile = ./backup.pub;
        };
        fuse.userAllowOther = true;
        gnupg.dirmngr.enable = true;
        nix-ld.enable = true;
        wireshark = {
          enable = true;
          package = pkgs.wireshark;
        };
      };

      nix.envVars.SSH_AUTH_SOCK = "/run/user/1000/gnupg/S.gpg-agent.ssh";

      programs.firejail.enable = true;
      nix = {
        buildMachines = [
          {
            hostName = "192.168.178.24";
            system = "x86_64-linux";
            sshUser = "root";
            maxJobs = 8;
            speedFactor = 2;
            supportedFeatures = [
              "nixos-test"
              "benchmark"
              "big-parallel"
              "kvm"
            ];
            mandatoryFeatures = [ ];
          }
        ];
        distributedBuilds = false;
      };

      nixpkgs.hostPlatform = "x86_64-linux";

      specialisation.normalInternet.configuration = {
        dnscrypt.enable = lib.mkForce false;
        docker.enable = lib.mkForce false;
      };

      nix = {
        settings.trusted-users = [ "florian" ];
        registry.nixpkgs.flake = inputs.nixpkgs;
        nixPath = [ "nixpkgs=${inputs.nixpkgs.outPath}" ];
      };

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

      microvm.work.enable = true;

      # https://github.com/CachyOS/linux-cachyos/issues/878#issuecomment-4754587751
      security.lsm = [
        "capability"
        "landlock"
        "lockdown"
        "yama"
        "apparmor"
        "bpf"
      ];
    }
  ];
}
