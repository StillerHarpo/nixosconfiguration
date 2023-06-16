# here are every configs that are used on my laptop but not on my workstation

{ config, pkgs, home-manager, agenix, lib, ... }:

{

  zramSwap.enable = true;

  imports = [
    ./work-container.nix
    ./hardware.nix
    ./hibernate.nix
    ../../doom.nix
    ../configuration.nix
    ./themeChanger.nix
    ./backup.nix
    (with lib.apparmor;
      generate [
        {
          pkgs = with pkgs; [
            xsane
            (writers.writeHaskellBin "scan" {
              libraries = with haskellPackages; [ turtle extra ];
            } ../../haskell/scans/Scan.hs)
            csvkit
          ];
          profile = generateFileRules [ "docs" ];
        }
        {
          pkgs = with pkgs; [
            tmux
            #### Java ####
            maven
            eclipses.eclipse-java
            jetbrains.idea-ultimate
            openjdk
            ##############
            nixpkgs-review
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
            scrot
            pinta
            ### screenshots ###
            nix-alien
            languagetool
            xclip
            (kodi.withPackages (kodiPkgs:
              with kodiPkgs; [
                netflix
                steam-controller
                # FIXME Remove this when https://github.com/NixOS/nixpkgs/pull/215792 is in stable
                (kodiPkgs.invidious.overrideAttrs (_: {
                  src = fetchFromGitHub {
                    owner = "TheAssassin";
                    repo = "kodi-invidious-plugin";
                    rev = "85b66525632d94630c9301d9c490fc002a335d77";
                    hash =
                      "sha256-DpsAQUOUYCs3rpWwsk82+00KME4J+Iocu/v781dyyws=";
                  };
                }))
                arteplussept
              ]))
            remmina
            mullvad-vpn
            sqlite
            pamixer
            aria
            pavucontrol
            arandr
            networkmanagerapplet
            mtpfs
            wget
            cachix
            vim
            shellcheck
            ######## Games ###############
            airshipper
            superTuxKart
            openarena
            xonotic
            #############
            sshfs
            dzen2
            chromium
            w3m
            passff-host
            neomutt
            mu
            toxic
            poppler
            tuir
            xsel
            silver-searcher
            mpv
            rlwrap
            you-get
            xosd
            pandoc
            mytexlive
            python3Packages.pygments
            bc
            anki
            nix-prefetch-git
            youtube-dl
            libnotify
            unzip
            rofi
            wmctrl
            unclutter-xfixes
            cabal2nix
            niv
            networkmanager-openvpn
            networkmanager_dmenu
            git-crypt
            tigervnc
            ### chat ###
            slack
            signal-desktop
            mumble
            teamspeak_client
            ### chat ###
            xsecurelock
            fzf
            ## better rust tools
            procs
            delta
            du-dust
            grex
            kubectl
            k9s
          ];
          profile = defaultProfile;
        }
        {
          pkgs = with pkgs; [ psmisc bandwhich bottom ];
          profile = ''
            ptrace,
            unix (getattr),
            signal,
            ${defaultProfile}
          '';
        }
        {
          # FIXME only allow usage of program. Not access.
          pkgs = with pkgs; [ go-jira pass ];

          profile = ''
            ${generateFileRules [ "pass" "gnupg" ]}
          '';
        }
        {
          pkgs = [ pkgs.firefox ];
          profile = ''
            network,
            ${generateFileRules [ "firefox" ]}
          '';
        }
        {
          pkgs = [ agenix.packages.x86_64-linux.agenix ];
          profile = ''
            mount,
            capability,
            ${generateFileRules [ "ssh" ]}
          '';
        }
        {
          pkgs = [ pkgs.deploy-rs pkgs.tmux-xpanes ];
          profile = ''
            ${generateFileRules [ "ssh" ]}
          '';
        }
        {
          pkgs = with pkgs; [
            imagemagick
            ripgrep
            poppler_utils
            bat
            exa
            fd
            tesseract5
            zathura
            xournal
            feh
            litecli
            csvs-to-sqlite
          ];
          profile = ''
            ${generateFileRules [ "docs" ]}
          '';
        }
      ])
  ];

  boot = {
    # Use the systemd-boot EFI boot loader.
    loader = {
      systemd-boot.enable = true;
      efi.canTouchEfiVariables = true;
    };
    kernel.sysctl."kernel.yama.ptrace_scope" = 1;
  };

  fonts.fonts = with pkgs; [ terminus_font nerdfonts ];

  location = import ./cords.nix;

  systemd.packages = [ pkgs.dconf ];

  home-manager.users.florian = import ./home/configuration.nix;

  environment = { pathsToLink = [ "/share/agda" "/share/zsh" ]; };

  security = {
    apparmor = {
      enable = true;
      policies = with lib.apparmor; {
        steam.profile = getProfiles [ pkgs.steam ] defaultProfile;
        paperless.profile = getProfiles [ pkgs.paperless-ng ] ''
          network,
          ${generateFileRules [ "paperless" ]}
        '';
      };
    };
    rtkit.enable = true;
  };

  powerManagement.cpuFreqGovernor = lib.mkDefault "powersave";

  networking = {
    hostName = "nixosThinkpad";
    firewall.allowedTCPPorts = [ 24800 ];
  };

  age.secrets = {
    florian.file = ./secrets/florian.age;
    paperless.file = ./secrets/paperless.age;
    birthdate.file = ./secrets/birthdate.age;
  };

  users = {
    users = {
      florian = {
        passwordFile = config.age.secrets.florian.path;
        description = "Florian Engel";
        extraGroups =
          [ "adbusers" "wheel" "networkmanager" "docker" "scan" "lp" ];
      };
      playground = { isNormalUser = true; };
    };
  };

  # powerManagement.enable = false;
  services = {

    unclutter-xfixes.enable = true;

    tlp.enable = true;

    # Go in hibernate at lid
    logind = {
      lidSwitch = "hibernate";
      extraConfig = ''
        HandlePowerKey=hibernate
        RuntimeDirectorySize=30%
      '';
    };
    # mouse pad
    xserver = {
      resolutions = [
        {
          x = 2560;
          y = 1440;
        }
        {
          x = 1920;
          y = 1080;
        }
      ];
      windowManager.xmonad.extraPackages = haskellPackages:
        with haskellPackages; [
          MissingH
          protolude
        ];
      xautolock = {
        enable = true;
        locker = "${pkgs.xsecurelock}/bin/xsecurelock";
        killtime = 200;
        killer = "/run/current-system/systemd/bin/systemctl hibernate";
      };
    };

    blueman.enable = true;

    hoogle.enable = true;
    redshift = {
      enable = true;
      brightness = {
        day = "0.8";
        night = "0.7";
      };
      temperature.night = 1501;
    };
    picom = {
      enable = true;
      inactiveOpacity = 0.8;
      opacityRules = [
        "100:name = 'Dmenu'"
        "100:name = 'Rofi'"
        "100:class_g ?= 'Rofi'"
        "100:name = 'Notification'"
      ];
    };

    paperless = {
      enable = true;
      passwordFile = config.age.secrets.paperless.path;
      consumptionDir = "/home/florian/paperlessInput";
      extraConfig = {
        PAPERLESS_OCR_LANGUAGE = "deu+eng";
        PAPERLESS_IGNORE_DATES = config.age.secrets.birthdate.path;
      };
      consumptionDirIsPublic = true;
    };

    syncthing = {
      enable = true;
      user = "florian";
      dataDir = "/home/florian/.syncthing";
      devices."android".id =
        "VWFGCVO-56ZMY6L-5N7MQ5F-GB4TJFS-AHAGT5L-WYN4WTS-TQJHEVN-NBBOOAS";
      devices."macos".id =
        "SEEGNGR-RV3PPXZ-AYPLTV3-VAEUOAO-IACRN32-Z4IEBCO-NECN453-FUF6OA3";
      folders = {
        "android-photos" = {
          path = "/home/florian/android/photos";
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
        "macos" = {
          path = "/home/florian/syncthing-macos";
          devices = [ "macos" ];
        };

      };

    };
  };

  # openvnp
  environment.etc."office.ovpn".source = ./office.ovpn;

  services.openvpn.servers.officeVPN = {
    config = "config /etc/office.ovpn ";
    autoStart = false;
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
    mullvad-vpn.enable = true;
    batteryNotifier.enable = true;
    printing.enable = true;
    avahi = {
      enable = true;
      nssmdns = true;
      # for a WiFi printer
      openFirewall = true;
    };
    autorandr = {
      enable = true;
      profiles = {
        default = {
          fingerprint.eDP-1 =
            "00ffffffffffff0006af362300000000001b0104a51f117802f4f5a4544d9c270f505400000001010101010101010101010101010101e65f00a0a0a040503020350035ae100000180000000f0000000000000000000000000020000000fe0041554f0a202020202020202020000000fe004231343051414e30322e33200a00b2";
          config = {
            eDP-1 = {
              crtc = 0;
              mode = "2560x1440";
              position = "0x0";
              rate = "60.01";
            };
            DP-1.enable = false;
            HDMI-1.enable = false;
            DP-2.enable = false;
            HDMI-2.enable = false;
            DP-2-1.enable = false;
            DP-2-2.enable = false;
            DP-2-3.enable = false;
          };
        };
        home = {
          fingerprint = {
            eDP-1 =
              "00ffffffffffff0006af362300000000001b0104a51f117802f4f5a4544d9c270f505400000001010101010101010101010101010101e65f00a0a0a040503020350035ae100000180000000f0000000000000000000000000020000000fe0041554f0a202020202020202020000000fe004231343051414e30322e33200a00b2";
            HDMI-2 =
              "00ffffffffffff0009d1a77845540000181a010380351e782eba45a159559d280d5054a56b80810081c08180a9c0b300d1c001010101023a801871382d40582c4500132a2100001e000000ff0047364730353537363031390a20000000fd00324c1e5311000a202020202020000000fc0042656e5120474c32343530480a0146020322f14f90050403020111121314060715161f2309070765030c00100083010000023a801871382d40582c4500132a2100001f011d8018711c1620582c2500132a2100009f011d007251d01e206e285500132a2100001e8c0ad08a20e02d10103e9600132a21000018000000000000000000000000000000000000000000eb";
          };
          config = {
            eDP-1 = {
              crtc = 1;
              mode = "2560x1440";
              position = "0x1080";
              rate = "60.01";
            };
            HDMI-1.enable = false;
            HDMI-2 = {
              primary = true;
              crtc = 0;
              mode = "1920x1080";
              position = "0x0";
              rate = "60.00";
            };
            DP-2.enable = false;
            DP-2-1.enable = false;
            DP-2-2.enable = false;
            DP-2-3.enable = false;
          };
        };
        "work" = {
          fingerprint = {
            eDP1 =
              "00ffffffffffff0006af362300000000001b0104a51f117802f4f5a4544d9c270f505400000001010101010101010101010101010101e65f00a0a0a040503020350035ae100000180000000f0000000000000000000000000020000000fe0041554f0a202020202020202020000000fe004231343051414e30322e33200a00b2";
            DP-2-1 =
              "00ffffffffffff0010ac80405333323732170104a53c22783a4bb5a7564ba3250a5054a54b008100b300d100714fa940818001010101565e00a0a0a029503020350055502100001a000000ff00374a4e5935334342373233530a000000fc0044454c4c205532373133484d0a000000fd0031561d711e010a20202020202001be02031df15090050403020716010611121513141f2023097f0783010000023a801871382d40582c250055502100001e011d8018711c1620582c250055502100009e011d007251d01e206e28550055502100001e8c0ad08a20e02d10103e960055502100001800000000000000000000000000000000000000000000000000005d";
          };
          config = {
            eDP-1 = {
              crtc = 0;
              mode = "2560x1440";
              position = "0x1440";
              rate = "60.01";
            };
            HDMI-1.enable = false;
            HDMI-2.enable = false;
            DP-1.enable = false;
            DP-2.enable = false;
            DP-2-1 = {
              primary = true;
              crtc = 1;
              mode = "2560x1440";
              position = "0x0";
              rate = "59.9";
            };
            DP-2-2.enable = false;
            DP-2-3.enable = false;
          };

        };
        "work2" = {
          fingerprint = {
            eDP1 =
              "00ffffffffffff0006af362300000000001b0104a51f117802f4f5a4544d9c270f505400000001010101010101010101010101010101e65f00a0a0a040503020350035ae100000180000000f0000000000000000000000000020000000fe0041554f0a202020202020202020000000fe004231343051414e30322e33200a00b2";
            DP-1-1 =
              "00ffffffffffff0010ac80405333323732170104a53c22783a4bb5a7564ba3250a5054a54b008100b300d100714fa940818001010101565e00a0a0a029503020350055502100001a000000ff00374a4e5935334342373233530a000000fc0044454c4c205532373133484d0a000000fd0031561d711e010a20202020202001be02031df15090050403020716010611121513141f2023097f0783010000023a801871382d40582c250055502100001e011d8018711c1620582c250055502100009e011d007251d01e206e28550055502100001e8c0ad08a20e02d10103e960055502100001800000000000000000000000000000000000000000000000000005d";
          };
          config = {
            eDP-1 = {
              crtc = 0;
              mode = "2560x1440";
              position = "0x1440";
              rate = "60.01";
            };
            HDMI-1.enable = false;
            HDMI-2.enable = false;
            DP-1.enable = false;
            DP-2.enable = false;
            DP-1-1 = {
              primary = true;
              crtc = 1;
              mode = "2560x1440";
              position = "0x0";
              rate = "59.9";
            };
            DP-2-2.enable = false;
            DP-2-3.enable = false;
          };
        };
      };

    };
  };

  programs = {
    light.enable = true;
    ssh.knownHosts.tim = {
      hostNames = [ "45.157.177.92" ];
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

}
