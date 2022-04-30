# here are every configs that are used on my laptop but not on my workstation

{ config, pkgs, home-manager, sane-unstable, borgbackup-local
, defaultShell, pkgs-master, agenix, pkgs-unstable, lib, ... }:
{

  disabledModules = [
    "services/hardware/sane.nix"
    "services/backup/borgbackup.nix"
  ];

  imports = [
    ./hardware.nix
    ./hibernate.nix
    ../configuration.nix
    sane-unstable
    borgbackup-local
    (with (import ./apparmor.nix); generate [
      {
        pkgs = with pkgs; [
          pkgs-unstable.xsane
          (writers.writeHaskellBin
            "scan"
            { libraries = with haskellPackages; [turtle extra]; }
            ../../scripts/Scan.hs)
        ];
        profile = generateFileRules [];
      }
      {
        pkgs = with pkgs; [
          xclip
          remmina
          mullvad-vpn
          sqlite
          tesseract4
          poppler_utils
          gimp
          pamixer
          aria
          imagemagick
          pavucontrol
          arandr
          networkmanagerapplet
          mtpfs
          wget
          cachix
          vim
          shellcheck
          ##################### Games ###############
          multimc                 # minecraft launcher
          openjdk                 # java
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
          ag
          zathura
          spotify
          mpv
          rlwrap
          you-get
          xosd
          pandoc
          (texlive.combine {inherit (texlive) scheme-full pygmentex pgf collection-basic;})
          python37Packages.pygments
          bc
          feh
          anki
          nix-prefetch-git
          pkgs-master.youtube-dl
          libnotify
          unzip
          rofi
          wmctrl
          unclutter-xfixes
          psmisc
          cabal2nix
          pkgs-master.haskellPackages.implicit-hie
          pkgs-master.niv
          pkgs-unstable.stack
          #(haskellPackages.ghcWithPackages (self : with self;
          #  [ hlint hindent QuickCheck parsec megaparsec optparse-applicative
          #    adjunctions Agda ]))
          networkmanager_openvpn networkmanager_dmenu
          git-crypt
          slack
          nixopsUnstable
          tigervnc
        ];
        profile = defaultProfile;
      }
      {
        pkgs = [ pkgs.pass ];
        profile = ''
          ${generateFileRules ["pass"]}
          ${pkgs.gnupg}/* cix,
        '';
      }
      {
        pkgs = [ pkgs.firefox ];
        profile = ''
          network,
          ${generateFileRules ["firefox"]}
        '';
      }
      {
        pkgs = with pkgs; [
          agenix.defaultPackage.x86_64-linux
        ];
          profile = ''
            ${generateFileRules []}
            ${pkgs.openssh}/* cix,
          '';
      }
      {
        pkgs = [ pkgs-master.signal-desktop ];
        profile = ''
          capability,
          ${(import ./apparmor.nix).defaultProfile}
        '';
      }
    ])
    ];

  fonts.fonts = [ pkgs.terminus_font ];

  location = import ./cords.nix;


  systemd.packages = [ pkgs.dconf ];

  home-manager.users.florian = import ./home/configuration.nix defaultShell;

  environment = {
    pathsToLink = [ "/share/agda" "/share/zsh" ];
  };

  security = {
    apparmor.enable = true;
    rtkit.enable = true;
  };

  nix.maxJobs = lib.mkDefault 8;
  powerManagement.cpuFreqGovernor = lib.mkDefault "powersave";

  networking = {
    hostName = "nixos-thinkpad";
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
        extraGroups = [ "adbusers" "wheel" "networkmanager" "docker" "scan" "lp"];
      };
      playground = {
        isNormalUser = true;
      };
    };
  };

  # powerManagement.enable = false;
  services = {
    pipewire.media-session.config.bluez-monitor.properties.bluez5.msbc-support = true;

    unclutter-xfixes.enable = true;

    # Go in hibernate at lid
    logind = {
      lidSwitch = "hibernate";
      extraConfig = ''HandlePowerKey=hibernate
                      RuntimeDirectorySize=30%'';
    };
    # mouse pad
    xserver = {
      resolutions = [{x = "1920"; y = "1080";} {x = "2560"; y = 1440;}];
      windowManager.xmonad.extraPackages = haskellPackages:
        with haskellPackages; [MissingH protolude];
      synaptics = {
        enable = true;
        twoFingerScroll = true;
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
      opacityRules = [ "100:name = 'Dmenu'" "100:name = 'Rofi'" "100:class_g ?= 'Rofi'" "100:name = 'Notification'" ];
    };

    paperless-ng = {
      enable = true;
      passwordFile = config.age.secrets.paperless.path;
      consumptionDir = "/home/florian/Dokumente/paperlessInput";
      extraConfig =
        {
          PAPERLESS_OCR_LANGUAGE = "deu+eng";
          PAPERLESS_IGNORE_DATES = config.age.secrets.birthdate.path;
        };
      consumptionDirIsPublic = true;
    };

    borgbackup.jobs."florian" = {
      paths = [
        "/var/lib/paperless/media/documents/archive"
        "/home/florian/Dokumente"
        "/home/florian/.password-store"
        "/home/florian/Maildir"
        "/home/florian/android"
      ];
      repo = "borg@45.157.177.92:.";
      encryption = {
        mode = "repokey-blake2";
        passCommand = "cat /root/borgbackup/passphrase";
      };
      environment.BORG_RSH = "ssh -i /root/.ssh/id_rsa";
      compression = "auto,lzma";
      startAt = "weekly";
      restartOnFail.enable = true;
    };

    syncthing = {
      enable = true;
      user = "florian";
      dataDir = "/home/florian/.syncthing";
      devices."android".id = "VWFGCVO-56ZMY6L-5N7MQ5F-GB4TJFS-AHAGT5L-WYN4WTS-TQJHEVN-NBBOOAS";
      folders = {
        "android-photos" = {
          path = "/home/florian/android/photos";
          devices = [ "android" ];
        };
        "android-org" = {
          path = "/home/florian/android/org";
          devices = [ "android" ];
        };
      };

    };

  };

  # openvnp
  environment.etc."office.ovpn".source = ./office.ovpn;

  services.openvpn.servers.officeVPN = {
    config = '' config /etc/office.ovpn '';
    autoStart = false;
  };


  # Bluetooth sound
  hardware = {
    bluetooth = {
      enable = true;
      # FIXME https://bbs.archlinux.org/viewtopic.php?id=267219&p=2 (A2DP not working before 5.60)
      package = pkgs-unstable.bluez;
      hsphfpd.enable = true;
    };
    sane = {
      enable = true;
      extraBackends = with pkgs; [ epkowa sane-airscan hplipWithPlugin utsushi ];
      drivers.scanSnap = {
        enable = true;
       };
     };
  };

  # wifi
  networking.networkmanager.enable = true;
  # networking.wireless.enable = true;
  # boot.initrd.network.enable = true;
  # system.activationScripts.wpa_supplicant=
  #  "ln -sfn /etc/nixos/wpa_supplicant.conf /etc/wpa_supplicant.conf";

  # big font for high resolution
  console.font = "sun12x22";

  services = {
    # dhcpd4.extraConfig = ''
    #    option subnet-mask 255.255.255.0;
    #    option routers 10.0.0.100;
    #    subnet 10.0.0.0 netmask 255.255.255.0 {
    #      range 10.0.0.150 10.0.0.250;
    #    }
    # '';
    mullvad-vpn.enable = true;
    batteryNotifier.enable = true;
    synergy.server.enable = true;
    printing.enable = true;
    avahi = {
      enable = true;
      nssmdns = true;
    };
    udev.packages = [ pkgs.utsushi ];
  };

  programs = {
    light.enable = true;
    ssh.knownHosts.tim = {
      hostNames = [ "45.157.177.92" ];
      publicKeyFile = ./backup.pub;
    };
    slock.enable = true;
    adb.enable = true;
    fuse.userAllowOther = true;
    gnupg.dirmngr.enable = true;
  };

  systemd = {
    targets.my-post-resume = {
      description = "Post-Resume Actions";
      requires = [ "my-post-resume.service" ];
      after = [ "my-post-resume.service" ];
      wantedBy = [ "sleep.target" ];
      unitConfig.StopWhenUnneeded = true;
    };
    services = {
      # Service executed before suspending/hibernating.
      # FIXME not working
      "my-pre-sleep" = {
        description = "Pre-Sleep Actions";
        wantedBy = [ "sleep.target" ];
        before = [ "sleep.target" ];
        environment.DISPLAY = ":0";
        script =
          ''
            ${pkgs.pulseaudio}/bin/pactl set-sink-mute @DEFAULT_SINK@ true
          '';
        serviceConfig = {
          Type = "oneshot";
          User = "florian";
          Group = "users";
        };
        enable = true;
      };
      "my-post-resume-hibernate" = {
        description = "Post-Resume Actions";
        after = [ "hibernate.target" "hybrid-sleep.target" ];
        environment.DISPLAY = ":0";
        script =
          ''
            if ${pkgs.xorg.xrandr}/bin/xrandr -q | grep 'HDMI-2' | grep disconnected
            then
                ${pkgs.xorg.xrandr}/bin/xrandr --output HDMI-2 --off
                ${pkgs.xorg.xrandr}/bin/xrandr --output eDP-1 --mode 1920x1080
            elif ${pkgs.xorg.xrandr}/bin/xrandr -q | grep 'eDP-1' -A1 | tail -n1 | grep -v '\*' # screen off
            then
                ${pkgs.xorg.xrandr}/bin/xrandr --output eDP-1 --mode 1920x1080
                ${pkgs.xorg.xrandr}/bin/xrandr --output HDMI-2 --mode 1920x1080 --above eDP-1 # put eDP-1 below
            else
                ${pkgs.xorg.xrandr}/bin/xrandr --output eDP-1 --off
                ${pkgs.xorg.xrandr}/bin/xrandr --output HDMI-2 --mode 1920x1080
            fi
            ${pkgs.pulseaudio}/bin/pactl set-sink-mute @DEFAULT_SINK@ true
          '';
        serviceConfig = {
          Type = "oneshot";
          User = "florian";
          Group = "users";
        };
        enable = true;
      };
      "my-post-resume-suspend" = {
        description = "Post-Resume Actions";
        after = [ "suspend.target" ];
        environment.DISPLAY = ":0";
        script =
          ''
            ${pkgs.pulseaudio}/bin/pactl set-sink-mute @DEFAULT_SINK@ true
          '';
        serviceConfig = {
          Type = "oneshot";
          User = "florian";
          Group = "users";
        };
        enable = true;
      };
    };
  };
  nix = {
    buildMachines = [ {
      hostName = "10.42.0.151";
      system = "x86_64-linux";
      sshUser = "root";
      sshKey = "/home/florian/.ssh/nix_remote";
      maxJobs = 1;
      speedFactor = 2;
      supportedFeatures = [ "nixos-test" "benchmark" "big-parallel" "kvm" ];
      mandatoryFeatures = [ ];
    }] ;
    distributedBuilds = true;
  };
}
