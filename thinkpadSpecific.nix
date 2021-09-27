# here are every configs that are used on my laptop but not on my workstation

{ config, pkgs, home-manager, sane-unstable, paperless-ng, agenix, pkgs-unstable, lib, ... }:
{

  disabledModules = [ "services/hardware/sane.nix" "services/misc/paperless.nix" ];

  imports = [
    ./hibernate.nix
    ./linuxSpecific.nix
    sane-unstable
    paperless-ng
  ];

  environment.systemPackages = with pkgs; [
    agenix.defaultPackage.x86_64-linux
    tigervnc
    pkgs-unstable.xsane
    (writers.writeHaskellBin
      "scan"
      { libraries = with haskellPackages; [turtle extra]; }
      ./scripts/Scan.hs)
  ];

  boot = {
    initrd = {
      # luks encryption
      luks.devices.luksroot.device = "/dev/disk/by-uuid/6d8ca465-1ff7-45a5-88d3-9aa0b4807cb7";
      availableKernelModules = [ "xhci_pci" "nvme" "usb_storage" "sd_mod" ];
    };
    kernelModules = [ "kvm-intel" ];
    extraModulePackages = [ ];
  };

  fileSystems = {
    "/" =
      { device = "/dev/disk/by-uuid/f97f5f90-314e-434a-8585-42694d5cf202";
        fsType = "ext4";
      };

    "/boot" =
      { device = "/dev/disk/by-uuid/C998-C706";
        fsType = "vfat";
      };
  };

  swapDevices =
    [ { device = "/dev/disk/by-uuid/f1429c26-3127-4932-8051-face01ca9ac8"; }
    ];

  nix.maxJobs = lib.mkDefault 8;
  powerManagement.cpuFreqGovernor = lib.mkDefault "powersave";

  networking = {
    hostName = "nixos-thinkpad";
    firewall.allowedTCPPorts = [ 24800 ];
  };

  age = {
    sshKeyPaths = [ "/root/.ssh/id_rsa" ];
    secrets = {
      paperless.file = ./secrets/paperless.age;
      birthdate.file = ./secrets/birthdate.age;
    };
  };

  # powerManagement.enable = false;
  services = {
    # Go in hibernate at lid
    logind = {
      lidSwitch = "hibernate";
      extraConfig = ''HandlePowerKey=hibernate
                      RuntimeDirectorySize=30%'';
    };
    # mouse pad
    xserver = {
      resolutions = [{x = "1920"; y = "1080";} {x = "2560"; y = 1440;}];
      synaptics = {
        enable = true;
        twoFingerScroll = true;
      };
    };

    blueman.enable = true;

    paperless-ng = {
      enable = true;
      passwordFile = config.age.secrets.paperless.path;
      consumptionDir = "/home/florian/Dokumente/scans";
      extraConfig =
        {
          PAPERLESS_OCR_LANGUAGE = "deu+eng";
          PAPERLESS_IGNORE_DATES = builtins.readFile config.age.secrets.birthdate.path;
        };
      consumptionDirIsPublic = true;
    };

    borgbackup.jobs."florian" = {
      paths = [  "/home/florian/Dokumente" "/home/florian/.password-store" ];
      repo = "borg@45.157.177.92:.";
      encryption = {
        mode = "repokey-blake2";
        passCommand = "cat /root/borgbackup/passphrase";
      };
      environment.BORG_RSH = "ssh -i /root/.ssh/id_rsa";
      compression = "auto,lzma";
      startAt = "weekly";
    };

  };

  # Bluetooth sound
  hardware = {
    bluetooth = {
      enable = true;
      # FIXME https://bbs.archlinux.org/viewtopic.php?id=267219&p=2 (A2DP not working before 5.60)
      package = pkgs-unstable.bluez;
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
