{ config, lib, pkgs, inputs, outputs, private, ... }:

let sshKeys = import ../thinkpad/sshKeys.nix; in

{
  imports = [
    ./disko.nix
    ../configuration.nix
  ];

  age.secrets = {
    florian.file = ./secrets/florian.age;
    mediasourcesXml = {
      file = ./secrets/mediasources.xml.age;
      path = "/home/florian/.kodi/userdata/mediasources.xml";
      owner = "florian";
      group = "users";
      mode = "400";
    };
  };

  jovian = {
    steam = {
      enable = true;
      autoStart = true;
      user = "florian";
      desktopSession = "plasmawayland";
    };
    decky-loader.enable = true;
    devices.steamdeck = {
      enable = true;
      autoUpdate = true;
      enableGyroDsuService = true;
    };
  };


  # Select internationalisation properties.
  console.keyMap = "us";
  i18n.defaultLocale = "en_US.UTF-8";

  # Set your time zone.
  time.timeZone = "Europe/Berlin";

  hardware.bluetooth.enable = true;

  services = {
    blueman.enable = true;
    tailscale.enable = true;
    openssh = {
      enable = true;
      settings.PasswordAuthentication = false;
    };
    libinput.enable = true;
    xserver = {
      desktopManager.plasma5 = {
        enable = true;
        runUsingSystemd = true;
      };
      autoRepeatDelay = 250;
      autoRepeatInterval = 100;
      enable = true;
      xkb = {
        layout = "us";
        variant = "altgr-intl";
      };
      serverLayoutSection = ''
        Option  "BlankTime"   "0"
        Option  "OffTime"     "0"
        Option  "StandbyTime" "0"
        Option  "SuspendTime" "0"
      '';
    };
  };

  programs = {
    nix-ld.libraries = [ pkgs.pciutils ];
    steam.enable = true;
  };

  environment = {
    plasma5.excludePackages = with pkgs.plasma5Packages; [
      elisa
      khelpcenter
      oxygen
      plasma-browser-integration
    ];
    systemPackages = with pkgs; [
      librespeed-cli
      xonotic
      openarena
      airshipper
      superTuxKart
      prismlauncher
      minetest
      firefox
      vim
      git-crypt
      git
      mumble
      pavucontrol
      arc-kde-theme # theme
      arc-theme # theme
      tailscale
      steamdeck-firmware
      ryujinx
      steam-rom-manager
      nvtop-amd
      lutris
      wine-staging
      lolcat
      (kodi.withPackages (kodiPkgs:
        with kodiPkgs; [mediathekview invidious]
      ))
      libcec
      v4l-utils
      (jupiter-dock-updater-bin.overrideAttrs (_: {
        version = "20231121.01";
        src = pkgs.fetchFromGitHub {
          owner = "Jovian-Experiments";
          repo = "jupiter-dock-updater-bin";
          rev = "jupiter-20231121.01";
          hash = "sha256-0ynous5tUVF+0IJIPR+DKMkQKwtMCkzYHCu4VzzuYyQ=";
        };
      }))
    ];
  };

  networking = {
    hostName = "flosDeck";
    interfaces.wlo1.useDHCP = true;
  };

  boot = {
    initrd = {
      availableKernelModules = ["evdev" "hid-generic" "i2c_hid_api" "hid_multitouch"];
      unl0kr.enable = true;
      systemd.enable = true;
    };
    loader = {
      efi.canTouchEfiVariables = true;
      timeout = 5;
      systemd-boot = {
        editor = false;
        enable = true;
      };
    };
  };

  users.users = {
    florian = {
      hashedPasswordFile = config.age.secrets.florian.path;
      description = "Florian Engel";
      extraGroups =
        [ "wheel" "networkmanager" "dialout" "video" ];
      isNormalUser = true;
      openssh.authorizedKeys.keys = sshKeys;
    };
    root.openssh.authorizedKeys.keys = sshKeys;
  };


  home-manager = {
    useGlobalPkgs = true;
    useUserPackages = true;
    users.florian = {
      imports = [  outputs.homeModules ];
      firefox.enable = true;
      home.stateVersion = "23.05";
      home.file.".kodi/userdata/addon_data/plugin.video.invidious/settings.xml".source = ./invidious.xml;
    };
  };

  networking.networkmanager.enable = true;

  nixpkgs = {
    hostPlatform.system = "x86_64-linux";
    overlays = builtins.attrValues outputs.overlays;
    config.allowUnfree = true;
  };

  nix.settings.trusted-users = [ "florian" ];

  dnscrypt.enable = true;

  system.stateVersion = "23.05";

  # TODO How to just remove "fbcon=rotate:1" without getting infitite recursion
  # boot.kernelParams = lib.mkForce [ "fbcon=rotate:1" ] "${inputs.jovian}/config.boot.kernelParams";
  boot.kernelParams = lib.mkForce [ "spi_amd.speed_dev=1" "log_buf_len=4M" "amd_iommu=off" "amdgpu.lockup_timeout=5000,10000,10000,5000" "amdgpu.gttsize=8128" "amdgpu.sched_hw_submission=4" "audit=0" "fbcon=rotate:1" "loglevel=4" ];

  specialisation.kodi.configuration =
    let myKodi =
          (pkgs.kodi-gbm.withPackages (kodiPkgs:
            with kodiPkgs; [mediathekview invidious]
          ));
    in {
      jovian.steam.enable = lib.mkForce false;
      services.greetd = {
        enable = true;
        settings = {
          default_session= {
            command = "${pkgs.greetd.greetd}/bin/agreety";
            user = "florian";
          };
          initial_session = {
            command = "${myKodi}/bin/kodi --windowing=gbm";
            user = "florian";
          };
        };
      };
      users.users.florian.extraGroups = [ "input" ];
      services.xserver.enable = lib.mkForce false;
    };
}
