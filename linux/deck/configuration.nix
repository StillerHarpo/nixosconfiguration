{ config, lib, pkgs, inputs, outputs, private, ... }:

let sshKeys = import ../thinkpad/sshKeys.nix; in

{

  disabledModules = [ "system/boot/unl0kr.nix" ];

  imports = [
    ./disko.nix
    ../configuration.nix
    "${inputs.nixpkgs-unl0kr}/nixos/modules/system/boot/unl0kr.nix"
  ];

  age.secrets = {
    florian.file = ./secrets/florian.age;
    mediasourcesXml = {
      file = ./secrets/mediasources.xml.age;
      path = "/home/kodi/.kodi/userdata/mediasources.xml";
      owner = "kodi";
      group = "users";
      mode = "400";
    };
  };

  jovian = {
    steam = {
      enable = true;
      autoStart = true;
      user = "steam";
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
      nvtopPackages.amd
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
    steam = {
      isNormalUser = true;
      extraGroups =
        [  "networkmanager" "dialout" "video" ];
    };
    kodi = {
      isNormalUser = true;
      extraGroups =
        [  "networkmanager" "dialout" "video" ];
    };
    root.openssh.authorizedKeys.keys = sshKeys;
  };


  home-manager = {
    useGlobalPkgs = true;
    useUserPackages = true;
    users.florian = {
      imports = [  outputs.homeModules ];
      firefox.enable = true;
      home.stateVersion = "24.11";
      home.file.".kodi/userdata/addon_data/plugin.video.invidious/settings.xml".source = ./invidious.xml;
    };
  };

  networking.networkmanager.enable = true;

  nixpkgs = {
    hostPlatform.system = "x86_64-linux";
    overlays = builtins.attrValues outputs.overlays ++
               [(_: _: {
                 unl0kr = inputs.nixpkgs-unl0kr.legacyPackages."x86_64-linux".unl0kr;
               })];
    config.allowUnfree = true;
  };

  nix.settings.trusted-users = [ "florian" ];

  dnscrypt.enable = true;

  system.stateVersion = "24.11";

  specialisation.kodi.configuration =
    let myKodi =
          (pkgs.kodi.withPackages (kodiPkgs:
            with kodiPkgs; [
              mediathekview
              invidious
              jellyfin
              (buildKodiAddon rec {
                pname = "BluetoothManager";
                namespace = pname;
                version = "1.0.4";
                src = pkgs.fetchFromGitHub {
                  owner = "wastis";
                  repo = pname;
                  tag = "v${version}";
                  hash = "sha256-KKaR7rIkflMYU6EDBEcorHQ3t7jsB4Qe6Ikg+eBblkA=";
                };
              })
            ]
          ));
    in {
      jovian.steam.enable = lib.mkForce false;
      users.users = {
        kodi = {
          isNormalUser = true;
          extraGroups =
            [ "input"  "networkmanager" "dialout" "video" ];
        };
      };
      services.getty.autologinUser = "kodi";
      environment.loginShellInit = ''
        [[ "$(tty)" == /dev/tty1 ]] && sway
      '';
      home-manager.users.kodi = {
        home.stateVersion = "24.11";
        wayland.windowManager.sway = {
          enable = true;
          config = {
            input."type:touch" = {
              calibration_matrix = ''0 1 0 "-1" 0 1'';
            };
            startup = [
              { command = "${lib.getExe pkgs.pamixer} --set-volume 100"; }
              { command = "${lib.getExe' myKodi "kodi"}"; }
            ];
          };
        };
      };
      services.xserver.enable = lib.mkForce false;
    };
}
