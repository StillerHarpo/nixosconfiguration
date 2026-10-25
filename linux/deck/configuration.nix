{
  config,
  lib,
  pkgs,
  inputs,
  outputs,
  inHydra ? false,
  ...
}:

let
  sshKeys = import ../thinkpad/sshKeys.nix;
in

{

  imports = [
    ./disko.nix
    ../configuration.nix
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
      desktopSession = "plasma";
    };
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
    desktopManager.plasma6.enable = true;
  };

  programs = {
    nix-ld.libraries = [ pkgs.pciutils ];
    steam.enable = true;
  };

  environment = {
    plasma6.excludePackages = with pkgs.kdePackages; [
      elisa
      khelpcenter
      oxygen
      plasma-browser-integration
    ];
    systemPackages = with pkgs; [
      librespeed-cli
      xonotic
      airshipper
      supertuxkart
      prismlauncher
      luanti
      librewolf
      vim
      git-crypt
      git
      # FIXME don't use master, when
      # https://github.com/NixOS/nixpkgs/pull/568501 is in unstable
      inputs.nixpkgs-master.legacyPackages.x86_64-linux.mumble
      pavucontrol
      arc-kde-theme # theme
      tailscale
      jovianPackages.steamdeck-firmware
      steam-rom-manager
      nvtopPackages.amd
      wine-staging
      (kodi.withPackages (
        kodiPkgs: with kodiPkgs; [
          mediathekview
          invidious
        ]
      ))
      libcec
      v4l-utils
      (jovianPackages.jupiter-dock-updater-bin.overrideAttrs (_: {
        version = "20231121.01";
        src = pkgs.fetchFromGitHub {
          owner = "Jovian-Experiments";
          repo = "jupiter-dock-updater-bin";
          rev = "jupiter-20231121.01";
          hash = "sha256-0ynous5tUVF+0IJIPR+DKMkQKwtMCkzYHCu4VzzuYyQ=";
        };
      }))
      sqlite
    ];
  };

  networking.hostName = "flosDeck";

  boot = {
    initrd = {
      availableKernelModules = [
        "evdev"
        "hid-generic"
        "i2c_hid_api"
        "hid_multitouch"
      ];
      unl0kr.enable = true;
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
      extraGroups = [
        "wheel"
        "networkmanager"
        "dialout"
        "video"
      ];
      isNormalUser = true;
      openssh.authorizedKeys.keys = sshKeys;
    };
    steam = {
      isNormalUser = true;
      extraGroups = [
        "networkmanager"
        "dialout"
        "video"
      ];
    };
    kodi = {
      isNormalUser = true;
      extraGroups = [
        "networkmanager"
        "dialout"
        "video"
      ];
    };
    root.openssh.authorizedKeys.keys = sshKeys;
  };

  home-manager = {
    useGlobalPkgs = true;
    useUserPackages = true;
    users.florian = {
      imports = [ outputs.homeModules ];
      librewolf.enable = true;
      home.stateVersion = "24.11";
      home.file.".kodi/userdata/addon_data/plugin.video.invidious/settings.xml".source = ./invidious.xml;
    };
  };

  networking.networkmanager = {
    enable = true;
    wifi.backend = "iwd";
  };

  nixpkgs = {
    hostPlatform.system = "x86_64-linux";
    config.allowUnfree = true;
  };

  nix.settings.trusted-users = [ "florian" ];

  dnscrypt.enable = true;

  system.stateVersion = "24.11";

  specialisation.buffy.configuration = {
    jovian.steam.enable = lib.mkForce false;
    # Don't know why this was enabled
    # Autologin was not working with out this
    # Don't know why
    systemd.services.display-manager.enable = lib.mkForce false;
    i18n.defaultLocale = "en_US.UTF-8";
    console = {
      earlySetup = true;
      font = "${pkgs.terminus_font}/share/consolefonts/ter-132n.psf.gz";
      packages = with pkgs; [ terminus_font ];
      keyMap = "us";
    };
    services = {
      getty.autologinUser = "florian";
      buffyboard = {
        enable = true;
        extraFlags = [ "--rotate=1" ];
      };
      xserver.enable = lib.mkForce false;
    };
    environment =
      let
        mediamenu = pkgs.rustPlatform.buildRustPackage {
          name = "mediamenu";
          src = ../../rust/mediamenu;
          buildInputs = [
            pkgs.openssl.dev
            pkgs.systemd.dev
            pkgs.SDL2.dev
          ];
          nativeBuildInputs = [ pkgs.pkg-config ];
          cargoLock = {
            lockFile = ../../rust/mediamenu/Cargo.lock;
            outputHashes = {
              "mediathekviewtui-0.1.0" = "sha256-NQOpEATcG69dnPx40yXUZY0PrCKdVTMhWV1evSpFJro=";
              "jellytui-0.1.5" = "sha256-x4Yb7QgnVLABCgl6ydnnMqGD7QGZzFQEJ5bnOWX6GzQ=";
            };
          };
          buildFeatures = if inHydra then [ ] else [ "real" ];
        };
      in
      {
        loginShellInit = ''
          [[ "$(tty)" == /dev/tty1 ]] && ${mediamenu}/bin/mediamenu
        '';
        systemPackages = with pkgs; [
          inputs.jellytui.defaultPackage.x86_64-linux
          mediamenu
          pamixer
        ];
      };
    age.secrets.jelltuiConfig = {
      file = ./secrets/jellyfin_config.toml.age;
      path = "/home/florian/.config/jellytui/config.toml";
      owner = "florian";
      group = "users";
      mode = "400";
    };
    age.secrets.jelltuiConfigT = {
      file = ./secrets/jellyfin_config_t.toml.age;
      path = "/home/florian/.config/jellytui-t/config.toml";
      owner = "florian";
      group = "users";
      mode = "400";
    };
    home-manager.users.florian.programs.mpv = {
      package = pkgs.mpv-unwrapped.override { sdl2Support = true; };
      enable = true;
      config = {
        input-gamepad = true;
        vf-add = "sub";
        video-rotate = 90;
      };
      profiles = {
        "screen.external" = {
          profile-cond = ''os.execute('mpv --drm-connector=help | grep " DP-1 (connected)"')'';
          video-rotate = 0;
          drm-connector = "DP-1";
        };
      };
      bindings = {
        "GAMEPAD_DPAD_UP" = "add volume 2";
        "GAMEPAD_DPAD_DOWN" = "add volume -2";
        "GAMEPAD_DPAD_RIGHT" = "seek +30";
        "GAMEPAD_DPAD_LEFT" = "seek -30";
        "GAMEPAD_ACTION_UP" = "cycle audio";
        "GAMEPAD_ACTION_DOWN" = "cycle pause";
        "GAMEPAD_ACTION_RIGHT" = "quit";
        "GAMEPAD_ACTION_LEFT" = "run ${lib.getExe pkgs.pamixer} --set-volume 100";
        "GAMEPAD_LEFT_TRIGGER" = "cycle sid";
        "GAMEPAD_RIGHT_TRIGGER" = "cycle sid down";
      };
      profiles.external-monitor = {
        video-rotate = 0;
      };
    };
  };
}
