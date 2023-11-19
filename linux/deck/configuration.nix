{ config, lib, pkgs, inputs, outputs, private, ... }:

let sshKeys = import ../thinkpad/sshKeys.nix; in

{
  imports = [
    ./disko.nix
  ];

  age.secrets.florian.file = ./secrets/florian.age;

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

  sound.enable = true;

  hardware.bluetooth.enable = true;


  services = {
    blueman.enable = true;
    tailscale.enable = true;
    openssh = {
      enable = true;
      settings.PasswordAuthentication = false;
    };
    xserver = {
      desktopManager.plasma5 = {
        enable = true;
        runUsingSystemd = true;
      };
      libinput.enable = true;
      autoRepeatDelay = 250;
      autoRepeatInterval = 100;
      enable = true;
      layout = "us";
      xkbVariant = "altgr-intl";
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
      yuzu-ea
      ryujinx
      steam-rom-manager
      nvtop-amd
      lutris
      wine-staging
      lolcat
    ];
  };

  networking = {
    hostName = "flosDeck";
    interfaces.wlo1.useDHCP = true;
  };


  nix = {
    extraOptions = ''
      experimental-features = nix-command flakes
      keep-outputs = true
      keep-derivations = true
    '';
    optimise = {
      automatic = true;
      dates = [ "daily" ];
    };
    gc = {
      automatic = true;
      dates = "daily";
      options = "--delete-older-than 7d";
    };
    settings = {
      substituters = lib.mkAfter [
        "https://cache.ts.haering.dev?priority=39"
      ];
      trusted-public-keys = [
        "ssh-serve:WltAxNNiDoufj5yg7k9tJHKoN7D5PgIZUyqDOMBOaGM="
      ];
    };
  };

  boot.loader = {
    efi.canTouchEfiVariables = true;
    timeout = 5;
    systemd-boot = {
      editor = false;
      enable = true;
    };
  };

  users.users = {
    florian = {
      hashedPasswordFile = config.age.secrets.florian.path;
      description = "Florian Engel";
      extraGroups =
        [ "wheel" "networkmanager" ];
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
    };
  };

  networking.networkmanager.enable = true;

  nixpkgs = let system = "x86_64-linux"; in {
    hostPlatform.system = system;
    overlays = [ (_: _: { inherit system; }) ]
               ++ (builtins.attrValues outputs.overlays);
    config.allowUnfree = true;
  };

  system.stateVersion = "23.05";

}
