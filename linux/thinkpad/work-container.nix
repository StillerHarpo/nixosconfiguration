{ config, lib, pkgs, home-manager-flake, ... }:
let
    hostCfg = config;
    userName = "florian";
    userUid = hostCfg.users.users."${userName}".uid;
    xauth = "/tmp/work_xauth";
    outerPkgs = pkgs;
in {

  networking = {
    nat = {
      enable = true;
      internalInterfaces = ["ve-+"];
      externalInterface = "+";
    };
    networkmanager = {
      unmanaged = [ "interface-name:ve-*" ];
    };
  };

  services.xserver.displayManager.sessionCommands = ''
    xauth nextract - "$DISPLAY" | sed -e 's/^..../ffff/' | xauth -f "${xauth}" nmerge -
  '';

  containers.work = {
    # TODO and bind for file transfer
    bindMounts = {
      x11Display = rec {
        hostPath = "/tmp/.X11-unix";
        mountPoint = hostPath;
        isReadOnly = true;
      };
      xAuthority = rec {
        hostPath = xauth;
        mountPoint = hostPath;
        isReadOnly = true;
      };
    };

    enableTun = true;
    privateNetwork = true;
    hostAddress = "192.168.10.1";
    localAddress = "192.168.10.2";

    config = {
      imports = [ home-manager-flake ];
      users.users."${userName}" = {
        extraGroups = lib.mkForce [];
        isNormalUser = true;
        shell = pkgs.zsh;
      };

      systemd.services.fix-nix-dirs = let
        profileDir = "/nix/var/nix/profiles/per-user/${userName}";
        gcrootsDir = "/nix/var/nix/gcroots/per-user/${userName}";
      in {
        script = ''
          #!${pkgs.stdenv.shell}
          set -euo pipefail

          mkdir -p ${profileDir} ${gcrootsDir}
          chown ${userName}:root ${profileDir} ${gcrootsDir}
        '';
        wantedBy = [ "multi-user.target" ];
        serviceConfig = {
          Type = "oneshot";
        };
      };
      hardware.opengl = {
        enable = true;
        extraPackages = hostCfg.hardware.opengl.extraPackages;
      };

      environment.systemPackages = with pkgs; [
        git
        firefox
        remmina
      ];

      environment.etc."office.ovpn".source = ./office.ovpn;

      services.openvpn.servers.officeVPN = {
        config = '' config /etc/office.ovpn '';
        autoStart = true;
      };

#      networking.defaultGateway.address = "192.168.10.1";
      networking.nameservers = [ "8.8.8.8" ];

      home-manager = {
        users."${userName}" = {
          imports = [
            ../../zsh.nix
            home/firefox.nix
          ];
          programs.zsh = {
            enable = true;
            oh-my-zsh.theme = "agnoster";
            shellAliases = {
              ll = "ls -a";
              "." ="cd ..";
              ".." = "cd ../..";
              "..." = "cd ../../..";
              "...." = "cd ../../../..";
            };
            enableAutosuggestions = true;
            oh-my-zsh = {
              enable = true;
              plugins = [ "vi-mode" "per-directory-history" ];
            };
          };
          programs.ssh.enable = true;
          home.sessionVariables = {
            SHELL = "zsh";
            DISPLAY = ":0";
            XDG_RUNTIME_DIR = "/run/user/${toString userUid}";
            XAUTHORITY  = xauth;
          };
          home.stateVersion = "22.05";
          nixpkgs.overlays = outerPkgs.overlays;
        };
      };

      system.stateVersion = "22.05";
      nixpkgs.overlays = outerPkgs.overlays;
    };
  };
}
