{ config, pkgs, lib, ... }:

with lib;

let
  cfg = config.wayland;

in


{
  options.wayland = {
    enable = mkEnableOption (lib.mdDoc "Wether to use wayland");
    autoLogin = mkEnableOption (lib.mdDoc "Wether to autologin into wayland");
  };
  config = mkIf cfg.enable (
    mkMerge [
      {
        xdg.portal = {
          wlr.enable = true;
          configPackages = [ pkgs.xdg-desktop-portal-wlr ];
        };

        home-manager.users.florian = {
          programs = {
            wofi.enable = true;
            swaylock.enable = true;
          };
          services.swayidle.enable = true;
          wayland.windowManager.sway =
            let modifier = "Mod4";
                openorraise = pkgs.rustPlatform.buildRustPackage {
                  name = "openorraise";
                  src = lib.fileset.toSource {
                    root = ../.;
                    fileset = lib.fileset.unions [
                      ../Cargo.lock
                      ../Cargo.toml
                      ../rust
                    ];
                  };
                  cargoLock.lockFile = ../Cargo.lock;
                };
            in {
              enable = true;
              config = {
                inherit modifier; 
                keybindings = lib.mkOptionDefault {
                  "${modifier}+v" = "exec ${pkgs.emacs}/bin/emacsclient";
                  "${modifier}+d" = "exec ${openorraise}/bin/openorraise";
                };
                terminal = "${pkgs.alacritty}/bin/alacritty";
              };
            };
        };
        services.xserver = {
          displayManager = mkMerge [
            {
              gdm.enable = true;
            }
            (mkIf cfg.autoLogin {
              defaultSession = "sway";
              autoLogin = {
                enable = true;
                user = "florian";
              };
            })
          ];
        };
        security.pam.services.swaylock = {};
      }
      (mkIf (!config.isRealSystem) {
        environment.variables = {
          # Use a fixed SWAYSOCK path:
          "SWAYSOCK" = "/tmp/sway-ipc.sock";
          # We currently have to use the Pixman software
          # renderer since the GLES2 renderer doesn't work inside the VM (even
          # with WLR_RENDERER_ALLOW_SOFTWARE):
          # "WLR_RENDERER_ALLOW_SOFTWARE" = "1";
          "WLR_RENDERER" = "pixman";
        };
        virtualisation.qemu.options = [ "-vga none -device virtio-gpu-pci" ];
      })
    ]);
}
