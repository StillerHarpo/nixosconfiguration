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

        programs.sway.enable = true;
        home-manager.users.florian = { config, ... }: {
          programs = {
            wofi.enable = true;
            swaylock.enable = true;
          };
          services.swayidle.enable = true;
          wayland.windowManager.sway =
            let modifier = "Mod4";
               dbus = "dbus-send --print-reply ";
               dest = "--dest=org.mpris.MediaPlayer2.mpv ";
               org = "/org/mpris/MediaPlayer2 org.mpris.MediaPlayer2.Player.";
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
                keybindings = lib.mkMerge [
                  (lib.mkIf config.services.emacs.enable (lib.mkOptionDefault {
                    "${modifier}+n" = "exec ${config.services.emacs.package}/bin/emacsclient -c";
                  }))
                  (lib.mkOptionDefault {
                    "${modifier}+d" = "exec ${openorraise}/bin/openorraise";
                    "XF86MonBrightnessDown" = "exec ${getExe pkgs.light} -U 10";
                    "XF86MonBrightnessUp" = "exec ${getExe pkgs.light} -A 10";
                    "XF86AudioRaiseVolume" = "exec 'pamixer -i 5'";
                    "XF86AudioLowerVolume" = "exec 'pamixer -t'";
                    "XF86AudioMute" = "exec 'pactl set-sink-mute @DEFAULT_SINK@ toggle'";
                    "${modifier}+p" = "exec '" + dbus + dest + org + "PlayPause'";
                    "${modifier}+i" = "exec '" + dbus + dest + org + "Next'";
                    "${modifier}+o" = "exec '" + dbus + dest + org + "Previous'";
                  })
                ];
                };
                terminal = "${pkgs.alacritty}/bin/alacritty";
              };
            };
        };
        security.pam.services.swaylock = {};
      }
      (mkIf config.wayland.autoLogin {
        services.getty.autologinUser = "florian";
        environment.loginShellInit = ''
          [[ "$(tty)" == /dev/tty1 ]] && sway
        '';
      })
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
      })
    ]);
}
