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
        apparmor.systemPackages =
          [ { pkgs = with pkgs; [
                wl-clipboard
                grim
                slurp
                adw-gtk3
                adwaita-qt
                adwaita-qt6
                wdisplays
              ];
              rules = lib.apparmor.defaultRules config;
            }
            { pkgs = [pkgs.wofi-pass];
              rules = ''
                ${lib.getExe pkgs.pass-wayland} Px,
                ${lib.apparmor.generateFileRules { allows = [ "pass" ]; inherit config; }}
              '';
            }
          ];

        hardware.graphics.enable = true;

        home-manager.users.florian = { config, osConfig, ... }: {

          programs = {
            wofi.enable = true;
            swaylock.enable = true;
          };
          services = {
            gammastep = {
              enable = true;
              longitude = osConfig.location.longitude;
              latitude = osConfig.location.latitude;
            };
            swayidle = {
              enable = true;
              timeouts = [
                { timeout = 900; command = "swaylock"; }
              ];
              events = [
                { event = "before-sleep"; command = "swaylock"; }
                { event = "lock"; command = "swaylock"; }
              ];
            };
            kanshi = {
              enable = true;
              settings = [
                {
                  profile = {
                    name = "default";
                    outputs = [{
                      criteria = "AU Optronics 0x2336 Unknown";
                      position = "0,0";
                      mode = "2560x1440@60.01";
                    }];
                  };
                }
                {
                  profile = {
                    name = "home";
                    outputs = [
                      {
                        criteria = "AU Optronics 0x2336 Unknown";
                        position = "0,1080";
                        mode = "2560x1440@60.01Hz";
                        scale = 1.5;
                      }
                      {
                        criteria = "BNQ BenQ GL2450H G6G05576019";
                        position = "0,0";
                        mode = "1920x1080@60.01Hz";
                      }
                    ];
                  };
                }
                { profile = {
                    name = "work";
                    outputs = [
                      {
                        criteria = "AU Optronics 0x2336 Unknown";
                        position = "1309,1440";
                        mode = "2560x1440@60.01Hz";
                        scale = 1.5;
                      }
                      {
                        criteria = "Dell Inc. DELL U2721DE FJD1C23";
                        position = "456,0";
                        mode = "2560x1440@59.951Hz";
                        scale = 1.0;
                      }
                    ];
                  };
                }
              ];
            };
          };

          wayland.windowManager.sway =
            let modifier = "Mod4";
                dbus = "dbus-send --print-reply ";
                dest = "--dest=org.mpris.MediaPlayer2.mpv ";
                org = "/org/mpris/MediaPlayer2 org.mpris.MediaPlayer2.Player.";
                openorraise = pkgs.rustPlatform.buildRustPackage {
                  name = "openorraise";
                  src = ../rust/openorraise;
                  cargoLock.lockFile = ../rust/openorraise/Cargo.lock;
                };
            in {
              enable = true;
              extraSessionCommands = ''
                export QT_WAYLAND_DISABLE_WINDOWDECORATION=1
                export XDG_SESSION_TYPE=wayland
                export NIXOS_OZONE_WL=1
              '';
              wrapperFeatures.gtk = true;
              systemd.enable = true;
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
                    "XF86AudioLowerVolume" = "exec 'pamixer -d 5'";
                    "XF86AudioMute" = "exec 'paxmixer -t'";
                    "${modifier}+p" = "exec '" + dbus + dest + org + "PlayPause'";
                    "${modifier}+i" = "exec '" + dbus + dest + org + "Next'";
                    "${modifier}+o" = "exec '" + dbus + dest + org + "Previous'";
                  })
                ];
                input."type:keyboard" = {
                  xkb_layout = "us";
                  xkb_options = "eurosign:e,caps:escape";
                  xkb_variant = "altgr-intl";
                };
                startup = [
                  { command = "${lib.getExe pkgs.sway-contrib.inactive-windows-transparency}"; }
                ];
                window = {
                  titlebar = false;
                  border = 0;
                  hideEdgeBorders = "both";
                };
                floating = {
                  titlebar = false;
                  border = 0;
                };
                terminal = "${osConfig.myUnprivilegedTerminal}";
                bars = [
                  {
                    mode = "hide";
                    statusCommand = "${pkgs.i3status}/bin/i3status";
                    fonts = {
                      names = [ "monospace" ];
                      size = 10.0;
                    };
                  }
                ];
              };
            };
          xdg.portal = {
            enable = true;
            xdgOpenUsePortal = true;
            extraPortals = [pkgs.xdg-desktop-portal-wlr pkgs.xdg-desktop-portal-gtk];
            config.common = {
              # use xdg-desktop-portal-gtk for every portal interface
              default="gtk";
              # except for the xdg-desktop-portal-wlr supplied interfaces
              # this are the only portals, that xdg-desktop-portal-wlr supports
              "org.freedesktop.impl.portal.Screencast"="wlr";
              "org.freedesktop.impl.portal.Screenshot"="wlr";
            };
          };
        };
        environment.pathsToLink = [ "/share/xdg-desktop-portal" "/share/applications" ];
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
