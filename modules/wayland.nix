{ config, pkgs, lib, ... }:

with lib;

let
  cfg = config.wayland;

  windowChanger = pkgs.writeShellScript "choseWindow" ''
    ${pkgs.sway}/bin/swaymsg -t get_tree | ${pkgs.jq}/bin/jq -r '
            # descend to workspace or scratchpad
            .nodes[].nodes[]
            # save workspace name as .w
            | {"w": .name} + (
                    if .nodes then # workspace
                            [recurse(.nodes[])]
                    else # scratchpad
                            []
                    end
                    + .floating_nodes
                    | .[]
                    # select nodes with no children (windows)
                    | select(.nodes==[])
            )
            | ((.id | tostring) + "\t "
            # remove markup and index from workspace name, replace scratch with "[S]"
            + (.w | gsub("^[^:]*:|<[^>]*>"; "") | sub("__i3_scratch"; "[S]"))
            + "\t " +  .name)
            ' | ${pkgs.wofi}/bin/wofi --show dmenu --prompt='Focus a window' | {
        read -r id name
        ${pkgs.sway}/bin/swaymsg "[con_id=$id]" focus
    }
'';
in


{
  options.wayland = {
    enable = mkEnableOption (lib.mdDoc "Wether to use wayland");
    autoLogin = mkEnableOption (lib.mdDoc "Wether to autologin into wayland");
  };
  config = mkIf cfg.enable {
    xdg.portal.wlr.enable = true;
    home-manager.users.florian = {
      programs = {
        wofi.enable = true;
        swaylock.enable = true;
      };
      services.swayidle.enable = true;
      wayland.windowManager.sway = let modifier = "Mod4"; in {
        enable = true;
        config = {
          inherit modifier; 
          keybindings = {
            "${modifier}+Return" = "exec ${pkgs.alacritty}/bin/alacritty";
            "${modifier}+v" = "exec ${pkgs.emacs}/bin/emacsclient";
            "${modifier}+f" = "exec ${windowChanger}";
            "${modifier}+h" = "focus left";
            "${modifier}+j" = "focus up";
            "${modifier}+l" = "focus right";
          };
          terminal  = "${pkgs.alacritty}/bin/alacritty";
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
  };
}
