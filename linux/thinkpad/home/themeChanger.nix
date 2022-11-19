# FIXME Use right font for mac
{ config, lib, pkgs, ... }:

with import ../../../converters.nix {inherit config lib pkgs;};

let
  alacrittyCommon = {
    env.TERM = "xterm-256color";
    window.dynamic_title = true;
    shell.program = pkgs.myshell;
    live_config_reload = true;
    font = {
      normal = {
        family = "Iosevka Nerd Font Mono";
        style = "Regular";
      };

      bold = {
        family = "Iosevka Nerd Font Mono";
        style = "Bold";
      };

      italic = {
        family = "Iosevka Nerd Font Mono";
        style = "Italic";
      };

      bold_italic = {
        family = "Iosevka Nerd Font Mono";
        style = "Bold Italic";
      };

      size = 11;
    };
  };
  alacrittyConf =
    colors: pkgs.writeTextFile {
      name = "alacritty.yml";
      text =
        toAlacrittyJSON (lib.attrsets.recursiveUpdate colors alacrittyCommon);
    };
  alacrittyDark = alacrittyConf {
    env.BAT_THEME = "gruvbox-dark";
    # Default colors
    primary = {
      # hard contrast: background = "#f9f5d7";
      background = "#282828";
      # soft contrast: background = "#f2e5bc";
      foreground = "#ebdbb2";
    };
    # Normal colors
    normal = {
      black = "#282828";
      red = "#cc241d";
      green = "#98971a";
      yellow = "#d79921";
      blue = "#458588";
      magenta = "#b16286";
      cyan = "#689d6a";
      white = "#a89984";
    };
    # Bright colors
    bright = {
      black = "#928374";
      red = "#fb4934";
      green = "#b8bb26";
      yellow = "#fabd2f";
      blue = "#83a598";
      magenta = "#d3869b";
      cyan = "#8ec07c";
      white = "#ebdbb2";
    };
  };
  alacrittyLight = alacrittyConf {
    # Default colors
    primary = {
      # hard contrast: background = "#f9f5d7";
      background = "#fbf1c7";
      # soft contrast: background = "#f2e5bc";
      foreground = "#3c3836";
    };
    # Normal colors
    normal = {
      black = "#fbf1c7";
      red = "#cc241d";
      green = "#98971a";
      yellow = "#d79921";
      blue = "#458588";
      magenta = "#b16286";
      cyan = "#689d6a";
      white = "#7c6f64";
    };
    # Bright colors
    bright = {
      black = "#928374";
      red = "#9d0006";
      green = "#79740e";
      yellow = "#b57614";
      blue = "#076678";
      magenta = "#8f3f71";
      cyan = "#427b58";
      white = "#3c3836";
    env.BAT_THEME = "gruvbox-light";
    };
  };
  alacrittyConfLoc = "~/.config/alacritty";
  alacrittyConfPath = "${alacrittyConfLoc}/alacritty.yml";
  rofiCommon.window.fullscreen = true;
  rofiConf =
    theme: builtins.toFile "rofi.rasi"
      (toRofiRasi rofiCommon + "\n@theme \"${theme}\"");
  rofiDark = rofiConf "gruvbox-dark";
  rofiLight = rofiConf "gruvbox-light";
  gtk3Conf =
    theme: builtins.toFile "settings.ini"
      (toGtk3Ini { Settings = { gtk-icon-theme-name = theme; gtk-theme-name = theme; }; });
  gtk3ConfDark = gtk3Conf "Adwaita-dark";
  gtk3ConfLight = gtk3Conf "Adwaita";
  gtk2Conf =
    theme: builtins.toFile ".gtkrc-2.0"
      ''
      gtk-icon-theme-name = "${theme}"
      gtk-theme-name = "${theme}"
      '';
  gtk2ConfDark = gtk2Conf "Adwaita-dark";
  gtk2ConfLight = gtk2Conf "Adwaita";
  gtk2ConfPath = "~/.gtkrc-2.0";
  dconf =
    theme: builtins.toFile "tc-${theme}.ini"
      (toDconfIni {
        "org/gnome/desktop/interface" = {
          gtk-theme = theme;
          icon-theme = theme;
        };
      });
  dconfDark = dconf "Adwaita-dark";
  dconfLight = dconf "Adwaita";
  rofiConfLoc = "~/.config/rofi";
  rofiConfPath = "${rofiConfLoc}/config.rasi";
  gtk3ConfLoc  = "~/.config/gtk-3.0";
  gtk3ConfPath = "${gtk3ConfLoc}/settings.ini";
  xsettingsd =
    theme: builtins.toFile ".xsettingsd"
      ''Net/ThemeName "${theme}"'';
  xsettingsdDark = xsettingsd "Adwaita-dark";
  xsettingsdLight = xsettingsd "Adwaita";
  xsettingsdPath = "~/.xsettingsd";
  prepareFiles = ''
      if [ ! -d ${alacrittyConfLoc} ]
      then
          mkdir ${alacrittyConfLoc}
      fi
      # make it writable
      # (cp from nix store generates non writable file otherwise)
      touch ${alacrittyConfPath}
      if [ ! -d ${rofiConfLoc} ]
      then
          mkdir ${rofiConfLoc}
      fi
      touch ${rofiConfPath}
      if [ ! -d ${gtk3ConfLoc} ]
      then
          mkdir ${gtk3ConfLoc}
      fi
      touch ${gtk3ConfPath}
      touch ${gtk2ConfPath}
      touch ${xsettingsdPath}
   '';
  runXsettings = ''
      if [ $(pgrep xsettingsd) ]
      then
         killall -HUP xsettingsd
      else
         ${pkgs.xsettingsd}/bin/xsettingsd &
      fi
  '';
in {
  home.packages = with pkgs; with writers; [
    gnome3.adwaita-icon-theme
    (writeBashBin "darkTheme"
      ''
            ${feh}/bin/feh --bg-scale ~/scripts/var/black.png
            ${prepareFiles}
            cp ${alacrittyDark} ${alacrittyConfPath}
            cp ${rofiDark} ${rofiConfPath}
            cp ${gtk2ConfDark} ${gtk2ConfPath}
            cp ${gtk3ConfDark} ${gtk3ConfPath}
            ${pkgs.dconf}/bin/dconf load / < ${dconfDark}
            emacsclient -e "(load-theme 'doom-gruvbox t)"
            cp ${xsettingsdDark} ${xsettingsdPath}
            ${runXsettings}
        '')
    (writeBashBin "lightTheme"
      ''
            ${feh}/bin/feh --bg-scale ~/scripts/var/white.png
            ${prepareFiles}
            cp ${alacrittyLight} ${alacrittyConfPath}
            cp ${rofiLight} ${rofiConfPath}
            cp ${gtk2ConfLight} ${gtk2ConfPath}
            cp ${gtk3ConfLight} ${gtk3ConfPath}
            ${pkgs.dconf}/bin/dconf load / < ${dconfLight}
            emacsclient -e "(load-theme 'doom-gruvbox-light t)"
            cp ${xsettingsdLight} ${xsettingsdPath}
            ${runXsettings}
        '')
  ];
        echo BAT_THEME=gruvbox-dark > ~/.env
        echo BAT_THEME=gruvbox-light > ~/.env
}
