# FIXME Use right font for mac
{ config, lib, pkgs, ... }:

with lib.converters;

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
  alacrittyConf = colors:
    pkgs.writeTextFile {
      name = "alacritty.yml";
      text =
        toAlacrittyJSON (lib.attrsets.recursiveUpdate colors alacrittyCommon);
    };
  darkBackground = "#282828";
  alacrittyDark = alacrittyConf {
    env.BAT_THEME = "gruvbox-dark";
    # Default colors
    colors = {
      primary = {
        # hard contrast: background = "#f9f5d7";
        background = darkBackground;
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
  };
  lightBackground = "#fbf1c7";
  alacrittyLight = alacrittyConf {
    env.BAT_THEME = "gruvbox-light";
    colors = {
      # Default colors
      primary = {
        # hard contrast: background = "#f9f5d7";
        background = lightBackground;
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
      };
    };
  };
  alacrittyConfLoc = "~/.config/alacritty";
  alacrittyConfPath = "${alacrittyConfLoc}/alacritty.yml";
  rofiCommon.window.fullscreen = true;
  rofiConf = theme:
    builtins.toFile "rofi.rasi" (toRofiRasi rofiCommon + ''

      @theme "${theme}"'');
  rofiDark = rofiConf "gruvbox-dark";
  rofiLight = rofiConf "gruvbox-light";
  gtk3Conf = theme:
    builtins.toFile "settings.ini" (toGtk3Ini {
      Settings = {
        gtk-icon-theme-name = theme;
        gtk-theme-name = theme;
      };
    });
  gtk3ConfDark = gtk3Conf "Adwaita-dark";
  gtk3ConfLight = gtk3Conf "Adwaita";
  gtk2Conf = theme:
    builtins.toFile ".gtkrc-2.0" ''
      gtk-icon-theme-name = "${theme}"
      gtk-theme-name = "${theme}"
    '';
  gtk2ConfDark = gtk2Conf "Adwaita-dark";
  gtk2ConfLight = gtk2Conf "Adwaita";
  gtk2ConfPath = "~/.gtkrc-2.0";
  dconf = theme:
    builtins.toFile "tc-${theme}.ini" (toDconfIni {
      "org/gnome/desktop/interface" = {
        gtk-theme = theme;
        icon-theme = theme;
      };
    });
  dconfDark = dconf "Adwaita-dark";
  dconfLight = dconf "Adwaita";
  rofiConfLoc = "~/.config/rofi";
  rofiConfPath = "${rofiConfLoc}/config.rasi";
  gtk3ConfLoc = "~/.config/gtk-3.0";
  gtk3ConfPath = "${gtk3ConfLoc}/settings.ini";
  xsettingsd = theme:
    builtins.toFile ".xsettingsd" ''Net/ThemeName "${theme}"'';
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
  blackWallpaper = pkgs.runCommand "dark.png" { }
    "${pkgs.imagemagick}/bin/convert -size 1920x1080 xc:${darkBackground} $out";
  whiteWallpaper = pkgs.runCommand "white.png" { }
    "${pkgs.imagemagick}/bin/convert -size 1920x1080 xc:${lightBackground} $out";
in {
  systemd.user = {
    timers.themeChange = {
      description = "Automatically change the theme";
      wantedBy = [ "timers.target" ];
      timerConfig = {
        Unit = "themeChange.service";
        OnCalendar = [ "*-*-* 20:00:00" "*-*-* 07:00:00" ];
        Persistent = true;
      };
    };
    services.themeChange = {
      description = "Automatically change the theme";
      partOf = [ "xsettingsd.service" ];
      before = [ "xsettingsd.service" ];
      script = ''
        TIME=$(date +%H%M)
        if [[ $TIME < 0630 || $TIME > 1930 ]]
        then
          ${pkgs.feh}/bin/feh --bg-scale ${blackWallpaper}
          ${prepareFiles}
          cp ${alacrittyDark} ${alacrittyConfPath}
          cp ${rofiDark} ${rofiConfPath}
          cp ${gtk2ConfDark} ${gtk2ConfPath}
          cp ${gtk3ConfDark} ${gtk3ConfPath}
          ${pkgs.dconf}/bin/dconf load / < ${dconfDark}
          ${pkgs.myemacs}/bin/emacsclient -e "(load-theme 'doom-gruvbox t)"
          cp ${xsettingsdDark} ${xsettingsdPath}
          echo ${darkBackground}> ~/.var/bgcolor
          echo BAT_THEME=gruvbox-dark > ~/.env
        else
          ${pkgs.feh}/bin/feh --bg-scale ${whiteWallpaper}
          ${prepareFiles}
          cp ${alacrittyLight} ${alacrittyConfPath}
          cp ${rofiLight} ${rofiConfPath}
          cp ${gtk2ConfLight} ${gtk2ConfPath}
          cp ${gtk3ConfLight} ${gtk3ConfPath}
          ${pkgs.dconf}/bin/dconf load / < ${dconfLight}
          ${pkgs.myemacs}/bin/emacsclient -e "(load-theme 'doom-gruvbox-light t)"
          cp ${xsettingsdLight} ${xsettingsdPath}
          echo ${lightBackground} > ~/.var/bgcolor
          echo BAT_THEME=gruvbox-light > ~/.env
        fi
      '';
    };
  };

  home-manager.users.florian.services.xsettingsd.enable = true;
}
