# FIXME Use right font for mac
{
  config,
  lib,
  pkgs,
  ...
}:

with lib.converters;

let
  alacrittyCommon = {
    env.TERM = "xterm-256color";
    window.dynamic_title = true;
    terminal.shell.program = pkgs.myshell;
    general.live_config_reload = true;
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
    colors:
    (pkgs.formats.toml { }).generate "alacritty.toml" (
      lib.attrsets.recursiveUpdate colors alacrittyCommon
    );
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
  alacrittyConfPath = "${alacrittyConfLoc}/alacritty.toml";
  rofiCommon.window.fullscreen = true;
  rofiConf =
    theme:
    builtins.toFile "rofi.rasi" (
      toRofiRasi rofiCommon
      + ''

        @theme "${theme}"''
    );
  rofiDark = rofiConf "gruvbox-dark";
  rofiLight = rofiConf "gruvbox-light";
  gtk3Conf =
    theme:
    builtins.toFile "settings.ini" (toGtk3Ini {
      Settings = {
        gtk-icon-theme-name = theme;
        gtk-theme-name = theme;
      };
    });
  gtk3ConfDark = gtk3Conf "Adwaita-dark";
  gtk3ConfLight = gtk3Conf "Adwaita";
  gtk2Conf =
    theme:
    builtins.toFile "gtkrc-2.0" ''
      gtk-icon-theme-name = "${theme}"
      gtk-theme-name = "${theme}"
    '';
  gtk2ConfDark = gtk2Conf "Adwaita-dark";
  gtk2ConfLight = gtk2Conf "Adwaita";
  gtk2ConfPath = "~/.gtkrc-2.0";
  dconf =
    theme:
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
  gtk4ConfLoc = "~/.config/gtk-4.0";
  gtk4Css =
    theme:
    pkgs.writeTextFile {
      name = "gtk.css";
      text = ''
        /**
         * GTK 4 reads the theme configured by gtk-theme-name, but ignores it.
         * It does however respect user CSS, so import the theme from here.
        **/
        @import url("file://${pkgs.adw-gtk3}/share/themes/${theme}/gtk-4.0/gtk.css");
      '';
    };
  gtk4CssDark = gtk4Css "Adwaita-dark";
  gtk4CssLight = dconf "Adwaita";
  gtk3ConfPath = "${gtk3ConfLoc}/settings.ini";
  gtk4ConfPath = "${gtk4ConfLoc}/settings.ini";
  gtk4CssPath = "${gtk4ConfLoc}/gtk.css";
  xsettingsd = theme: builtins.toFile "xsettingsd" ''Net/ThemeName "${theme}"'';
  xsettingsdDark = xsettingsd "Adwaita-dark";
  xsettingsdLight = xsettingsd "Adwaita";
  xsettingsdPath = "~/.xsettingsd";
  prepareFiles = ''
    if [ ! -d ${alacrittyConfLoc} ]
    then
        ${pkgs.coreutils-full}/bin/mkdir ${alacrittyConfLoc}
    fi
    # make it writable
    # (cp from nix store generates non writable file otherwise)
    ${pkgs.coreutils-full}/bin/touch ${alacrittyConfPath}
    if [ ! -d ${rofiConfLoc} ]
    then
        ${pkgs.coreutils-full}/bin/mkdir ${rofiConfLoc}
    fi
    ${pkgs.coreutils-full}/bin/touch ${rofiConfPath}
    if [ ! -d ${gtk3ConfLoc} ]
    then
        ${pkgs.coreutils-full}/bin/mkdir ${gtk3ConfLoc}
    fi
    if [ ! -d ${gtk4ConfLoc} ]
    then
        ${pkgs.coreutils-full}/bin/mkdir ${gtk4ConfLoc}
    fi
    ${pkgs.coreutils-full}/bin/touch ${gtk3ConfPath}
    ${pkgs.coreutils-full}/bin/touch ${gtk4ConfPath}
    ${pkgs.coreutils-full}/bin/touch ${gtk4CssPath}
    ${pkgs.coreutils-full}/bin/touch ${gtk2ConfPath}
    ${pkgs.coreutils-full}/bin/touch ${xsettingsdPath}
  '';
  blackWallpaper =
    pkgs.runCommand "dark.png" { }
      "${pkgs.imagemagick}/bin/convert -size 1920x1080 xc:${darkBackground} $out";
  whiteWallpaper =
    pkgs.runCommand "white.png" { }
      "${pkgs.imagemagick}/bin/convert -size 1920x1080 xc:${lightBackground} $out";
in
{
  systemd.user = {
    timers.themeChange = {
      Install.WantedBy = [ "timers.target" ];
      Timer = {
        OnCalendar = [
          "*-*-* 20:00:00"
          "*-*-* 07:00:00"
        ];
        Persistent = true;
      };
    };
    services.themeChange = {
      Install.WantedBy = [ "emacs.service" ];
      Unit = {
        Description = "Automatically change the theme";
        After = [ "emacs.service" ];
      };

      Service.ExecStart = pkgs.writeShellScript "themeChange" ''
        export GIO_EXTRA_MODULES=${pkgs.dconf.lib}/lib/gio/modules
        export XDG_DATA_DIRS="${pkgs.gsettings-desktop-schemas}/share/gsettings-schemas/${pkgs.gsettings-desktop-schemas.name}:$XDG_DATA_DIRS"
        TIME=$(${pkgs.coreutils-full}/bin/date +%H%M)
        ${prepareFiles}
        if [[ $TIME < 0630 || $TIME > 1930 ]]
        then
          ${pkgs.feh}/bin/feh --bg-scale ${blackWallpaper}
          ${pkgs.coreutils-full}/bin/cp ${alacrittyDark} ${alacrittyConfPath}
          ${pkgs.coreutils-full}/bin/cp ${rofiDark} ${rofiConfPath}
          ${pkgs.coreutils-full}/bin/cp ${gtk2ConfDark} ${gtk2ConfPath}
          ${pkgs.coreutils-full}/bin/cp ${gtk3ConfDark} ${gtk3ConfPath}
          ${pkgs.coreutils-full}/bin/cp ${gtk3ConfDark} ${gtk4ConfPath}
          ${pkgs.coreutils-full}/bin/cp ${gtk4CssDark} ${gtk4ConfPath}
          ${pkgs.dconf}/bin/dconf load / < ${dconfDark}
          ${config.services.emacs.package}/bin/emacsclient -e "(load-theme 'doom-gruvbox t)"
          ${pkgs.coreutils-full}/bin/cp ${xsettingsdDark} ${xsettingsdPath}
          ${pkgs.coreutils-full}/bin/cp ${xsettingsdDark} ${xsettingsdPath}
          ${pkgs.glib}/bin/gsettings set org.gnome.desktop.interface gtk-theme "Adwaita-dark"
          ${pkgs.glib}/bin/gsettings set org.gnome.desktop.interface color-scheme 'prefer-dark'
          echo ${darkBackground}> ~/.var/bgcolor
          echo BAT_THEME=gruvbox-dark > ~/.env
        else
          ${pkgs.feh}/bin/feh --bg-scale ${whiteWallpaper}
          ${pkgs.coreutils-full}/bin/cp ${alacrittyLight} ${alacrittyConfPath}
          ${pkgs.coreutils-full}/bin/cp ${rofiLight} ${rofiConfPath}
          ${pkgs.coreutils-full}/bin/cp ${gtk2ConfLight} ${gtk2ConfPath}
          ${pkgs.coreutils-full}/bin/cp ${gtk3ConfLight} ${gtk3ConfPath}
          ${pkgs.coreutils-full}/bin/cp ${gtk3ConfLight} ${gtk4ConfPath}
          ${pkgs.coreutils-full}/bin/cp ${gtk4CssLight} ${gtk4CssPath}
          ${pkgs.dconf}/bin/dconf load / < ${dconfLight}
          ${config.services.emacs.package}/bin/emacsclient -e "(load-theme 'doom-gruvbox-light t)"
          ${pkgs.coreutils-full}/bin/cp ${xsettingsdLight} ${xsettingsdPath}
          ${pkgs.glib}/bin/gsettings set org.gnome.desktop.interface gtk-theme "Adwaita"
          ${pkgs.glib}/bin/gsettings set org.gnome.desktop.interface color-scheme 'prefer-light'
          echo ${lightBackground} > ~/.var/bgcolor
          echo BAT_THEME=gruvbox-light > ~/.env
        fi
        ${pkgs.systemd}/bin/systemctl --user restart xsettingsd
      '';
    };
  };

  services.xsettingsd.enable = true;

  xdg.systemDirs.data = [
    "${pkgs.gtk3}/share/gsettings-schemas/${pkgs.gtk3.name}"
    "${pkgs.gsettings-desktop-schemas}/share/gsettings-schemas/${pkgs.gsettings-desktop-schemas.name}"
  ];
}
