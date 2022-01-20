# FIXME Use right font for mac
defaultShell:
  { config, lib, pkgs, ... }:

  with import ./converters.nix {inherit config lib pkgs;};

  let
    alacrittyCommon = {
      env.TERM = "xterm-256color";
      window.dynamic_title = true;
      shell.program = defaultShell;
      live_config_reload = true;
    };
    alacrittyConf =
      colors: builtins.toFile "alacritty.yml"
        (toAlacrittyJSON ({inherit colors;} // alacrittyCommon));
    alacrittyDark = alacrittyConf {
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
      };
    };
    alacrittyConfLoc = "~/.config/alacritty";
    alacrittyConfPath = "${alacrittyConfLoc}/alacritty.yml";
    rofiCommon.window.fullscreen = true;
    rofiConf =
      theme: builtins.toFile "rofi.rasi"
        (toRofiRasi ({configuration = {inherit theme;};} // rofiCommon));
    rofiDark = rofiConf "gruvbox-dark";
    rofiLight = rofiConf "gruvbox-light";
    rofiConfLoc = "~/.config/rofi";
    rofiConfPath = "${rofiConfLoc}/config.rasi";
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
   '';
  in {
    home.packages = with pkgs; with writers; [
      (writeBashBin "darkTheme"
        ''
            ${feh}/bin/feh --bg-scale ~/scripts/var/black.png
            ${prepareFiles}
            cp ${alacrittyDark} ${alacrittyConfPath}
            cp ${rofiDark} ${rofiConfPath}
            emacsclient -e "(load-theme 'doom-gruvbox t)"
        '')
      (writeBashBin "lightTheme"
        ''
            ${feh}/bin/feh --bg-scale ~/scripts/var/white.png
            ${prepareFiles}
            cp ${alacrittyLight} ${alacrittyConfPath}
            cp ${rofiLight} ${rofiConfPath}
            emacsclient -e "(load-theme 'doom-gruvbox-light t)"
        '')
    ];
  }
