defaultShell:
  { config, lib, pkgs, ... }:

  let
    alacrittyCommon = {
      env.TERM = "xterm-256color";
      window.dynamic_title = true;
      shell.program = defaultShell;
      live_config_reload = true;
    };
    alacrittyConf =
      colors: builtins.toFile "alacritty.yml"
        (lib.replaceStrings [ "\\\\" ] [ "\\" ] (builtins.toJSON ({inherit colors;} // alacrittyCommon)));
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
    createDir = ''
      if [ ! -d ${alacrittyConfLoc} ]
      then
          mkdir ${alacrittyConfLoc}
      fi
   '';
  in {
    home-manager.users.florian = {
      home.file = {
        "bin/darkTheme" = {
          executable = true;
          text = ''
            #!${pkgs.runtimeShell}
            ${createDir}
            ln -snf ${alacrittyDark} ${alacrittyConfLoc}/alacritty.yml
          '';
        };
        "bin/lightTheme" = {
          executable = true;
          text = ''
            #!${pkgs.runtimeShell}
            ${createDir}
            ln -snf ${alacrittyLight} ${alacrittyConfLoc}/alacritty.yml
          '';
        };
      };
      programs.alacritty.enable = true;
    };
  }
