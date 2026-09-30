{
  config,
  lib,
  osConfig,
  pkgs,
  ...
}:
let
  cfg = config.zsh;
in
{
  options.zsh = {
    enable = lib.mkEnableOption (lib.mdDoc "Use zsh");
    enableMeqoSsh = lib.mkOption {
      default = false;
      description = lib.mdDoc "use meqo ssh";
    };
    extraPlugins = lib.mkOption {
      default = [ ];
      type = lib.types.listOf lib.types.str;
      description = lib.mdDoc "extra zsh plugins";
    };
  };
  config = lib.mkIf (cfg.enable == true) {
    programs.zsh = {
      enable = true;
      autosuggestion.enable = true;
      oh-my-zsh = {
        enable = true;
        plugins = [
          "vi-mode"
          "per-directory-history"
          "pass"
        ]
        ++ cfg.extraPlugins;
        theme = "agnoster";
      };
      shellAliases = lib.mkMerge [
        {
          ls = "ls -lh --color=auto";
          "." = "cd ..";
          ".." = "cd ../..";
          "..." = "cd ../../..";
          "...." = "cd ../../../..";
        }
        (lib.mkIf osConfig.programs.firejail.enable {
          firejail-nix = "firejail --private=~/.private-home nix";
        })
      ];
      sessionVariables = lib.mkMerge [
        (lib.mkIf osConfig.wayland.enable {
          BROWSER = lib.getExe pkgs.my-linkopen-wayland-or-tty;
        })
        (lib.mkIf osConfig.xserver.enable {
          BROWSER = lib.getExe pkgs.my-linkopen-x-or-tty;
        })
        { SHELL = pkgs.myshell; }
      ];
      initContent = ''
        precmd() {
          if [[ -a .env ]]; then
            . ~/.env
          fi
        }
        ${lib.optionalString cfg.enableMeqoSsh "export PATH=~/meqo/ssh:$PATH"}
        ${lib.optionalString config.emacs.enable ''
          vterm_printf(){
              if [ -n "$TMUX" ] && ([ "''${TERM%%-*}" = "tmux" ] || [ "''${TERM%%-*}" = "screen" ] ); then
                  # Tell tmux to pass the escape sequences through
                  printf "\ePtmux;\e\e]%s\007\e\\" "$1"
              elif [ "$''${TERM%%-*}" = "screen" ]; then
                  # GNU screen (screen, screen-256color, screen-256color-bce)
                  printf "\eP\e]%s\007\e\\" "$1"
              else
                  printf "\e]%s\e\\" "$1"
              fi
          }
        ''}
        ${lib.optionalString config.programs.direnv.enable ''
          eval "$(direnv hook zsh)"
        ''}
        ${lib.optionalString osConfig.apparmor.enable ''
          privileged() {
            if cat /proc/$PPID/cmdline | grep -qc Privileged; then
              PRIVILEGED="%K{red}%F{black}Privileged %F{red}%K{$1}$(echo '\ue0b0')%k%f%{%f%b%k%}"
            fi
          }
        ''}
        precmd_in_nix_shell() {
          if echo "$PATH" | grep -qc '/nix/store'; then
            ${lib.optionalString osConfig.apparmor.enable ''
              privileged "green"
            ''}
            PROMPT='$PRIVILEGED%K{green}%F{black} nix-shell %F{green}%K{black}$(echo "\ue0b0")%k%f%{%f%b%k%}$(build_prompt) '
          else
            ${lib.optionalString osConfig.apparmor.enable ''
              privileged "black"
            ''}
            PROMPT='$PRIVILEGED%{%f%b%k%}$(build_prompt) '
          fi
        }
        add-zsh-hook precmd precmd_in_nix_shell
      '';
      localVariables = {
        PROMPT = ''
          if echo "$PATH" | grep -qc '/nix/store'
          then
          fi
        '';
        CPM_COMPILE_CACHE_SPEC = "local:/tmp/";
      };
    };
    home.file.".private/oh-my-zsh/.keep".text = "";
  };
}
