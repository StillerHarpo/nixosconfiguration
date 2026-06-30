{
  config,
  osConfig,
  pkgs,
  lib,
  ...
}:
{
  imports = [ ../../../zsh.nix ];
  programs.zsh = {
    shellAliases =
      let
        firejailWithBlacklist =
          "firejail " + (lib.concatMapStrings (x: " --blacklist=" + x)) (lib.protectFiles [ ] osConfig);
      in
      {
        ls = "ls -lh --color=auto";
        firejail-nix = "firejail --private=~/.private-home nix";
      };
    oh-my-zsh.theme = "agnoster";
    initContent =
      let
        linkopen =
          if osConfig.wayland.enable then pkgs.my-linkopen-wayland-or-tty else pkgs.my-linkopen-x-or-tty;
      in
      ''
        export BROWSER='${lib.getExe linkopen}'
        export CPM_ENABLE_TMUX=y
        eval "$(direnv hook zsh)"
        privileged() {
          if cat /proc/$PPID/cmdline | grep -qc Privileged; then
            PRIVILEGED="%K{red}%F{black}Privileged %F{red}%K{$1}$(echo '\ue0b0')%k%f%{%f%b%k%}"
          fi
        }
        precmd_in_nix_shell() {
          if echo "$PATH" | grep -qc '/nix/store'; then
            privileged "green"
            PROMPT='$PRIVILEGED%K{green}%F{black} nix-shell %F{green}%K{black}$(echo "\ue0b0")%k%f%{%f%b%k%}$(build_prompt) '
          else
            privileged "black"
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
}
