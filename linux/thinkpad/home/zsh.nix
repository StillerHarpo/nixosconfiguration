{ pkgs, ... }: {
  imports = [ ../../../zsh.nix ];
  programs.zsh = {
    shellAliases.ls = "ls -lh --color=auto";
    oh-my-zsh.theme = "agnoster";
    initExtra = ''
      export BROWSER='${(import ../scripts pkgs).linkopen}/bin/linkopen'
      eval "$(direnv hook zsh)"
      precmd_in_nix_shell() {
        if echo "$PATH" | grep -qc '/nix/store'; then
          PROMPT='%K{green}%F{black} nix-shell %F{green}%K{black}$(echo "\ue0b0")%k%f%{%f%b%k%}$(build_prompt) '
        else
          PROMPT='%{%f%b%k%}$(build_prompt) '
        fi
      }
      add-zsh-hook precmd precmd_in_nix_shell
    '';
    localVariables.PROMPT = ''
      if echo "$PATH" | grep -qc '/nix/store'
      then
      fi
    '';
  };
}
