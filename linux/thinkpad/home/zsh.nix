{ config, pkgs, lib, ... }: {
  imports = [ ../../../zsh.nix ];
  programs.zsh = {
    shellAliases = let
      firejailWithBlacklist = "firejail "
        + (lib.concatMapStrings (x: " --blacklist=" + x))
        (lib.protectFiles [ ]);
    in {
      ls = "ls -lh --color=auto";
      firejail-nix = "firejail --private=~/.private-home nix";
    };
    oh-my-zsh.theme = "agnoster";
    initExtra = ''
      export BROWSER='${pkgs.my-linkopen}/bin/linkopen'
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
  home.file = {
    ".private-home/.zshrc".source = config.home.file.".zshrc".source;
    ".private-home/.zshenv".source = config.home.file.".zshenv".source;
    ".private/oh-my-zsh/.keep".text = "";
  };
}
