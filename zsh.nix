{ config, ... }:

{
  programs.zsh = {
    enable = true;
    enableAutosuggestions = true;
    oh-my-zsh = {
      enable = true;
      plugins = [ "vi-mode" "per-directory-history" "pass" ];
    };
    shellAliases = {
      ll = "ls -a";
      "." ="cd ..";
      ".." = "cd ../..";
      "..." = "cd ../../..";
      "...." = "cd ../../../..";
    };
    initExtra = ''
        export VISUAL='vim'
        export PATH=~/checkpad/checkpad-ssh:$PATH
      '';
  };
}
