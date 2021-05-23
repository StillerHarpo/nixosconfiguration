{
  imports = [ ./zshCommon.nix ];
  programs.zsh = {
    shellAliases.ls = "ls -lh --color=auto";
    oh-my-zsh.theme = "agnoster";
  };
  localVariables.PROMPT = ''
     WORKSPACE=$(wmctrl -d | grep "*" | cut -f1 -d' ')
     if [ $SHELL != "/run/current-system/sw/bin/zsh" ]
     then
       PROMPT='%K{green}%F{black} nix-shell %F{green}%K{black}$(echo "\ue0b0")%k%f'$PROMPT
     else
       cd $(grep -e "^''${WORKSPACE} .*" ~/scripts/var/roots | cut -f2 -d' ')
     fi
  '';
}
