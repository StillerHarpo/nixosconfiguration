{
  imports = [ ./zshCommon.nix ];
  programs.zsh = {
    shellAliases.ls = "ls -lh";
    oh-my-zsh.theme = "agnoster";
  };
}
