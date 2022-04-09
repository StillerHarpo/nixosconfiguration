{
  imports = [ ../zsh.nix ];
  programs.zsh = {
    shellAliases.ls = "ls -lh";
    oh-my-zsh.theme = "agnoster";
  };
}
