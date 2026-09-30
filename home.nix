{ ... }:

{
  zsh.enable = true;
  programs = {
    alacritty.enable = true;
    git.enable = true;
    direnv = {
      enable = true;
      nix-direnv.enable = true;
    };
  };
}
