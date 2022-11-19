{ config, lib, pkgs, self, ... }:

{

  home = {
    sessionVariables = {
      SHELL = pkgs.myshell;
      EDITOR = "vim";
    };
    packages = with pkgs;
      [
        afew # needed for notmuch update in emacs
      ];
  };

  programs = {
    alacritty.enable = true;
    git.enable = true;

    direnv = {
      enable = true;
      nix-direnv.enable = true;
    };
    mbsync.enable = true;
    msmtp.enable = true;
    # FIXME notmuch config can't be found by emacs
    # workaround: ln -s .config/notmuch/notmuchrc .notmuch-config
  };
}
