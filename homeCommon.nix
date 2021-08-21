{ config, lib, pkgs, ... }:

let
  defaultShell = "zsh";

in {
  imports = [
    (import ./alacritty.nix defaultShell)
  ];


  home = {
    sessionVariables = {
      SHELL = defaultShell;
      EDITOR = "vim";
    };
    packages = with pkgs; [
      ripgrep # needed for doom search in project FIXME make it only availabe in doom
      afew # needed for notmuch update in emacs
    ];
  };

  programs = {
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
