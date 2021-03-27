{ config, lib, pkgs, ... }:

let
  defaultShell = "zsh";

  doom-emacs = pkgs.callPackage (builtins.fetchTarball {
    url = https://github.com/vlaci/nix-doom-emacs/archive/master.tar.gz;
  }) {
    doomPrivateDir = ./doom.d;  # Directory containing your config.el init.el
    # and packages.el files
  };
in {
  imports = [
    (import ./alacritty.nix defaultShell)
  ];
  home = {
    file.".emacs.d/init.el".text = ''
        (load "default.el")
      '';
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
      enableNixDirenvIntegration = true;
    };
    mbsync.enable = true;
    msmtp.enable = true;
    # FIXME notmuch config can't be found by emacs
    # workaround: ln -s .config/notmuch/notmuchrc .notmuch-config
    notmuch.enable = true;
    emacs = {
      enable = true;
      package = doom-emacs;
    };
  };
}
