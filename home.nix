{ config, lib, pkgs, ... }:

let
  realName = "Florian Engel";
  mailAddress = "florianengel@librem.one";
  key = "4E2D9B26940E0DABF376B7AF76762421D45837DE";
  defaultShell = "zsh";

  doom-emacs = pkgs.callPackage (builtins.fetchTarball {
    url = https://github.com/vlaci/nix-doom-emacs/archive/master.tar.gz;
  }) {
    doomPrivateDir = ./doom.d;  # Directory containing your config.el init.el
    # and packages.el files
  };
in {
  imports = [
    ./zsh.nix
    (import ./alacritty.nix defaultShell)
    ./defaultApplications.nix
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
    git = {
      enable = true;
      userName = realName;
      userEmail = mailAddress;
    };
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
    gpg = {
      enable = true;
      settings.keyserver = hkps://keys.openpgp.org;
    };
  };
  accounts.email = {
    accounts.librem = import ./mail.nix {
      inherit realName key;
      addressPrefix = "florianengel";
      host = "librem.one";
      primary = true;
    };
    accounts.gmail = import ./mail.nix {
      inherit realName key;
      address = "florianengel39@gmail.com";
      passName = "gmailMu4e";
      host = "gmail.com";
    };
    accounts.uni = import ./mail.nix {
      inherit realName key;
      address = "florian.engel@student.uni-tuebingen.de";
      userName = "zxmvm60";
      imapHost = "mailserv.uni-tuebingen.de";
      smtpHost = "smtpserv.uni-tuebingen.de";
    };
    accounts.arbeit = import ./mail.nix {
      inherit realName key;
      addressPrefix = "florian.engel";
      host = "active-group.de";
      userName = "engel";
    };
  };
  services = {
    emacs.enable = true;
    gpg-agent = {
      enable = true;
      defaultCacheTtl = 1800;
      enableSshSupport = true;
    };
  };
}
