{ config, lib, pkgs, ... }:

let
  realName = "Florian Engel";
  mailAddress = "florianengel@librem.one";
  key = "4E2D9B26940E0DABF376B7AF76762421D45837DE";

in {
  imports = [
    ./homeCommon.nix
    ./zshLinux.nix
    ./defaultApplications.nix
  ];

  programs = {
    git = {
      userName = realName;
      userEmail = mailAddress;
    };
    gpg = {
      enable = true;
      settings.keyserver = hkps://keys.openpgp.org;
    };

    notmuch.enable = true;
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
