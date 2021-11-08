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
    accounts.posteo = import ./mail.nix {
      inherit realName key;
      addressPrefix = "engelflorian";
      host = "posteo.de";
      imapHost = "posteo.de";
      smtpHost = "posteo.de";
      primary = true;
    };
    accounts.librem = import ./mail.nix {
      inherit realName key;
      addressPrefix = "florianengel";
      host = "librem.one";
    };
    accounts.gmail = import ./mail.nix {
      inherit realName key;
      address = "florianengel39@gmail.com";
      passName = "gmailMu4e";
      host = "gmail.com";
    };
#    accounts.cpMed = import ./mail.nix {
#      inherit realName key;
#      address = "engel@cp-med.com";
#      addressPrefix = "florian.engel";
#      passName = "arbeitNotmuch";
#      host = "gmail.com";
#    };
  };
  services = {
    emacs.enable = true;
    gpg-agent = {
      enable = true;
      defaultCacheTtl = 1800;
      enableSshSupport = true;
      # pinentryFlavor = "qt";
    };
  };
  gtk = {
    enable = true;
    iconTheme = {
      name = "Adwaita-dark";
      package = pkgs.gnome3.adwaita-icon-theme;
    };
    theme = {
      name = "Adwaita-dark";
      package = pkgs.gnome3.adwaita-icon-theme;
    };
  };
}
