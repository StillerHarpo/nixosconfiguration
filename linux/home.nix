defaultShell:
{ config, lib, pkgs, ... }:

let
  realName = "Florian Engel";
  mailAddress = "florianengel@librem.one";
  key = "4E2D9B26940E0DABF376B7AF76762421D45837DE";

in {
  imports = [
    (import ../home.nix defaultShell)
    ./thinkpad/zsh.nix
    ./defaultApplications.nix
    (import ./themeChanger.nix defaultShell)
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

    notmuch =
      let
        notmuchTags = with builtins; toFile "notmuchTags" (readFile ../notmuchTags);
      in {
        enable = true;
        hooks.postNew = ''
          notmuch tag --input=${notmuchTags}
        '';
      };
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
  services.dunst = {
    enable = true;
    iconTheme = {
      name = "Adwaita";
      package = pkgs.gnome3.adwaita-icon-theme;
      size = "16x16";
    };
    settings = {
      global = {
        monitor = 0;
        geometry = "600x50-10+10";
        shrink = "yes";
        transparency = 10;
        padding = 16;
        horizontal_padding = 16;
        font = "JetBrainsMono Nerd Font 10";
        line_height = 4;
        format = ''<b>%s</b>\n%b'';
      };
      urgency_low = {
        frame_color = "#222222";
        background = "#111111";
        foreground = "#11AA11";
      };
      urgency_normal = {
        frame_color = "#222222";
        background = "#111111";
        foreground = "#11AA11";
      };
      urgency_critical = {
        frame_color = "#222222";
        background = "#111111";
        foreground = "#AA1111";
      };
    };
  };
}
