{ config, lib, pkgs, private, ... }:

let
  realName = "Florian Engel";
  mailAddress = "engelflorian@posteo.de";
  key = "4E2D9B26940E0DABF376B7AF76762421D45837DE";

in {
  imports = [
    ../../../home.nix
    ./zsh.nix
    ./defaultApplications.nix
    ../../home/configuration.nix
    ./firefox.nix
  ];

  programs = {
    nix-index.enable = true;
    gh = {
      enable = true;
      settings = { git_protocol = "ssh"; };
    };
    zoxide = {
      enable = true;
      enableZshIntegration = true;
    };
    git = {
      userName = realName;
      userEmail = mailAddress;
      delta.enable = true;
      signing = {
        inherit key;
        signByDefault = true;
      };
      includes = [{
        contents = {
          user = {
            email = "mail";
            signingKey = "66ADDC714AD52330F69371F2BEC83EA3C41DBF14";
          };

          # TODO Fix endless loop
          hooks.pre-commit = pkgs.writeShellScript "test" "test";
        };
        condition = "hasconfig:remote.*.url:git@github.com:factisresearch/**";
      }];
      extraConfig = {
        merge.conflictStyle = "zdiff3";
        github.user = "StillerHarpo";
        gitlab.user = "StillerHarpo";
      };
    };
    gpg = {
      enable = true;
      settings.keyserver = "hkps://keys.openpgp.org";
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
    gpg-agent = {
      enable = true;
      defaultCacheTtl = 1800;
      enableSshSupport = true;
      # pinentryFlavor = "qt";
    };
  };

  xsession.windowManager.xmonad = {
    config = ../../../haskell/xmonad-thinkpad/xmonad.hs;
    extraPackages = haskellPackages:
      with haskellPackages; [
        my-common
        MissingH
        protolude
        pretty-simple
      ];
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
        format = "<b>%s</b>\\n%b";
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

  xdg.configFile."networkmanager-dmenu/config.ini".text =
    lib.generators.toINI { } {
      dmenu.dmenu_command = "rofi";
      dmenu_passphrase.rofi_obscure = true;
      editor.terminal = "alacritty";
    };
  home.file.".kodi/userdata/addon_data/plugin.video.invidious/settings.xml".source =
    ./kodi/invidious.xml;
  home.file.".kodi/userdata/addon_data/plugin.video.arteplussept/settings.xml".source =
    ./kodi/arteplussept.xml;
  # home.file.".kodi/userdata/keymaps/vim.xml".source = ./kodi/vim.xml;
  home.file.".kodi/userdata/addon_data/skin.estuary/settings.xml".source =
    ./kodi/estuary.xml;
  home.file.".kodi/userdata/sources.xml".source = ./kodi/sources.xml;
  home.file.".authinfo.gpg".source = ./authinfo.gpg;
}
