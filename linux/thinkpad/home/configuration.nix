defaultShell:
{ config, lib, pkgs, ... }:

let
  realName = "Florian Engel";
  mailAddress = "florianengel@librem.one";
  key = "4E2D9B26940E0DABF376B7AF76762421D45837DE";

in {
  imports = [
    (import ../../../home.nix defaultShell)
    ./zsh.nix
    ./defaultApplications.nix
    ../../home/configuration.nix
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
        notmuchTags = with builtins; toFile "notmuchTags" (readFile ./notmuchTags);
      in {
        enable = true;
        hooks.postNew = ''
          notmuch tag --input=${notmuchTags}
        '';
      };

    firefox = {
      enable = true;
      extensions =
        with pkgs.nur.repos.rycee.firefox-addons;
        let ffreszoom = buildFirefoxXpiAddon rec {
              pname = "ffreszoom";
              version = "0.3.1";
              addonId = "{b2e3360c-a72c-4ba4-813c-603a1fa34356}";
              url = "https://addons.mozilla.org/firefox/downloads/file/1712974/ffreszoom-${version}-fx.xpi";
              sha256 = "078h0x6xikg7dlzji32vfcchynmay56wixkhvw1b5rpyq2z8y3cn";

              meta = with lib;
                {
                  homepage = "https://github.com/notartom/ffreszoom";
                  description = "Sets the zoom level based on the screen resolution.";
                  license = licenses.gpl3;
                  platforms = platforms.all;
                };
            };
        in [
          user-agent-string-switcher
          tridactyl
          noscript
          ublock-origin
          ffreszoom
        ];
      profiles.default.settings = {
        # colors
        "widget.content.allow-gtk-dark-theme" = true;
        "browser.display.use_system_colors" = true;

        "browser.urlbar.placeholderName" = "DuckDuckGo";

        # Enable HTTPS-Only Mode
        "dom.security.https_only_mode" = true;
        "dom.security.https_only_mode_ever_enabled" = true;

        # Privacy settings
        "privacy.donottrackheader.enabled" = true;
        "privacy.trackingprotection.enabled" = true;
        "privacy.trackingprotection.socialtracking.enabled" = true;
        "privacy.partition.network_state.ocsp_cache" = true;

        # Disable all sorts of telemetry
        "browser.newtabpage.activity-stream.feeds.telemetry" = false;
        "browser.newtabpage.activity-stream.telemetry" = false;
        "browser.ping-centre.telemetry" = false;
        "toolkit.telemetry.archive.enabled" = false;
        "toolkit.telemetry.bhrPing.enabled" = false;
        "toolkit.telemetry.enabled" = false;
        "toolkit.telemetry.firstShutdownPing.enabled" = false;
        "toolkit.telemetry.hybridContent.enabled" = false;
        "toolkit.telemetry.newProfilePing.enabled" = false;
        "toolkit.telemetry.reportingpolicy.firstRun" = false;
        "toolkit.telemetry.shutdownPingSender.enabled" = false;
        "toolkit.telemetry.unified" = false;
        "toolkit.telemetry.updatePing.enabled" = false;

        # As well as Firefox 'experiments'
        "experiments.activeExperiment" = false;
        "experiments.enabled" = false;
        "experiments.supported" = false;
        "network.allow-experiments" = false;

        # Disable Pocket Integration
        "browser.newtabpage.activity-stream.section.highlights.includePocket" = false;
        "extensions.pocket.enabled" = false;
        "extensions.pocket.api" = "";
        "extensions.pocket.oAuthConsumerKey" = "";
        "extensions.pocket.showHome" = false;
        "extensions.pocket.site" = "";
      };
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

  xsession.windowManager.xmonad = {
    config = ./xmonad/xmonad.hs;
    libFiles = {
      "Bookmarks.hs" = ./xmonad/lib/Bookmarks.hs;
      "Utils.hs" = ./xmonad/lib/Utils.hs;
      "Xrandr.hs" = ./xmonad/lib/Xrandr.hs;
    };
    extraPackages = haskellPackages:
      with haskellPackages; [MissingH protolude];
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

  xdg.configFile."networkmanager-dmenu/config.ini".text = lib.generators.toINI {} {
    dmenu.dmenu_command = "rofi";
    dmenu_passphrase.rofi_obscure = true;
    editor.terminal = "alacritty";
  };
}
