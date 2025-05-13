{ config, lib, pkgs, private, osConfig, ... }:

let
  realName = "Florian Engel";
  mailAddress = "engelflorian@posteo.de";
  key = "4E2D9B26940E0DABF376B7AF76762421D45837DE";

in {
  imports = [
    ../../../home.nix
    ./zsh.nix
    ./defaultApplications.nix
    ./themeChanger.nix
    ../../home/configuration.nix
  ];

  xsession.windowManager.xmonad.extraPackages = haskellPackages:
    with haskellPackages; [
      my-common
      MissingH
      protolude
      pretty-simple
    ];

  firefox = {
    enable = true;
    additionalProfiles.work.id = 1;
  };

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
        contents.user = {
          email = private.workMail;
          signingKey = "66ADDC714AD52330F69371F2BEC83EA3C41DBF14";
        };
        condition = "hasconfig:remote.*.url:git@github.com:factisresearch/**";
      }];
      extraConfig = {
        merge.conflictStyle = "zdiff3";
        github.user = "StillerHarpo";
        gitlab.user = "StillerHarpo";
      };
      # TODO move this into includes. Debug endless loop
      hooks.pre-commit = pkgs.writeShellScript "pre-commit-hook" ''
        branch="$(git rev-parse --abbrev-ref HEAD)"

        if pwd | grep checkpad && [ "$branch" = "develop" ] || [ "$branch" = "prod" ]; then
          echo "You can't commit directly to " $branch " branch"
          exit 1
        fi
      '';
    };
    gpg = {
      enable = true;
      settings.keyserver = "hkps://keys.openpgp.org";
    };

    notmuch = {
      enable = true;
      hooks.postNew = "notmuch tag --input=${osConfig.age.secrets.notmuchTags.path}";
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
      # Entw&APw-rfe can't be synced, but it shouldn't get any new mails
      # anyway
      # Either Gmail removed Entw&APw-rfe this or its a bug. It is
      # probably a bug because it happens after a major update of nixos
      patterns = ["*" "![Gmail]/Entw&APw-rfe"];
    };
    accounts.cpMedOld = import ./mail.nix {
      inherit realName key;
      address = private.workMailOld;
      aliases = [ private.workMail ];
      passName = "mbsync/${private.workMailOld}";
      host = "gmail.com";
      patterns = [ "INBOX" "[Gmail]/Sent Mail" "[Gmail]/Starred" ];
    };
  };
  services.gpg-agent = {
    enable = true;
    defaultCacheTtl = 1800;
    enableSshSupport = true;
    pinentryPackage =  pkgs.pinentry-gnome3;
  };
  emacs.enable = true;


  services.dunst = {
    enable = true;
    iconTheme = {
      name = "Adwaita";
      package = pkgs.adwaita-icon-theme;
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

  home.file.".aspell.conf".source = pkgs.runCommand "aspell.conf" {} ''
        ${pkgs.lib.getExe' pkgs.myAspellWithDicts "aspell"} dump config > $out
      '';

  home.file.".authinfo.gpg".source = ./authinfo.gpg;

}
