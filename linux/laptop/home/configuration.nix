{
  lib,
  pkgs,
  private,
  osConfig,
  ...
}:

let
  realName = "Florian Engel";
  mailAddress = "engelflorian@posteo.de";
  key = "4E2D9B26940E0DABF376B7AF76762421D45837DE";

in
{
  imports = [
    ../../../home.nix
    ./defaultApplications.nix
    ./themeChanger.nix
  ];

  zsh = {
    enableMeqoSsh = true;
    extraPlugins = [ "pass" ];
  };

  librewolf = {
    enable = true;
    additionalProfiles.work.id = 1;
  };

  programs = {
    nix-index.enable = true;
    gh = {
      enable = true;
      settings = {
        git_protocol = "ssh";
      };
    };
    zoxide = {
      enable = true;
      enableZshIntegration = true;
    };
    delta = {
      enable = true;
      enableGitIntegration = true;
    };
    git = {
      settings = {
        user = {
          name = realName;
          email = mailAddress;
        };
        merge.conflictStyle = "zdiff3";
        github.user = "StillerHarpo";
        gitlab.user = "StillerHarpo";
      };
      signing = {
        inherit key;
        signByDefault = true;
      };
      includes = [
        {
          contents.user = {
            email = private.workMail;
            signingKey = "66ADDC714AD52330F69371F2BEC83EA3C41DBF14";
          };
          condition = "hasconfig:remote.*.url:git@github.com:factisresearch/**";
        }
      ];
    };
    gpg = {
      enable = true;
      settings.keyserver = "hkps://keys.openpgp.org";
    };

    notmuch.hooks.postNew = "notmuch tag --input=${osConfig.age.secrets.notmuchTags.path}";
  };

  services.gpg-agent = {
    enable = true;
    defaultCacheTtl = 1800;
    enableSshSupport = true;
    pinentry.package = pkgs.pinentry-gnome3;
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

  xdg.configFile."networkmanager-dmenu/config.ini".text = lib.generators.toINI { } {
    dmenu.dmenu_command = "rofi";
    dmenu_passphrase.rofi_obscure = true;
    editor.terminal = "alacritty";
  };

  home.file.".authinfo.gpg".source = ./authinfo.gpg;

}
