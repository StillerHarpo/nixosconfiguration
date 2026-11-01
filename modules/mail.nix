{
  config,
  pkgs,
  private,
  lib,
  ...
}:

let
  cfg = config.mail;

  mkMail =
    {
      addressPrefix ? "",
      host ? "",
      address ? "${addressPrefix}@${host}",
      userName ? address,
      imapHost ? "imap.${host}",
      smtpHost ? "smtp.${host}",
      realName,
      key ? "",
      aliases ? [ ],
      passPath,
      primary ? false,
      patterns ? [ "*" ],
    }:
    {
      inherit
        address
        realName
        primary
        userName
        aliases
        ;
      imap.host = imapHost;
      smtp = {
        host = smtpHost;
        port = 465;
      };
      passwordCommand = "cat ${passPath}";
      gpg = {
        inherit key;
        signByDefault = true;
      };
      mbsync = {
        enable = true;
        create = "maildir";
        inherit patterns;
      };
      msmtp.enable = true;
      notmuch.enable = true;
    };
  # TODO put that into varibales
  realName = "Florian Engel";
  key = "4E2D9B26940E0DABF376B7AF76762421D45837DE";
in
{
  options.mail = {
    user = lib.mkOption {
      type = lib.types.str;
      description = lib.mdDoc "The user for which the mails are configured";
    };
    posteo.enable = lib.mkEnableOption (lib.mdDoc "Enable posteo mail");
    librem.enable = lib.mkEnableOption (lib.mdDoc "Enable librem mail");
    gmail.enable = lib.mkEnableOption (lib.mdDoc "Enable gmail mail");
    work = {
      enable = lib.mkEnableOption (lib.mdDoc "Enable work mail");
      passPath = lib.mkOption {
        type = lib.types.path;
        description = lib.mdDoc "Path to password of work mail";
      };
    };
  };
  config = lib.mkIf (cfg.posteo.enable || cfg.librem.enable || cfg.gmail.enable || cfg.work.enable) {
    age.secrets = lib.mkMerge [
      (lib.mkIf cfg.posteo.enable {
        posteo = {
          file = ./secrets/posteo.age;
          owner = cfg.user;
          group = "users";
          mode = "400";
        };
      })
      (lib.mkIf cfg.librem.enable {
        librem = {
          file = ./secrets/librem.age;
          owner = cfg.user;
          group = "users";
          mode = "400";
        };
      })
      (lib.mkIf cfg.gmail.enable {
        gmail = {
          file = ./secrets/gmail.age;
          owner = cfg.user;
          group = "users";
          mode = "400";
        };
      })
      (lib.mkIf cfg.work.enable {
        workMail = {
          file = ./secrets/workMail.age;
          owner = cfg.user;
          group = "users";
          mode = "400";
        };
      })
    ];
    home-manager.users."${cfg.user}" = {
      programs = {
        mbsync.enable = true;
        msmtp.enable = true;
        notmuch.enable = true;
      };
      accounts.email.accounts = lib.mkMerge [
        (lib.mkIf cfg.posteo.enable {
          posteo = mkMail {
            inherit realName key;
            addressPrefix = "engelflorian";
            host = "posteo.de";
            imapHost = "posteo.de";
            smtpHost = "posteo.de";
            primary = true;
            passPath = config.age.secrets.posteo.path;
          };
        })
        (lib.mkIf cfg.librem.enable {
          librem = mkMail {
            inherit realName key;
            addressPrefix = "florianengel";
            host = "librem.one";
            passPath = config.age.secrets.librem.path;
          };

        })
        (lib.mkIf cfg.gmail.enable {
          gmail = mkMail {
            inherit realName key;
            address = "florianengel39@gmail.com";
            host = "gmail.com";
            # Entw&APw-rfe can't be synced, but it shouldn't get any new mails
            # anyway
            # Either Gmail removed Entw&APw-rfe this or its a bug. It is
            # probably a bug because it happens after a major update of nixos
            patterns = [
              "*"
              "![Gmail]/Entw&APw-rfe"
            ];
            passPath = config.age.secrets.gmail.path;
          };

        })
        (lib.mkIf cfg.work.enable {
          cpMedOld = mkMail {
            inherit realName;
            key = "66ADDC714AD52330F69371F2BEC83EA3C41DBF14";
            address = private.workMailOld;
            aliases = [
              private.workMail
              private.workMailAlt
            ];
            host = "gmail.com";
            patterns = [
              "INBOX"
              "[Gmail]/Sent Mail"
              "[Gmail]/Starred"
            ];
            primary = !cfg.posteo.enable;
            passPath = cfg.work.passPath;
          };
        })
      ];
      systemd.user = {
        services.update-mail.Service = {
          Environment = "PATH=${pkgs.coreutils-full}/bin";
          ExecStart = pkgs.writeShellScript "update-mail" ''
            ${lib.getExe config.home-manager.users."${cfg.user}".programs.mbsync.package} -a
            ${lib.getExe pkgs.notmuch} new --decrypt=false
            MAIL_NUM=$(${lib.getExe pkgs.notmuch} search tag:unread | wc -l)
            if [[ $MAIL_NUM > 0 ]]; then
              ${lib.getExe pkgs.libnotify} "You got $MAIL_NUM new mails"
            fi
          '';
        };
        timers.update-mail = {
          Timer = {
            OnBootSec = "2m";
            OnUnitInactiveSec = "15m";
            Unit = "update-mail.service";
          };
          Install.WantedBy = [ "timers.target" ];
        };
      };
    };
    security.apparmor.policies.mails.profile = ''
      ${lib.getExe config.home-manager.users."${cfg.user}".programs.mbsync.package} {
        ${lib.apparmor.generateFileRules {
          allows = [
            "notmuch"
          ]
          ++ (lib.optional cfg.posteo.enable config.age.secrets.posteo.path)
          ++ (lib.optional cfg.librem.enable config.age.secrets.librem.path)
          ++ (lib.optional cfg.gmail.enable config.age.secrets.gmail.path)
          ++ (lib.optional cfg.work.enable config.age.secrets.workMail.path);
          inherit config;
        }}
        network,
      }
      ${lib.getExe pkgs.notmuch} {
        ${lib.apparmor.generateFileRules {
          allows = [
            "notmuch"
            "gnupg"
          ];
          inherit config;
        }}
      }
    '';
  };
}
