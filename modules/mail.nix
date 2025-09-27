{ config, pkgs, private, lib, ... }:

let
  cfg = config.mail;

  mkMail =
    { addressPrefix ? "", host ? "", address ? "${addressPrefix}@${host}"
    , userName ? address, imapHost ? "imap.${host}", smtpHost ? "smtp.${host}"
    , realName, key ? "", aliases ? [],
      passPath, primary ? false, patterns ? [ "*" ]
    }: {
      inherit address realName primary userName aliases;
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
    enable = lib.mkEnableOption (lib.mdDoc "Load emails");
  };
  config = lib.mkIf cfg.enable {

    age.secrets = {
      posteo = {
        file = ./secrets/posteo.age;
        owner = "florian";
        group = "users";
        mode = "400";
      };
      librem = {
        file = ./secrets/librem.age;
        owner = "florian";
        group = "users";
        mode = "400";
      };
      gmail = {
        file = ./secrets/gmail.age;
        owner = "florian";
        group = "users";
        mode = "400";
      };
      workMail = {
        file = ./secrets/workMail.age;
        owner = "florian";
        group = "users";
        mode = "400";
      };
    };
    home-manager.users.florian = {
      accounts.email.accounts = {
        posteo = mkMail {
          inherit realName key;
          addressPrefix = "engelflorian";
          host = "posteo.de";
          imapHost = "posteo.de";
          smtpHost = "posteo.de";
          primary = true;
          passPath = config.age.secrets.posteo.path;
        };
        librem = mkMail {
          inherit realName key;
          addressPrefix = "florianengel";
          host = "librem.one";
          passPath = config.age.secrets.librem.path;
        };
        gmail = mkMail {
          inherit realName key;
          address = "florianengel39@gmail.com";
          host = "gmail.com";
          # Entw&APw-rfe can't be synced, but it shouldn't get any new mails
          # anyway
          # Either Gmail removed Entw&APw-rfe this or its a bug. It is
          # probably a bug because it happens after a major update of nixos
          patterns = ["*" "![Gmail]/Entw&APw-rfe"];
          passPath = config.age.secrets.gmail.path;
        };
        cpMedOld = mkMail {
          inherit realName key;
          address = private.workMailOld;
          aliases = [ private.workMail ];
          host = "gmail.com";
          patterns = [ "INBOX" "[Gmail]/Sent Mail" "[Gmail]/Starred" ];
          passPath = config.age.secrets.workMail.path;
        };
      };
      systemd.user = {
        services.update-mail.Service = {
          Environment = "PATH=${pkgs.coreutils-full}/bin";
          ExecStart =
            pkgs.writeShellScript "update-mail" ''
              ${lib.getExe config.home-manager.users.florian.programs.mbsync.package} -a
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
       ${lib.getExe config.home-manager.users.florian.programs.mbsync.package} {
         ${lib.apparmor.generateFileRules
           [ "notmuch"
              config.age.secrets.posteo.path
              config.age.secrets.librem.path
              config.age.secrets.gmail.path
              config.age.secrets.workMail.path
           ]
           config}
         network,
       }
       ${lib.getExe pkgs.notmuch} {
         ${lib.apparmor.generateFileRules [ "notmuch" "gnupg"] config}
       }
    '';
  };
}
