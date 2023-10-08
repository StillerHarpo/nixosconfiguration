{ config, lib, pkgs, ... }:

{
  age.secrets.backblaze-restic.file = ./secrets/backblaze-restic.age;
  age.secrets.restic-password.file = ./secrets/restic-password.age;
  services.restic.backups.florian = {
    paths = [
      "/var/lib/paperless/media/documents/archive"
      "/home/florian/Dokumente"
      "/home/florian/.password-store"
      "/home/florian/Maildir"
      "/home/florian/android"
    ];
    repository = "rclone:b2:restic-backup-flo";
    initialize = true;
    passwordFile = config.age.secrets.restic-password.path;
    rcloneConfigFile = config.age.secrets.backblaze-restic.path;
    rcloneOptions = {
      allow-other = true;
      allow-non-empty = true;
      log-level = "INFO";
      buffer-size = "50M";
      drive-acknowledge-abuse = true;
      no-modtime = true;
      vfs-cache-mode = "full";
      vfs-cache-max-size = "20G";
      vfs-read-chunk-size = "32M";
      vfs-read-chunk-size-limit = "256M";
    };
    timerConfig = {
      OnCalendar = "weekly";
      Persistent = true;
    };
  };
}
