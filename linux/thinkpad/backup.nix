{ config, lib, pkgs, ... }:

{
  age.secrets.backblaze.file = ./secrets/backblaze.age;
  systemd.services.backblaze = {
    description = "Mount backblaze";
    wantedBy = [ "multi-user.target" ];
    serviceConfig = {
      ExecStartPre = "${pkgs.coreutils}/bin/mkdir -p /var/borg-backup";
      ExecStart = ''
        ${pkgs.rclone}/bin/rclone mount 'b2:borg-backup-flo/' /var/borg-backup \
          --config=${config.age.secrets.backblaze.path} \
          --allow-other \
          --allow-non-empty \
          --log-level=INFO \
          --buffer-size=50M \
          --drive-acknowledge-abuse=true \
          --no-modtime \
          --vfs-cache-mode full \
          --vfs-cache-max-size 20G \
          --vfs-read-chunk-size=32M \
          --vfs-read-chunk-size-limit=256M
      '';
      ExecStop = "${pkgs.fuse}/bin/fusermount -u /var/borg-backup";
      Type = "notify";
      Restart = "always";
      RestartSec = "10s";
      Environment = [ "PATH=/run/wrappers/bin/:$PATH" ];
    };
  };
  services.borgbackup.jobs."florian" = {
    paths = [
      "/var/lib/paperless/media/documents/archive"
      "/home/florian/Dokumente"
      "/home/florian/.password-store"
      "/home/florian/Maildir"
      "/home/florian/android"
    ];
    repo = "/var/borg-backup/florian_borg_repo";
    encryption = {
      mode = "repokey-blake2";
      passCommand = "cat /root/borgbackup/passphrase";
    };
    compression = "auto,lzma";
    startAt = "weekly";
  };
}
