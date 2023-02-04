{ lib }:
with lib; {
  protectFiles = allows:
    flatten
    ((optional (!elem "ssh" allows) [ "/home/florian/.ssh" "/root/.ssh" ])
      ++ (optional (!elem "gnupg" allows) [
        "/home/florian/.gnupg"
        "/root/.gnupg"
      ]) ++ (optional (!elem "docs" allows) [ "/home/florian/Dokumente" ])
      ++ (optional (!elem "paperless" allows) [
        "/home/florian/paperlessInput"
        "/var/lib/paperless"
      ]) ++ (optional (!elem "pass" allows) [ "/home/florian/.password-store" ])
      ++ (optional (!elem "firefox" allows) [ "/home/florian/.mozilla" ])
      ++ (optional (!elem "notmuch" allows) [ "/home/florian/Maildir" ])
      ++ (optional (!elem "authinfo" allows) [
        "/home/florian/authinfo"
        "/home/florian/authinfo.gpg"
      ]));
}
