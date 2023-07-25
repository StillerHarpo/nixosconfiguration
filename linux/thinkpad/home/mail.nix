{ addressPrefix ? "", host ? "", address ? "${addressPrefix}@${host}"
, userName ? address, imapHost ? "imap.${host}", smtpHost ? "smtp.${host}"
, realName, key ? "", passName ? (if userName == address && host != "" then
  address
else if host == "" then
  "${userName}"
else
  "${userName}@${host}"), primary ? false, patterns ? [ "*" ] }: {
    inherit address realName primary userName;
    imap.host = imapHost;
    smtp = {
      host = smtpHost;
      port = 465;
    };
    passwordCommand = "pass show ${passName}";
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
  }
