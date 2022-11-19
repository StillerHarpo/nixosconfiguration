{ addressPrefix ? "", host ? "", address ? "${addressPrefix}@${host}"
, userName ? address, imapHost ? "imap.${host}", smtpHost ? "smtp.${host}"
, realName, key ? "", passName ? (if userName == address && host != "" then
  address
else if host == "" then
  "${userName}"
else
  "${userName}@${host}"), primary ? false }: {
    inherit address realName primary userName;
    imap.host = imapHost;
    smtp.host = smtpHost;
    passwordCommand = "pass show ${passName}";
    gpg = {
      inherit key;
      signByDefault = true;
    };
    mbsync = {
      enable = true;
      create = "maildir";
    };
    msmtp.enable = true;
    notmuch.enable = true;
  }
