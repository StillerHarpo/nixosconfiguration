# TODO wirte rules which get used if it is environment.systemPackages or in home.packages
{ lib }:
with lib;
rec {
  generateFileRules =
    {
      allows ? [ ],
      config,
      fileDefault ? "file",
    }:
    ''
      # Allow other processes to read our /proc entries, futexes, perf tracing and
      # kcmp for now (they will need 'read' in the first place). Administrators can
      # override with:
      #   deny ptrace (readby) ...
      ptrace (readby),

      # Allow unconfined processes to send us signals by default
      signal (receive) peer=unconfined,

      # Allow us to signal ourselves
      signal peer=@{profile_name},

      # Allow us to ptrace read ourselves
      ptrace (read) peer=@{profile_name},

      ${fileDefault},
      ${concatMapStrings (x: ''
        audit deny ${x} mrwlkx,
        audit deny ${x}/{,**} mrwlkx,
      '') (protectFiles allows config)}

      # Dont allow overriding init files
      deny /home/florian/.{zshrc,zshenv,zprofile,profile,zlogin,zlogout} wl,
      deny /home/florian/.config/{sway,systemd,autostart,direnv,environment.d}/** wl,
      deny /home/florian/.local/share/applications/** wl,
      deny /home/florian/.config/alacritty/** wl,
      deny /home/florian/.emacs.d/** wl,
      deny /home/florian/.config/emacs wl,
    '';
  defaultRules = config: ''
    ${generateFileRules { inherit config; }}
    network,
  '';
}
