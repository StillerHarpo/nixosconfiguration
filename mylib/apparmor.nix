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
    '';
  defaultRules = config: ''
    ${generateFileRules { inherit config; }}
    network,
  '';
}
