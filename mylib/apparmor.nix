{ lib }:
with lib; rec {
  getProfiles = pkgs: rule:
    foldl' (rulesPkgs: pkg:
      rulesPkgs + "\n" + ''
        ${pkg}/bin/* {
           ${rule}
        }
      '') "" pkgs;
  generateFileRules = allows: ''
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

    file,
    ${concatMapStrings (x: ''
      deny rw ${x},
      deny rw ${x}/**,
    '') (protectFiles allows)}
  '';
  defaultProfile = ''
    ${generateFileRules [ ]}
    network,
    capability,
  '';
  generate = pkgsAppArmors:
    with (foldl' (config: pkgsAppArmor: {
      systemPackages = config.systemPackages ++ pkgsAppArmor.pkgs;
      profiles = config.profiles + "\n"
        + getProfiles pkgsAppArmor.pkgs pkgsAppArmor.profile;
    }) {
      systemPackages = [ ];
      profiles = "";
    } pkgsAppArmors); {
      security.apparmor.policies.allRules.profile = profiles;
      environment.systemPackages = systemPackages;
    };
}
