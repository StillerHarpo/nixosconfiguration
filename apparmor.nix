with builtins;
let
  getProfiles = pkgs: rule:
    foldl'
      (rulesPkgs: pkg:
        rulesPkgs + "\n" + ''
          ${pkg}/bin/* {
             ${rule}
          }
        ''
      )
      ""
      pkgs;
in
{ defaultProfile = ''
    file,
    network,
    capabilities,
    deny rw /root/.ssh/**,
    deny rw /root/.ssh,
    deny rw /root/.gnupg/**,
    deny rw /root/.gnupg,
    deny rw /home/florian/Dokumente/**,
    deny rw /home/florian/Dokumente,
    deny rw /home/florian/.ssh/**,
    deny rw /home/florian/.ssh,
    deny rw /home/florian/.gnupg/**,
    deny rw /home/florian/.gnupg,
    deny rw /home/florian/.password-store/**,
    deny rw /home/florian/.password-store,
  '';
  generate =
    pkgsAppArmors:
    with (foldl'
      (config: pkgsAppArmor:
        {
          systemPackages = config.systemPackages ++ pkgsAppArmor.pkgs;
          profiles = config.profiles + "\n" + getProfiles pkgsAppArmor.pkgs pkgsAppArmor.profile;
        }
      )
      {
        systemPackages = [];
        profiles = "";
      }
      pkgsAppArmors);
    {
      security.apparmor.policies.allRules.profile = profiles;
      environment.systemPackages = systemPackages;
    };
}
