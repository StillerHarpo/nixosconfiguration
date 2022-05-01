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
rec {
  generateFileRules = allows:
    ''
    file,
    ${if !elem "ssh" allows
      then ''
        deny rw /home/florian/.ssh/**,
        deny rw /home/florian/.ssh,
        deny rw /root/.ssh/**,
        deny rw /root/.ssh,
       ''
      else ""}
    ${if !elem "gnupg" allows
      then ''
        deny rw /home/florian/.gnupg/**,
        deny rw /home/florian/.gnupg,
        deny rw /root/.gnupg/**,
        deny rw /root/.gnupg,
        ''
      else ""}
        deny rw /home/florian/Dokumente/**,
        deny rw /home/florian/Dokumente,
    ${if !elem "pass" allows
      then ''
        deny rw /home/florian/.password-store/**,
        deny rw /home/florian/.password-store,
        ''
      else ""}
    ${if !elem "firefox" allows
      then ''
        deny rw /home/florian/.mozilla/**,
        deny rw /home/florian/.mozilla,
        ''
      else ""}
    ${if !elem "notmuch" allows
      then ''
        deny rw /home/florian/Maildir/**,
        deny rw /home/florian/Maildir,
        ''
      else ""}
   '';
  defaultProfile = ''
    ${generateFileRules []}
    network,
    capability,
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
