{ config, lib, pkgs, ... }:

let resources = pkgs.fetchzip {
      name = "pia-vpn-config";
      url = "https://www.privateinternetaccess.com/openvpn/openvpn.zip";
      sha256 = "ZA8RS6eIjMVQfBt+9hYyhaq8LByy5oJaO9Ed+x8KtW8=";
      stripRoot = false;
    };
    fixup = (builtins.replaceStrings [ ".ovpn" "_" ] [ "" "-" ]);
    servers =
      (builtins.filter (name: !(isNull (builtins.match ".+ovpn$" name)))
        (builtins.attrNames (builtins.readDir resources)));
    make_server = (name: {
      name = fixup name;
      value = {
        autoStart = false;
        authUserPass = config.services.pia.authUserPass;
        config = "config ${resources}/${name}";
        updateResolvConf = true;
      };
    });
in
{
  age = {
    secrets = {
      piaAuthUserPass.file = ./secrets/piaAuthUserPass.age;
    };
  };
  services.openvpn.servers = let
    resources = pkgs.fetchzip {
      name = "pia-vpn-config";
      url = "https://www.privateinternetaccess.com/openvpn/openvpn.zip";
      sha256 = "ZA8RS6eIjMVQfBt+9hYyhaq8LByy5oJaO9Ed+x8KtW8=";
      stripRoot = false;
    };
    fixup = (builtins.replaceStrings [ ".ovpn" "_" ] [ "" "-" ]);
    servers =
      (builtins.filter (name: !(isNull (builtins.match ".+ovpn$" name)))
        (builtins.attrNames (builtins.readDir resources)));
    make_server = (name: {
      name = fixup name;
      value = {
        autoStart = false;
        config = ''
          config ${resources}/${name}
          auth-user-pass ${config.age.secrets.piaAuthUserPass.path}
        '';
        updateResolvConf = true;
      };
    });
  in builtins.listToAttrs (map make_server servers);
}
