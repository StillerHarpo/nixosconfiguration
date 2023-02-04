# contains some functions from https://github.com/nix-community/home-manager
{ lib }:
with lib;

let
  mkValueString = value:
    if isBool value then
      if value then "true" else "false"
    else if isInt value then
      toString value
    else if (value._type or "") == "literal" then
      value.value
    else if isString value then
      ''"${value}"''
    else if isList value then
      "[ ${strings.concatStringsSep "," (map mkValueString value)} ]"
    else
      abort "Unhandled value type ${builtins.typeOf value}";

  mkKeyValue = { sep ? ": ", end ? ";" }:
    name: value:
    "${name}${sep}${mkValueString value}${end}";

  mkRasiSection = name: value:
    if isAttrs value then
      let
        toRasiKeyValue = generators.toKeyValue { mkKeyValue = mkKeyValue { }; };
        # Remove null values so the resulting config does not have empty lines
        configStr = toRasiKeyValue (filterAttrs (_: v: v != null) value);
      in ''
        ${name} {
        ${configStr}}
      ''
    else
      (mkKeyValue {
        sep = " ";
        end = "";
      } name value) + "\n";

in {
  toRofiRasi = attrs:
    concatStringsSep "\n" (concatMap (mapAttrsToList mkRasiSection) [
      (filterAttrs (n: _: n == "@theme") attrs)
      (filterAttrs (n: _: n == "@import") attrs)
      (removeAttrs attrs [ "@theme" "@import" ])
    ]);
  toAlacrittyJSON = attrs:
    (replaceStrings [ "\\\\" ] [ "\\" ] (builtins.toJSON attrs));
  toGtk3Ini = generators.toINI {
    mkKeyValue = key: value:
      let
        value' = if isBool value then
          (if value then "true" else "false")
        else
          toString value;
      in "${key}=${value'}";
  };
  toDconfIni = generators.toINI {
    mkKeyValue = key: value: "${key}=${toString (hm.gvariant.mkValue value)}";
  };

}
