lib: _:
let callLibs = file: import file { inherit lib; };
in {
  converters = callLibs ./converters.nix;
  inherit (callLibs ./protected.nix) protectFiles;
  apparmor = callLibs ./apparmor.nix;
}
