{ config, pkgs, lib, inputs, private, ... }:
{
  options.isRealSystem = lib.mkOption {
    type = lib.types.bool;
    default = true;
    description = lib.mdDoc "Should we use encrypted files. Only neccessary on the real system";
  };
}
