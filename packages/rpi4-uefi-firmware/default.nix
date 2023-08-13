{ lib, stdenv, fetchzip }:
stdenv.mkDerivation (finalAttrs: {
  pname = "rpi4-uefi-firmware";
  version = "v1.35";
  src = fetchzip {
    url = "https://github.com/pftf/RPi4/releases/download/${finalAttrs.version}/RPi4_UEFI_Firmware_${finalAttrs.version}.zip";
    hash = "sha256-/eeCXVayEfkk0d5OR743djzRgRnCU1I5nJrdUoGmfUk=";
    stripRoot=false;
  };
  phases = "installPhase";
  installPhase = ''
    mkdir $out
    cp -r $src/* $out
    rm $out/Readme.md
  '';
})
