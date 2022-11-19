{ stdenvNoCC, fetchurl, imagemagick, ... }:

stdenvNoCC.mkDerivation rec {
  name = "textcleaner";
  src = fetchurl {
    url =
      "http://www.fmwconcepts.com/imagemagick/downloadcounter.php?scriptname=${name}&dirname=${name}";
    sha256 = "sha256-f5W7FRf3ZGo9qjPi17AVwKDYhDUU/viEWspDT97UaQ8=";
  };
  patches = [ ./textcleaner.patch ];
  unpackPhase = ''
    cp $src ${name}
  '';
  installPhase = ''
    mkdir -p $out/bin
    cp ${name} $out/bin/
    chmod +x $out/bin/${name}
    substituteInPlace $out/bin/${name} --replace convert ${imagemagick}/bin/convert
  '';
}
