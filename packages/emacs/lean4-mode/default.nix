{
  trivialBuild,
  pkgs,
  dash,
  flycheck,
  lsp-mode,
  magit-section,
  ...
}:
trivialBuild {
  pname = "lean4-mode";
  version = "master";
  src = pkgs.fetchFromGitHub {
    owner = "leanprover";
    repo = "lean4-mode";
    rev = "da7b63d";
    hash = "sha256-U6MJIcBZf1XrUnssipgEy0BhF4/zhKVWblUvNqKNQe0=";
  };
  packageRequires = [
    dash 
    flycheck
    lsp-mode
    magit-section
  ];
  fixupPhase = ''
    mkdir $out/share/emacs/site-lisp/data
    cp $src/data/abbreviations.json $out/share/emacs/site-lisp/data/
  '';
}
