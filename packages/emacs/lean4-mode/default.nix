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
    owner = "leanprover-community";
    repo = "lean4-mode";
    rev = "1388f9d1429e38a39ab913c6daae55f6ce799479";
    hash = "sha256-6XFcyqSTx1CwNWqQvIc25cuQMwh3YXnbgr5cDiOCxBk=";
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
