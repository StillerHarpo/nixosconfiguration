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
    rev = "6d4030e494039383664a0c08348a5c137eb4641c";
    hash = "sha256-FzeGnxjtCpeYbDU3GzadCTnLGtDPoe1q8aTpaZBck7A=";
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
