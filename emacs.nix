with import <nixpkgs> {};

let
  myEmacs = pkgs.emacs25-nox; 
  emacsWithPackages = (pkgs.emacsPackagesNgGen myEmacs).emacsWithPackages; 
in
  emacsWithPackages (epkgs: (with epkgs.melpaStablePackages; [ 
    magit          # ; Integrate git <C-x g>
  ]) ++ (with epkgs.melpaPackages; [ 
    undo-tree      # ; <C-x u> to show the undo tree
    nix-mode
    nixos-options
    company-nixos-options
    nix-sandbox
    haskell-mode
    flycheck 
    evil
    yasnippet
  ]) ++ (with epkgs.elpaPackages; [ 
    auctex         # ; LaTeX mode
    beacon         # ; highlight my cursor when scrolling
    nameless       # ; hide current package name everywhere in elisp code
  ]) ++ [
    pkgs.notmuch   # From main packages set 
  ])
