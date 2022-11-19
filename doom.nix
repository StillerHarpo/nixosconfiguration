{ config, lib, pkgs, inputs, ... }:

with lib; let
  myemacs = with pkgs; (emacsPackagesFor emacsNativeComp).emacsWithPackages (
    epkgs: [ epkgs.vterm ]
  );
  # Taken from https://github.com/Mic92/dotfiles/blob/3e85e2b8a25f5dc16bb1b47e53566a4e8330974b/nixpkgs-config/modules/emacs/default.nix#L26
  daemonScript = pkgs.writeScript "emacs-daemon" ''
    #!${pkgs.zsh}/bin/zsh -l
    export PATH=$PATH:${lib.makeBinPath [pkgs.gcc pkgs.git pkgs.sqlite pkgs.unzip]}
    if [ ! -d $HOME/.emacs.d/.git ]; then
      mkdir -p $HOME/.emacs.d
      git -C $HOME/.emacs.d init
    fi
    git -C $HOME/.emacs.d remote add origin https://github.com/doomemacs/doomemacs.git || \
      git -C $HOME/.emacs.d remote set-url origin https://github.com/doomemacs/doomemacs.git
    if [ $(git -C $HOME/.emacs.d rev-parse HEAD) != ${inputs.doom-emacs.rev} ]; then
      git -C $HOME/.emacs.d fetch https://github.com/doomemacs/doomemacs.git || true
      git -C $HOME/.emacs.d checkout ${inputs.doom-emacs.rev} || true
      nice -n19 YES=1 FORCE=1 $HOME/.emacs.d/bin/doom sync -u || true
    else
      nice -n19 $HOME/.emacs.d/bin/doom sync || true
    fi
    exec ${myemacs}/bin/emacs --daemon
  '';

in {

  home-manager.users.florian = {
    home = {
      packages = [ myemacs ];
      file = {
        ".doom.d/init.el".source = ./doom.d/init.el;
        ".doom.d/packages.el".source = ./doom.d/packages.el;
        ".doom.d/config.el".text = ''
           ${fileContents ./doom.d/config.el}
           (with-nix-pathes '((nix-zsh-path . "${pkgs.myshell}")
             (nix-latexmk-path . "${pkgs.mytexlive}/bin/latexmk")
             (nix-mpv-path . "${pkgs.mpv}/bin/mpv")
             (nix-jdk-path . "${pkgs.jdk}/bin/java")
             (nix-languagetool-path . "${pkgs.languagetool}")))
        '';
      };
    };
    systemd.user.services.emacs-daemon = {
      Install.WantedBy = ["default.target"];
      Service = {
        Type = "forking";
        TimeoutStartSec = "10min";
        Restart = "always";
        ExecStart = toString daemonScript;
      };
    };

  };
}
