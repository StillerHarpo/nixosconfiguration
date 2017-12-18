{ config, ... }:

{
  programs.zsh = {
    enable = true;
    enableAutosuggestions = true;
    syntaxHighlighting.enable = true;
    ohMyZsh = {
      enable = true;
      theme = "agnoster";
      plugins = [ "vi-mode" "per-directory-history" "pass" "nix" ];
    };  
    shellAliases = {
      emacs = "emacsclient -c";
      ls = "ls -lh --color=auto";
      ll = "ls -a";
      srg = "sr google";
      srw = "sr wikipedia";
      rtv = ''nix-shell -p python -p pythonPackages.six --run "export BROWSER=linkopen; export EDITOR=vim; export PAGER=less;rtv --asci"'';
      vimread = "vim -RNu ~/.vimreadrc"; 
      randomYoutube = "mpv (shuf /var/tmp/youtubeVideos)";
      "." ="cd ..";
      ".." = "cd ../..";
      "..." = "cd ../../..";
      "...." = "cd ../../../..";
      poweroff = "closeAllWindows; poweroff";
      reboot = "closeAllWindows; reboot";
      slock = "killall unclutter; slock; unclutter -grab &"; #with unclutter, slock dont work
      trans = "rlwrap trans"; # to use history in tranlation shell
      jupyter = "jupyter notebook ~/Dokumente/Uni/angewandteStatistik";
      # use zsh in enviroments
      xmonad-env = "load-env-xmonad-env zsh";
      compilerbau-env = "load-env-compilerbau-env zsh";
      writeScheme-env = "load-env-writeScheme-env zsh";
      idris-env = "load-env-idris-env zsh";
      python2-env = "load-env-python2-env zsh";
      python3-env = "load-env-python3-env zsh";
      r-env = "load-env-r-env zsh";
      # start gui programms in background
      spotify = ''zsh -c "spotify&"'';
      steam = ''zsh -c "steam&"'';
      anki =  ''zsh -c "anki&"'';
      netflix = ''zsh -c "google-chrome-stable "netflix.com"&"'';
      nix-shell = ''nix-shell --run zsh'';
    };
    shellInit = ''
      export VISUAL='vim'
      export LIBVIRT_DEFAULT_URI="qemu:///system"
      #TODO make it work (only works in ~/.zshrc) (
      ~/scripts/nextApts
      # play the youtube search list
      function mm() {
          mpv ytdl://ytsearch10:"$@"
      }
      function mma() {
          mpv --no-video ytdl://ytsearch10:"$@"
      }
      # )
    '';
    promptInit = ''
      if [ $SHELL != "/run/current-system/sw/bin/zsh" ] 
      then
        PROMPT='%K{green}%F{black} nix-shell %F{green}%K{black}$(echo "\ue0b0")%k%f'$PROMPT
      fi 
    '';
  };
  users.defaultUserShell = "/run/current-system/sw/bin/zsh";
}
