{ config, ... }:

{
  programs.zsh = {
    enable = true;
    autosuggestions.enable = true;
    syntaxHighlighting.enable = true;
    ohMyZsh = {
      enable = true;
      theme = "agnoster";
      plugins = [ "vi-mode" "per-directory-history" "pass" "nix" ];
    };
    shellAliases = {
#     emacs = "emacsclient -c";
      ls = "ls -lh --color=auto";
      ll = "ls -a";
      srg = "sr google";
      srw = "sr wikipedia";
      rtv = ''export BROWSER=linkopen; export EDITOR=vim; export PAGER=less;rtv'';
      vimread = "vim -RNu ~/.vimreadrc";
      randomYoutube = "mpv (shuf /var/tmp/youtubeVideos)";
      "." ="cd ..";
      ".." = "cd ../..";
      "..." = "cd ../../..";
      "...." = "cd ../../../..";
      poweroff = "closeAllWindows; poweroff";
      reboot = "closeAllWindows; reboot";
      trans = "rlwrap trans"; # to use history in tranlation shell
      # start gui programms in background
      spotify = ''zsh -c "(spotify&) >/tmp/spotify.log 2>&1"'';
      anki =  ''zsh -c "(anki&) >/tmp/anki.log 2>&1"'';
      steam =  ''zsh -c "(steam&) >/tmp/steam.log 2>&1"'';
      firefox =  ''zsh -c "(firefox&) >/tmp/firefox.log 2>&1"'';
      qutebrowser = ''zsh -c "(qutebrowser --target window&) >/tmp/qutebrowser.log 2>&1"'';
      netflix = ''zsh -c "(google-chrome-stable "netflix.com"&) >/tmp/google-chrome-table.log 2>&1"'';
      eclipse = ''zsh -c "(eclipse&) >/tmp/eclipse.log 2>&1"'';
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
      eval "$(direnv hook zsh)"
    '';
    promptInit = ''
      WORKSPACE=$(wmctrl -d | grep "*" | cut -f1 -d' ')
      if [ $SHELL != "/run/current-system/sw/bin/zsh" ]
      then
        PROMPT='%K{green}%F{black} nix-shell %F{green}%K{black}$(echo "\ue0b0")%k%f'$PROMPT
      else
        cd $(grep -e "^''${WORKSPACE} .*" ~/scripts/var/roots | cut -f2 -d' ')
      fi
    '';
  };
  users.defaultUserShell = "/run/current-system/sw/bin/zsh";
}
