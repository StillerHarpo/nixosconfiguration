{ config, ... }:

{
  programs.zsh = {
    enable = true;
    enableAutosuggestions = true;
    # syntaxHighlighting.enable = true;
    oh-my-zsh = {
      enable = true;
      plugins = [ "vi-mode" "per-directory-history" "pass" ];
    };
    shellAliases = {
      ll = "ls -a";
      vimread = "vim -RNu ~/.vimreadrc";
      rtv = ''export BROWSER=linkopen; export EDITOR=vim; export PAGER=less;rtv'';
      randomYoutube = "mpv (shuf /var/tmp/youtubeVideos)";
      "." ="cd ..";
      ".." = "cd ../..";
      "..." = "cd ../../..";
      "...." = "cd ../../../..";
      # start gui programms in background
      spotify = ''zsh -c "(spotify&) >/tmp/spotify.log 2>&1"'';
      anki =  ''zsh -c "(anki&) >/tmp/anki.log 2>&1"'';
      steam =  ''zsh -c "(steam&) >/tmp/steam.log 2>&1"'';
      firefox =  ''zsh -c "(firefox&) >/tmp/firefox.log 2>&1"'';
      qutebrowser = ''zsh -c "(qutebrowser --target window&) >/tmp/qutebrowser.log 2>&1"'';
      netflix = ''zsh -c "(google-chrome-stable "netflix.com"&) >/tmp/google-chrome-table.log 2>&1"'';
      eclipse = ''zsh -c "(eclipse&) >/tmp/eclipse.log 2>&1"'';
      e = ''emacsclient -c'';
      nix-shell = ''nix-shell --run zsh'';
    };
    initExtra = ''
        export VISUAL='vim'
        export PATH=~/checkpad/checkpad-ssh:$PATH
      '';
  };
}
