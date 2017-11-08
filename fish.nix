{ config, ... }:

{
  programs.fish = {
    enable = true;
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
      # use fish in enviroments
      xmonad-env = "load-env-xmonad-env fish";
      python2-env = "load-env-python2-env fish";
      python3-env = "load-env-python3-env fish";
      r-env = "load-env-r-env fish";
      comilerbau-env = "load-env-compilerbau-env fish";
      # start gui programms in background
      spotify = ''fish -c "spotify&"'';
      steam = ''fish -c "steam&"'';
      anki =  ''fish -c "anki&"'';
      netlix = ''fish -c "google-chrome-stable "netflix.com"&"'';
    };
    shellInit = ''
      export VISUAL='vim'
      export LIBVIRT_DEFAULT_URI="qemu:///system"
      ~/sripts/nextApts
      fis_vi_key_bindings
      # play the youtube search list
      function mm 
        mpv ytdl://ytsearch10:$argv
      end 
      function mma
          mpv --no-video ytdl://ytsearch10:$argv
      end
    '';
  };
  users.defaultUserShell = "/run/current-system/sw/bin/fish"
};
