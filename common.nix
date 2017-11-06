# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, ... }:

{
  imports = [ ./enviroments.nix ];
  boot = { 
    resumeDevice = "/dev/disk/by-label/swap";
    initrd.postDeviceCommands = "sleep 5";

  };

  # networking.hostName = "nixos"; # Define your hostname.

  # Select internationalisation properties.
  i18n = {
    consoleFont = "Lat2-Terminus16";
    consoleKeyMap = "us";
    defaultLocale = "en_US.UTF-8";
  };

  # Set your time zone.
  time.timeZone = "Europe/Berlin";

  # List packages installed in system profile. To search by name, run:
  # $ nix-env -qaP | grep wget
  
  environment.systemPackages = with pkgs; ([
    wget
    (import ./vim.nix)
#   (import ./emacs.nix)
    sudo
    firefox
    git
    pass
    gnupg
    termite
    mutt
    termite
    toxic
    rtv
    xsel
    zathura
    spotify
    mpv
    rlwrap
    translate-shell
    you-get
    xosd
    pandoc
    (texlive.combine {inherit (texlive) scheme-full pygmentex pgf collection-basic;})
    stack
    unclutter
    #(unstable.steam.override { newStdcpp = true; })
    steam
    bc
    anki
    cabal2nix
    cabal-install 
    nix-prefetch-git
    calcurse 
    google-chrome
    torbrowser
    dunst
    libnotify
  ]);

  nixpkgs.config = 
  {
    # Allow proprietary packages
    allowUnfree = true;

    # Create an alias for the unstable channel
    packageOverrides = pkgs: 
    {
      unstable = import <nixos-unstable> 
      { 
        # pass the nixpkgs config to the unstable alias
        # to ensure `allowUnfree = true;` is propagated:
        config = config.nixpkgs.config; 
      };
    };
  };

  fonts = {
    enableFontDir = true;
    fonts = with pkgs; [
      terminus_font
      dejavu_fonts
    ];
  };

  # List services that you want to enable:

  # Enable the OpenSSH daemon.
  # services.openssh.enable = true;

  # Enable CUPS to print documents.
  # services.printing.enable = true;
   
  services = {
    # Enable the X11 windowing system.
    xserver = {
      enable = true;
      layout = "us,de";
      xkbOptions = "eurosign:e, caps:escape, grp:alt_shift_toggle";
      monitorSection = ''Option "DPMS" "false"'';
      serverLayoutSection =
      ''
        Option          "BlankTime"     "0"
        Option          "StandbyTime"   "0"
        Option          "SuspendTime"   "0"
        Option          "OffTime"       "0"
     '';
      # Enable XMonad
      windowManager.xmonad = {
        enable = true;
        enableContribAndExtras = true;
      };
    }; 
    emacs = {
      enable=true;
      package = import ./emacs.nix;
    };
    hoogle.enable = true;
    postgresql = {
      enable = true;
      package = pkgs.postgresql100;
    };
  }; 
  programs = {
    # fish 
    fish = {
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
	compilerbau-env = "load-env-compilerbau-env fish";
	# start gui programms in background
	spotify = ''fish -c "spotify&"'';
	steam = ''fish -c "steam&"'';
	anki =  ''fish -c "anki&"'';
	netflix = ''fish -c "google-chrome-stable "netflix.com"&"'';
      };
      shellInit = ''
        export VISUAL='vim'
        export LIBVIRT_DEFAULT_URI="qemu:///system"
	~/scripts/nextApts
	fish_vi_key_bindings
        # play the youtube search list
        function mm 
          mpv ytdl://ytsearch10:$argv
        end 
        function mma
            mpv --no-video ytdl://ytsearch10:$argv
	end
      '';
    };
  };
  users = {  
    defaultUserShell = "/run/current-system/sw/bin/fish";

    # Define a user account. Don't forget to set a password with ‘passwd’.
    extraUsers.florian= {
      isNormalUser = true;
      uid = 1000;
      createHome = true;
      extraGroups = [ "wheel" "networmanager" "audio" "docker" ];
    };
  };

  # passwordless sudo
  security.sudo.wheelNeedsPassword = false;

  # docker
  virtualisation.docker.enable=true;

  hardware = {
    pulseaudio = { 
      enable = true; 
      package = pkgs.pulseaudioFull;
      support32Bit = true;
    }; 
    opengl.driSupport32Bit = true;  
  }; 

  # The NixOS release to be compatible with for stateful data such as databases.
  system.stateVersion = "17.09";
}
