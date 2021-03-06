# -*- compile-command: "sudo nixos-rebuild switch"; -*-

# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, ... }:

{
  imports =
    [ # Include the results of the hardware scan.
      /etc/nixos/hardware-configuration.nix
      # Include the local machine configuration.
      /etc/nixos/local-machine.nix
      /etc/nixos/yubikey-gpg.nix
    ];

  system.autoUpgrade.enable = true;

  # Select internationalisation properties.
  # i18n = {
  #   consoleFont = "lat9w-16";
  #   consoleKeyMap = "us";
  #   defaultLocale = "en_US.UTF-8";
  # };
  nixpkgs.config.packageOverrides = pkgs : rec {
    xmonad = pkgs.haskellPackages.xmonad;
    xmonad-contrib = pkgs.haskellPackages.xmonad-contrib;
    xmonad-extras = pkgs.haskellPackages.xmonad-extras;
    freetds = pkgs.freetds.override { odbcSupport = true; };
    factorio = with pkgs; import /home/joranvar/git/nixos/nixpkgs/pkgs/games/factorio/default.nix ({
      stdenv = stdenv;
      callPackage = callPackage;
      fetchurl = fetchurl;
      makeWrapper = makeWrapper;
      alsaLib = alsaLib;
      libX11 = xlibs.libX11;
      libXcursor = xlibs.libXcursor;
      libXinerama = xlibs.libXinerama;
      libXrandr = xlibs.libXrandr;
      libXi = xlibs.libXi;
      mesa_noglu = mesa_noglu;
      factorio-utils = factorio-utils;
      releaseType = "alpha";
    });
#    conkeror = pkgs.conkeror-unwrapped.override { firefox = pkgs.firefox-esr; };
    composer2nix = (import (pkgs.fetchFromGitHub {
      owner = "svanderburg";
      repo = "composer2nix";
      "rev" = "2f130084b545992954567d758039bf65698e1603";
      "sha256" = "09ss6lzkyx77py7kh5r3sksjyb2871zi8088bqafhi80sa3m4fl1";
    }) {});
    nixops = pkgs.nixops.overrideDerivation (
      old: {
        patchPhase = ''
          substituteInPlace nix/eval-machine-info.nix \
          --replace 'system.nixosVersion' 'system.nixos.version'
          substituteInPlace nix/ec2.nix \
          --replace 'system.nixosVersion' 'system.nixos.version'
        '';
      }
    );
  };

  # List packages installed in system profile. To search by name, run:
  # $ nix-env -qaP | grep wget
  environment.systemPackages = with pkgs; [
    wget
    file
    silver-searcher

    zsh
    git
    gitAndTools.hub
    jre
    libusb
    gnupg
    gnupg1compat
    htop
    lsof
    usbutils
    binutils
    gnumake
    gcc
    gdb

    cabal-install
    cabal2nix
    dhall

    emacs
    aspell
    aspellDicts.en
    aspellDicts.nl
    antiword
    pandoc
    gv
    ghostscript
    libreoffice
    (texlive.combine {
      inherit (texlive) scheme-tetex wrapfig capt-of minted ifplatform xstring framed fvextra upquote; })
    python35Packages.pygments

#    fsharp
#    mono
#    czmq
#    ruby
#    bundler

    sqsh
    freetds
    unixODBC
    mysql
#    mysql-workbench

    python35
#    python35Packages.pywinrm

    nox
    ctags

    networkmanagerapplet
#    gkrellm
    gimp

    firefox
    conkeror
#    rxvt_unicode_with-plugins
    termite
    vlc
    sshfsFuse
    cifs_utils
    s3fs
    pavucontrol

    xlsfonts
    libxml2

    trayer
    haskellPackages.ghc
    haskellPackages.ghcid
    taffybar
    haskellPackages.hindent
    haskellPackages.hlint
    haskellPackages.hasktags
    haskellPackages.xmobar
    haskellPackages.xmonad
    haskellPackages.xmonad-contrib
    haskellPackages.xmonad-extras
    haskellPackages.yeganesh
    docker # For installing ghc-android - https://github.com/iphydf/docker-build-ghc-android

    composer2nix
    nodePackages.node2nix
    nixops

    libnotify
    notify-osd
    compton
    mpd
    scrot
    nitrogen

    traceroute

#    virtmanager
    linuxPackages.virtualbox
    vagrant
#    ue4
    #teamviewer

#    freerdp
#    remmina
#    x2vnc
#    tigervnc
    redshift
    davmail
    notmuch
    offlineimap

#    dmenu
#    dzen2
    xscreensaver
    xorg.xbacklight
    maim
    samba
#    flashplayer
    pidgin
    #skype
    #gitAndTools.git-annex
    zip
    unzip
    hledger
    gnuplot
    plantuml
    graphviz
#    taskjuggler

    steam
    androidsdk
    android-udev-rules
    chromium

    pass
    lastpass-cli
    opensc

    gnome3.gvfs
    gnome3.nautilus

#    factorio
  ];

  nixpkgs.config.chromium = { enableWideVine = true; enablePepperFlash = true; };

  fonts = {
    enableFontDir = true;
    fonts = with pkgs; [
      terminus_font
      hack-font
      source-code-pro
      symbola
    ];
  };

  # List services that you want to enable:

  # Enable the OpenSSH daemon.
  services.openssh.enable = true;
  services.openssh.passwordAuthentication = false;

  services.upower.enable = true;
  services.printing = {
    enable = true;
    drivers = [ pkgs.gutenprint pkgs.gutenprintBin pkgs.brgenml1cupswrapper ];
  };

  services.dbus = {
    enable = true;
    packages = [
        pkgs.gnome3.gconf.out
    ];
  };

  # Enable CUPS to print documents.
  # services.printing.enable = true;

  # Enable Samba.
  services.samba = {
    enable = true;
    shares = {
      devenv = {
        path = "/home/joranvar/git/";
        "read only" = "no";
        browseable = "yes";
        "guest ok" = "no";
        "valid users" = "joranvar";
        extraConfig = ''
          guest account = nobody
          map to guest = bad user
        '';
      };
    };
  };

  services.avahi = {
    enable = true;
    ipv4 = true;
    ipv6 = true;
  };

  nixpkgs.config.allowUnfree = true;

  # Enable the X11 windowing system.
  services.xserver = {
    enable = true;
    xkbOptions = "compose:ralt";
    windowManager.exwm = {
      enable = true;
      enableDefaultConfig = false;
      extraPackages = e: [
        e.emms
        e.magit
        e.structured-haskell-mode
        e.hindent
      ];
    };
    synaptics = {
      enable = true;
      twoFingerScroll = true;
    };
    windowManager.default = "exwm";
    desktopManager.xterm.enable = false;
    desktopManager.default = "none";
    displayManager.slim = {
      enable = true;
      defaultUser = "joranvar";
    };
  };

  environment.etc."odbcinst.ini".text = "[FreeTDS]\nDriver=${pkgs.freetds}/lib/libtdsodbc.so\n";

  time.timeZone = "Europe/Amsterdam";

  services.teamviewer.enable = false;

  services.nixosManual.showManual = true;
  virtualisation.docker.enable = true;

  # Define a user account. Don't forget to set a password with ‘passwd’.
  users.extraUsers.joranvar = {
    isNormalUser = true;
    createHome = true;
    home = "/home/joranvar";
    extraGroups = [ "wheel" "disk" "cdrom" "networkmanager" "audio" "libvirtd" "docker" ];
    useDefaultShell = true;
    uid = 1000;
  };
  networking.networkmanager.enable = true;
  security.sudo.enable = true;


  programs.browserpass.enable = true;
  programs.zsh.enable = true;
  users.defaultUserShell = "/var/run/current-system/sw/bin/zsh";
}
