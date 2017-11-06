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
    # factorio = with pkgs; import /home/joranvar/git/nixpkgs/pkgs/games/factorio/default.nix ({
    #   stdenv = stdenv;
    #   callPackage = callPackage;
    #   fetchurl = fetchurl;
    #   makeWrapper = makeWrapper;
    #   alsaLib = alsaLib;
    #   libX11 = xlibs.libX11;
    #   libXcursor = xlibs.libXcursor;
    #   libXinerama = xlibs.libXinerama;
    #   libXrandr = xlibs.libXrandr;
    #   libXi = xlibs.libXi;
    #   mesa_noglu = mesa_noglu;
    #   factorio-utils = factorio-utils;
    #   releaseType = "alpha"; });
  };

  # List packages installed in system profile. To search by name, run:
  # $ nix-env -qaP | grep wget
  environment.systemPackages = with pkgs; [
    wget
    file
    silver-searcher

    zsh
    git
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

    emacs
    aspell
    aspellDicts.en
    aspellDicts.nl
    antiword
    pandoc
    gv
    ghostscript
    libreoffice
    (texlive.combine { inherit (texlive) scheme-tetex wrapfig capt-of minted ifplatform xstring framed; })
    python35Packages.pygments

    fsharp
    mono
    czmq
    ruby
    bundler

    sqsh
    freetds
    unixODBC
    mysql
    mysql-workbench

    python35
#    python35Packages.pywinrm

    nox
    ctags

    networkmanagerapplet
    gkrellm
    gimp

    firefox
    conkeror
    rxvt_unicode_with-plugins
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
    taffybar
    haskellPackages.xmobar
    haskellPackages.xmonad
    haskellPackages.xmonad-contrib
    haskellPackages.xmonad-extras
    haskellPackages.yeganesh
    docker # For installing ghc-android - https://github.com/iphydf/docker-build-ghc-android
    libnotify
    notify-osd
    compton
    mpd
    scrot
    nitrogen

#    virtmanager
    linuxPackages.virtualbox
    vagrant

    freerdp
    remmina
    x2vnc
    tigervnc
    redshift
    davmail
    notmuch
    offlineimap

    dmenu
    dzen2
    xscreensaver
    maim
    samba
    flashplayer
    pidgin
    #skype
    #gitAndTools.git-annex
    zip
    unzip
    ledger
    gnuplot
    plantuml
    graphviz

    #steam
    androidsdk
    android-udev-rules
#    chromium

    pass
    lastpass-cli
    yubikey-personalization
    yubikey-personalization-gui
    opensc

    gnome3.gvfs
    gnome3.nautilus

#    factorio
  ];

#  nixpkgs.config.chromium = { enableWideVine = true; enablePepperFlash = true; };

  fonts = {
    enableFontDir = true;
    fonts = with pkgs; [
      terminus_font
      hack-font
      symbola
    ];
  };

  # List services that you want to enable:

  # Enable the OpenSSH daemon.
  services.openssh.enable = true;
  services.openssh.passwordAuthentication = false;

  services.upower.enable = true;

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

  time.timeZone = "Europe/Amsterdam";
  programs.ssh.startAgent = false; # gpg agent takes over this role

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

  #  FIDO YubiKey
  services.pcscd.enable = true;
  services.udev.extraRules = ''
     KERNEL=="hidraw*", SUBSYSTEM=="hidraw", ATTRS{idVendor}=="1050", ATTRS{idProduct}=="0111|0113|0114|0115|0116|0120", ENV{ID_SMARTCARD_READER}="1"
  '';

  programs.zsh.enable = true;
  users.defaultUserShell = "/var/run/current-system/sw/bin/zsh";
}
