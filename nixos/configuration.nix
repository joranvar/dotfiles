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
    libreoffice
    (texlive.combine { inherit (texlive) scheme-tetex wrapfig capt-of; })

    fsharp
    mono
    czmq
    ruby
    bundler

    sqsh

    python35
#    python35Packages.pywinrm

    nox

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

    #steam
    androidsdk
    android-udev-rules
#    chromium

    pass
    yubikey-personalization
    yubikey-personalization-gui
    opensc

    gnome3.gvfs
    gnome3.nautilus
  ];

  nixpkgs.config.chromium = { enableWideVine = true; enablePepperFlash = true; };

  fonts = {
    enableFontDir = true;
    fonts = with pkgs; [
      terminus_font
    ];
  };

  # List services that you want to enable:

  # Enable the OpenSSH daemon.
  services.openssh.enable = true;

  services.upower.enable = true;

  services.dbus = {
    enable = true;
    packages = [
#        pkgs.gnome.GConf
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
    windowManager.xmonad = {
      enable = true;
      enableContribAndExtras = true;
    };
    synaptics = {
      enable = true;
      twoFingerScroll = true;
    };
    windowManager.default = "xmonad";
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
