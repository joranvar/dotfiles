# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, ... }:

{
  imports =
    [ # Include the results of the hardware scan.
      /etc/nixos/hardware-configuration.nix
    ];

  # Use the gummiboot efi boot loader.
  boot.loader.gummiboot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  boot.extraModprobeConfig = ''
    options snd_hda_intel enable=0,1
  '';
  boot.blacklistedKernelModules = [ "snd_pcsp" ];

  networking.hostName = "lapbart"; # Define your hostname.
  networking.hostId = "3d14756b";
  # networking.wireless.enable = true;  # Enables wireless.

  # Select internationalisation properties.
  # i18n = {
  #   consoleFont = "lat9w-16";
  #   consoleKeyMap = "us";
  #   defaultLocale = "en_US.UTF-8";
  # };

  # List packages installed in system profile. To search by name, run:
  # $ nix-env -qaP | grep wget
  environment.systemPackages = with pkgs; [
    wget
    file

    zsh
    git
    gnupg
    htop

    emacs
    emacs24Packages.magit
    emacs24Packages.haskellMode
    networkmanagerapplet

    firefox
    rxvt_unicode_with-plugins

    xlsfonts

    trayer
    haskellPackages.xmobar
    haskellPackages.xmonad
    haskellPackages.xmonadContrib
    haskellPackages.xmonadExtras

    dmenu
    xscreensaver

    gnome3.gvfs
    gnome3.nautilus
  ];

  fonts = {
    enableFontDir = true;
    fonts = with pkgs; [
      terminus_font
    ];
  };

  # List services that you want to enable:

  # Enable the OpenSSH daemon.
  services.openssh.enable = true;

  # Enable CUPS to print documents.
  # services.printing.enable = true;

  # Enable the X11 windowing system.
  services.xserver = {
    enable = true;
    windowManager.xmonad = {
      enable = true;
      enableContribAndExtras = true;
    };
    synaptics = {
      enable = true;
      twoFingerScroll = true;
    };
    xrandrHeads = [ "HDMI1" "eDP1" ];
    resolutions = [ { x = 2560; y = 1440; } { x = 1920; y = 1080; } ];
    windowManager.default = "xmonad";
    desktopManager.xterm.enable = false;
    desktopManager.default = "none";
    displayManager.slim = {
      enable = true;
      defaultUser = "joranvar";
    };
  };

  programs.ssh.startAgent = true;

  services.nixosManual.showManual = true;

  # Enable the KDE Desktop Environment.
  # services.xserver.displayManager.kdm.enable = true;
  # services.xserver.desktopManager.kde4.enable = true;

  # Define a user account. Don't forget to set a password with ‘passwd’.
  users.extraUsers.joranvar = {
    isNormalUser = true;
    createHome = true;
    home = "/home/joranvar";
    extraGroups = [ "wheel" "disk" "cdrom" "networkmanager" ];
    useDefaultShell = true;
    uid = 1000;
  };
  networking.networkmanager.enable = true;
  security.sudo.enable = true;
  users.defaultUserShell = "/var/run/current-system/sw/bin/zsh";
}
