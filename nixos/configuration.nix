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
  hardware.pulseaudio.enable = true;

  hardware.opengl.driSupport32Bit = true;

  networking.hostName = "lapbart"; # Define your hostname.
  networking.hostId = "3d14756b";
  # networking.wireless.enable = true;  # Enables wireless.
  networking.firewall.enable = true;
  networking.firewall.allowPing = true;
  networking.firewall.allowedTCPPorts = [ 445 139 ];
  networking.firewall.allowedUDPPorts = [ 137 138 ];

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

    python34
    python34Packages.pywinrm

    nox

    networkmanagerapplet

    firefox
    rxvt_unicode_with-plugins
    vlc
    sshfsFuse

    xlsfonts

    trayer
    haskellPackages.xmobar
    haskellPackages.xmonad
    haskellPackages.xmonad-contrib
    haskellPackages.xmonad-extras

    virtmanager

    dmenu
    xscreensaver
    samba
    java
    flashplayer
    pidgin

    pass
    yubikey-personalization
    yubikey-personalization-gui
    opensc

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

  services.dbus = {
    enable = true;
    packages = [
        pkgs.libvirt
        pkgs.virtmanager
        pkgs.gnome.GConf
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
    videoDrivers = [ "intel" ];
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

  time.timeZone = "Europe/Amsterdam";
  services.xserver.startGnuPGAgent = true;
  programs.ssh.startAgent = false; # gpg agent takes over this role

  services.nixosManual.showManual = true;

  # Enable the KDE Desktop Environment.
  # services.xserver.displayManager.kdm.enable = true;
  # services.xserver.desktopManager.kde4.enable = true;

  # Define a user account. Don't forget to set a password with ‘passwd’.
  users.extraUsers.joranvar = {
    isNormalUser = true;
    createHome = true;
    home = "/home/joranvar";
    extraGroups = [ "wheel" "disk" "cdrom" "networkmanager" "audio" "libvirtd" ];
    useDefaultShell = true;
    uid = 1000;
  };
  networking.networkmanager.enable = true;
  security.sudo.enable = true;

  #  FIDO YubiKey
  services.udev.extraRules = ''
     KERNEL=="hidraw*", SUBSYSTEM=="hidraw", ATTRS{idVendor}=="1050", ATTRS{idProduct}=="0111|0113|0114|0115|0116|0120", ENV{ID_SMARTCARD_READER}="1"
  '';
  users.defaultUserShell = "/var/run/current-system/sw/bin/zsh";

  virtualisation.libvirtd.enable = true;
}

