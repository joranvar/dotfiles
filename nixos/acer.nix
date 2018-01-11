# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, ... }:

{
  imports = [ ];

  # Use the gummiboot efi boot loader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  boot.extraModprobeConfig = ''
    # options snd_hda_intel enable=0,1
    # options i915 modeset=1 i915_enable_rc6=7 i915_enable_fbc=1 lvds_downclock=1
    # options snd slots=snd-hda-intel
    # options nvidia-drm modeset=1
    options snd-hda-intel index=0 id=PCH
    options snd-hda-intel index=1 id=HDMI
  '';
  boot.blacklistedKernelModules = [ "snd_pcsp" ];
  boot.kernelModules = [ "intel_agp" "i915" ];
  hardware.pulseaudio.enable = true;

  hardware.opengl.driSupport32Bit = true;

  hardware.enableAllFirmware = true;

  networking.hostName = "lapbart"; # Define your hostname.
  networking.hostId = "3d14756b";
  # networking.wireless.enable = true;  # Enables wireless.
  networking.firewall.enable = true;
  networking.firewall.allowPing = true;
  networking.firewall.allowedTCPPorts = [ 445 139 ];
  networking.firewall.allowedUDPPorts = [ 137 138 ];


  # Enable the X11 windowing system.
  services.xserver = {
    videoDrivers = [ "nouveau" ];
    xrandrHeads = [ "HDMI1" "eDP1" ];
    resolutions = [ { x = 2560; y = 1440; } { x = 1920; y = 1080; } ];
  };

  # Power buttons.
  services.logind.extraConfig = ''
    HandlePowerKey=hibernate
    PowerKeyIgnoreInhibited=yes
  '';

  powerManagement.powerUpCommands = ''
    # Force ethernet
    ${pkgs.iproute}/bin/ip link set enp3s0 down
    ${pkgs.iproute}/bin/ip link set enp3s0 up
  '';

  virtualisation.libvirtd.enable = true;
  virtualisation.virtualbox.host.enable = true;
}
