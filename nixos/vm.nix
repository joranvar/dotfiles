# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, ... }:

{
  imports = [ ];

  # Use the GRUB 2 boot loader.
  boot.loader.grub.enable = true;
  boot.loader.grub.version = 2;
  boot.loader.grub.device = "/dev/sda";
  boot.initrd.checkJournalingFS = false;

  # networking.hostName = "vm"; # Define your hostname.
  # networking.wireless.enable = true;  # Enables wireless.

  virtualisation.virtualbox.guest.enable = true;

  system.stateVersion = "15.09";
}
