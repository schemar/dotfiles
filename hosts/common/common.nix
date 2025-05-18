{ pkgs, ... }:
{
  # List packages installed in system profile. To search by name, run:
  # $ nix-env -qaP | grep wget
  environment.systemPackages = with pkgs; [ zsh ];

  # Use user-level ZSH provided by home-manager config:
  environment.shells = [ "/run/current-system/sw/bin/zsh" ];

  # Include unfree packages from nixpkgs:
  nixpkgs.config.allowUnfree = true;
}
