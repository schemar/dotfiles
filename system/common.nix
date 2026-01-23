{ pkgs, inputs, ... }:
{
  # Necessary for using flakes on this system.
  nix.settings.experimental-features = "nix-command flakes";

  # List packages installed in system profile. To search by name, run:
  # $ nix-env -qaP | grep wget
  environment.systemPackages = with pkgs; [
    coreutils-prefixed
    nix
    zsh
  ];

  # Use user-level ZSH provided by home-manager config:
  environment.shells = [ "/run/current-system/sw/bin/zsh" ];

  # Configure user shell (both NixOS and nix-darwin):
  users.users.schemar.shell = "/run/current-system/sw/bin/zsh";

  fonts.packages = with pkgs; [
    monaspace
    nerd-fonts.symbols-only
  ];

  # Include unfree packages from nixpkgs:
  nixpkgs.config.allowUnfree = true;

  # Home Manager module configuration:
  home-manager = {
    useGlobalPkgs = true;
    useUserPackages = true;

    extraSpecialArgs = {
      inherit inputs;
      isDarwin = pkgs.stdenv.hostPlatform.isDarwin;
    };
  };
}
