{
  pkgs,
  inputs,
  username,
  ...
}:
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
  programs.zsh.enable = true;

  # Use user-level ZSH provided by home-manager config:
  environment.shells = [ "/run/current-system/sw/bin/zsh" ];

  users.users.${username} = {
    # Configure user shell (both NixOS and nix-darwin):
    shell = "/run/current-system/sw/bin/zsh";
    # Required by home-manager:
    home = if pkgs.stdenv.hostPlatform.isDarwin then "/Users/${username}" else "/home/${username}";
  };

  fonts.packages = [
    pkgs.lato
    pkgs.monaspace
    pkgs.nerd-fonts.symbols-only
    pkgs.noto-fonts-color-emoji
    pkgs.open-sans
    pkgs.source-serif

    inputs.private-fonts.packages.x86_64-linux.default
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
