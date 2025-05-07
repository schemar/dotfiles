{
  description = "schemar's system flake";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";

    nix-darwin.url = "github:nix-darwin/nix-darwin";
    nix-darwin.inputs.nixpkgs.follows = "nixpkgs";

    home-manager.url = "github:nix-community/home-manager";
    home-manager.inputs.nixpkgs.follows = "nixpkgs";
  };

  outputs =
    inputs@{
      self,
      nix-darwin,
      nixpkgs,
      home-manager,
    }:
    let
      configuration =
        { pkgs, ... }:
        {
          # Necessary for using flakes on this system.
          nix.settings.experimental-features = "nix-command flakes";

          # Set Git commit hash for darwin-version.
          system.configurationRevision = self.rev or self.dirtyRev or null;

          # Used for backwards compatibility, please read the changelog before changing.
          # $ darwin-rebuild changelog
          system.stateVersion = 5;
        };
    in
    {
      # Build darwin flake using:
      # $ darwin-rebuild switch --flake .
      darwinConfigurations = {
        "Schencks-MacBook-Air" = nix-darwin.lib.darwinSystem {
          system = "aarch64-darwin";
          modules = [
            configuration
            ./hosts/Schencks-MacBook-Air
            home-manager.darwinModules.home-manager
          ];
        };
        "MacBook-Pro-0083" = nix-darwin.lib.darwinSystem {
          system = "aarch64-darwin";
          modules = [
            configuration
            ./hosts/MacBook-Pro-0083
            home-manager.darwinModules.home-manager
          ];
        };
      };
    };
}
