{
  description = "schemar's system flake";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";

    nix-darwin.url = "github:nix-darwin/nix-darwin";
    nix-darwin.inputs.nixpkgs.follows = "nixpkgs";

    home-manager.url = "github:nix-community/home-manager";
    home-manager.inputs.nixpkgs.follows = "nixpkgs";

    blueberry-peach.url = "github:schemar/blueberry-peach";
  };

  outputs =
    inputs@{
      self,
      nix-darwin,
      nixpkgs,
      home-manager,
      blueberry-peach,
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
          specialArgs = { inherit inputs; };
          modules = [
            configuration
            home-manager.darwinModules.home-manager
            ./hosts/Schencks-MacBook-Air
          ];
        };
        "MacBook-Pro-0083" = nix-darwin.lib.darwinSystem {
          specialArgs = { inherit inputs; };
          modules = [
            configuration
            home-manager.darwinModules.home-manager
            ./hosts/MacBook-Pro-0083
          ];
        };
      };
    };
}
