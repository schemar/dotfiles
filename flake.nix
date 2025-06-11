{
  description = "schemar's system flake";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-25.05";

    nix-darwin.url = "github:nix-darwin/nix-darwin/nix-darwin-25.05";
    nix-darwin.inputs.nixpkgs.follows = "nixpkgs";

    home-manager.url = "github:nix-community/home-manager/release-25.05";
    home-manager.inputs.nixpkgs.follows = "nixpkgs";

    nixvim.url = "github:nix-community/nixvim/nixos-25.05";
    nixvim.inputs.nixpkgs.follows = "nixpkgs";

    blueberry-peach.url = "github:schemar/blueberry-peach";
  };

  outputs =
    inputs@{
      self,
      nix-darwin,
      nixpkgs,
      home-manager,
      nixvim,
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
        "Afilio-0083" = nix-darwin.lib.darwinSystem {
          specialArgs = { inherit inputs; };
          modules = [
            configuration
            home-manager.darwinModules.home-manager
            ./hosts/Afilio-0083
          ];
        };
      };
    };
}
