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
      darwin = {
        # Set Git commit hash for darwin-version.
        system.configurationRevision = self.rev or self.dirtyRev or null;
      };
    in
    {
      # Build darwin flake using:
      # $ darwin-rebuild switch --flake .
      darwinConfigurations = {
        "Schencks-MacBook-Air" = nix-darwin.lib.darwinSystem {
          specialArgs = { inherit inputs; };
          modules = [
            darwin
            home-manager.darwinModules.home-manager
            ./hosts/Schencks-MacBook-Air
          ];
        };
        "Afilio-0083" = nix-darwin.lib.darwinSystem {
          specialArgs = { inherit inputs; };
          modules = [
            darwin
            home-manager.darwinModules.home-manager
            ./hosts/Afilio-0083
          ];
        };
      };
      nixosConfigurations = {
        "klabautermann" = nixpkgs.lib.nixosSystem {
          specialArgs = { inherit inputs; };
          modules = [
            home-manager.nixosModules.home-manager
            ./hosts/klabautermann
          ];
        };
      };
    };
}
