{
  description = "schemar's system flake";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-26.05";

    nix-darwin.url = "github:nix-darwin/nix-darwin/nix-darwin-26.05";
    nix-darwin.inputs.nixpkgs.follows = "nixpkgs";

    home-manager.url = "github:nix-community/home-manager/release-26.05";
    home-manager.inputs.nixpkgs.follows = "nixpkgs";

    nixvim.url = "github:nix-community/nixvim/nixos-26.05";

    blueberry-peach.url = "github:schemar/blueberry-peach";

    disko.url = "github:nix-community/disko";
    disko.inputs.nixpkgs.follows = "nixpkgs";

    lanzaboote.url = "github:nix-community/lanzaboote/v1.0.0";
    lanzaboote.inputs.nixpkgs.follows = "nixpkgs";
  };

  outputs =
    inputs@{
      self,
      nix-darwin,
      nixpkgs,
      home-manager,
      nixvim,
      blueberry-peach,
      disko,
      lanzaboote,
    }:
    let
      username = "schemar";
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
          specialArgs = {
            inherit inputs username;
          };
          modules = [
            darwin
            ./hosts/Schencks-MacBook-Air
          ];
        };
      };
      nixosConfigurations = {
        "klabautermann" = nixpkgs.lib.nixosSystem {
          specialArgs = {
            inherit inputs username;
          };
          modules = [
            disko.nixosModules.disko
            ./hosts/klabautermann
          ];
        };
        "aegir" = nixpkgs.lib.nixosSystem {
          specialArgs = {
            inherit inputs username;
            isDarwin = false;
          };
          modules = [
            ./hosts/aegir
          ];
        };
        "nb0407" = nixpkgs.lib.nixosSystem {
          specialArgs = {
            inherit inputs username;
            isDarwin = false;
          };
          modules = [ ./hosts/nb0407 ];
        };
      };
      homeConfigurations = {
        "${username}@nb0400" = home-manager.lib.homeManagerConfiguration {
          pkgs = import nixpkgs {
            system = "x86_64-linux";
            config.allowUnfree = true;
          };
          extraSpecialArgs = {
            inherit inputs username;
            isDarwin = false;
          };
          modules = [
            ./hosts/nb0400
          ];
        };
      };
    };
}
