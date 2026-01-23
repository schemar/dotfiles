{
  description = "schemar's system flake";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs";

    nix-darwin.url = "github:nix-darwin/nix-darwin";
    nix-darwin.inputs.nixpkgs.follows = "nixpkgs";

    home-manager.url = "github:nix-community/home-manager";
    home-manager.inputs.nixpkgs.follows = "nixpkgs";

    nixvim.url = "github:nix-community/nixvim";
    # Remove the following lane in case of nixvim errors.
    # Their own nixpkgs are usually tested.
    # A working nixvim with a mismatch in programgs/libs is ok in that case.
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
            ./hosts/Schencks-MacBook-Air
          ];
        };
        "Afilio-0083" = nix-darwin.lib.darwinSystem {
          specialArgs = { inherit inputs; };
          modules = [
            darwin
            ./hosts/Afilio-0083
          ];
        };
      };
      nixosConfigurations = {
        "klabautermann" = nixpkgs.lib.nixosSystem {
          specialArgs = { inherit inputs; };
          modules = [
            ./hosts/klabautermann
          ];
        };
      };
      homeConfigurations = {
        "schemar@schenck-debian" = home-manager.lib.homeManagerConfiguration {
          pkgs = import nixpkgs { system = "x86_64-linux"; };
          extraSpecialArgs = {
            inherit inputs;
            isDarwin = false;
            npmAlias = null;
          };
          modules = [
            ./home/schemar/standalone.nix
          ];
        };
      };
    };
}
