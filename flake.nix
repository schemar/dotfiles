{
  description = "schemar's system flake";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-24.11-darwin";

    nix-darwin.url = "github:nix-darwin/nix-darwin/nix-darwin-24.11";
    nix-darwin.inputs.nixpkgs.follows = "nixpkgs";

    home-manager.url = "https://github.com/nix-community/home-manager/archive/release-24.11.tar.gz";
    home-manager.inputs.nixpkgs.follows = "nixpkgs";
  };

  outputs = inputs@{ self, nix-darwin, nixpkgs, home-manager }:
    let
      configuration = { pkgs, ... }: {
        # List packages installed in system profile. To search by name, run:
        # $ nix-env -qaP | grep wget
        environment.systemPackages = with pkgs; [];

        # Necessary for using flakes on this system.
        nix.settings.experimental-features = "nix-command flakes";

        # Enable alternative shell support in nix-darwin.
        # programs.fish.enable = true;

        # Set Git commit hash for darwin-version.
        system.configurationRevision = self.rev or self.dirtyRev or null;

        # Used for backwards compatibility, please read the changelog before changing.
        # $ darwin-rebuild changelog
        system.stateVersion = 5;
      };
      macConfiguration = {
        # The platform the configuration will be used on.
        nixpkgs.hostPlatform = "aarch64-darwin";

        # Add ability to use TouchID for sudo authentication in terminal
        security.pam.enableSudoTouchIdAuth = true;

        # Required by home-manager:
        users.users.schemar.home = /Users/schemar;
        home-manager = {
          useGlobalPkgs = true;
          useUserPackages = true;

          users.schemar = ./users/schemar;
        };
      };
    in
    {
      # Build darwin flake using:
      # $ darwin-rebuild switch --flake .
      darwinConfigurations = {
        "Schencks-MacBook-Air" = nix-darwin.lib.darwinSystem {
          system = "aarch64-darwin";
          modules = [
            home-manager.darwinModules.home-manager
            configuration
            macConfiguration
            ./hosts/Schencks-MacBook-Air
          ];
        };
        "MacBook-Pro-0083" = nix-darwin.lib.darwinSystem {
          system = "aarch64-darwin";
          modules = [
            home-manager.darwinModules.home-manager
            configuration
            macConfiguration
            ./hosts/MacBook-Pro-0083
          ];
        };
      };
    };
}
