{ pkgs, inputs, ... }:
{
  users.users.schemar.shell = "/run/current-system/sw/bin/zsh";
  system.primaryUser = "schemar";

  home-manager = {
    useGlobalPkgs = true;
    useUserPackages = true;

    users.schemar = ./home.nix;

    extraSpecialArgs = {
      isDarwin = pkgs.stdenv.hostPlatform.isDarwin;
      inherit inputs;
    };
  };
}
