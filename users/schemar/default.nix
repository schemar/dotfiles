{ pkgs, ... }:
{
  users.users.schemar.shell = "/run/current-system/sw/bin/zsh";

  home-manager = {
    useGlobalPkgs = true;
    useUserPackages = true;

    users.schemar = ./home.nix;

    extraSpecialArgs = {
      isDarwin = pkgs.stdenv.hostPlatform.isDarwin;
    };
  };
}
