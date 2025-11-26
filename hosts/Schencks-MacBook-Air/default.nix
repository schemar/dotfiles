{ ... }:
{
  imports = [
    ../common.nix
    ../darwin.nix
    ../../users/schemar/common.nix
    ../../users/schemar/darwin.nix
  ];

  home-manager.extraSpecialArgs.npmAlias = null;
}
