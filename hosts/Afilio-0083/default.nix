{ inputs, pkgs, ... }:
let
  gdk = pkgs.google-cloud-sdk.withExtraComponents (
    with pkgs.google-cloud-sdk.components;
    [
      kubectl
    ]
  );
in
{
  # Host configuration for Afilio-0083 (nix-darwin)

  imports = [
    # System-level configurations:
    ../../system/common.nix
    ../../system/darwin.nix

    # Home-manager as a nix-darwin module:
    inputs.home-manager.darwinModules.home-manager
  ];

  # Configure home-manager to use the schemar user config:
  home-manager.users.schemar = {
    imports = [
      ../../home/schemar
    ];
  };

  # Host-specific overrides:
  # Use `\\npm` or `env npm` to prevent using the alias when installing sfw.
  home-manager.extraSpecialArgs.npmAlias = "sfw npm";

  environment.systemPackages = [
    gdk
    pkgs.temporal-cli

    # For the firebase emulator:
    pkgs.jdk
    pkgs.cacert
  ];

  # Additional brew settings for this host only
  homebrew = {
    taps = [
      "pulumi/tap"
      "withgraphite/tap"
    ];
    brews = [
      "codex"
      "earthly"
      "podman"
      "pulumi/tap/pulumi"
      "withgraphite/tap/graphite"

      # For PDF shrinking:
      "imagemagick"
      "ghostscript"
    ];
    casks = [
      "polypane"
      "hoppscotch"
    ];
  };
}
