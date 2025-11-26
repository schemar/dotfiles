{ pkgs, ... }:
let
  gdk = pkgs.google-cloud-sdk.withExtraComponents (
    with pkgs.google-cloud-sdk.components;
    [
      kubectl
    ]
  );
in
{
  imports = [
    ../common.nix
    ../darwin.nix
    ../../users/schemar/common.nix
    ../../users/schemar/darwin.nix
  ];

  # Use `\npm` or `env npm` to prevent using the alias when installing sfw.
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
