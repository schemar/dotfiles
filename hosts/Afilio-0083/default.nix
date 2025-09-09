{ ... }:
{
  imports = [
    ../common.nix
    ../darwin.nix
    ../../users/schemar/common.nix
    ../../users/schemar/darwin.nix
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
