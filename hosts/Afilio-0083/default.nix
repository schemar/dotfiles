{
  inputs,
  pkgs,
  username,
  ...
}:
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

  # Configure home-manager to use the user config:
  home-manager.users.${username} = {
    imports = [
      ../../home
      {
        programs.bash.shellAliases.npm = "sfw npm";
        programs.zsh.shellAliases.npm = "sfw npm";
        programs.nixvim.plugins.none-ls.sources.formatting.prettier.settings.command = "oxfmt";
      }
    ];
  };

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
