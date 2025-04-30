{ ... }: {
  imports = [
    ../common/homebrew.nix
  ];

  # Additional brew settings for this host only
  homebrew = {
    taps = [ 
      "pulumi/tap"
      "withgraphite/tap"
    ];
    brews = [ 
      "pulumi/tap/pulumi"
      "withgraphite/tap/graphite"
    ];
    casks = [ "polypane" ];
  };
}
