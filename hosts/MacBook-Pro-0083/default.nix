{ ... }: {
  imports = [
    ../common/homebrew.nix
  ];

  # Additional brew settings for this host only
  homebrew = {
    taps = [ "withgraphite/tap" ];
    brews = [ "withgraphite/tap/graphite" ];
    casks = [ "polypane" ];
  };
}
