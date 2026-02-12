{
  pkgs,
  lib,
  isDarwin,
  ...
}:
{
  # This is the common home-manager configuration
  # It is used by both:
  # 1. NixOS/nix-darwin as a home-manager module
  # 2. Standalone home-manager (via standalone.nix)

  # This value determines the Home Manager release that your
  # configuration is compatible with. This helps avoid breakage
  # when a new Home Manager release introduces backwards
  # incompatible changes.
  #
  # You can update Home Manager without changing this value. See
  # the Home Manager release notes for a list of state version
  # changes in each release.
  home.stateVersion = "24.11";

  # List packages installed in user profile. To search by name, run:
  # $ nix-env -qaP | grep wget
  home.packages = with pkgs; [
    btop
    just
    sd
    delta

    # Languages, language servers, formatters, etc.:
    tree-sitter
    sqlite

    gdtoolkit_4

    nixfmt

    lua

    cargo
  ];

  imports = [
    ../configs/bash
    ../configs/bat
    ../configs/bottom
    ../configs/fd
    ../configs/fzf
    ../configs/gh
    ../configs/ghostty
    ../configs/git
    ../configs/jq
    ../configs/lazygit
    ../configs/mise
    ../configs/neovim
    ../configs/ripgrep
    ../configs/starship
    ../configs/ssh
    ../configs/tealdeer
    ../configs/theme
    ../configs/tmux
    ../configs/wezterm
    ../configs/yazi
    ../configs/zsh
  ]
  ++ lib.optionals isDarwin [ ../configs/aerospace ];
}
