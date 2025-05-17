{ pkgs, ... }:
{
  # Home Manager needs a bit of information about you and the
  # paths it should manage.
  home.username = "schemar";
  home.homeDirectory = /Users/schemar;

  # List packages installed in user profile. To search by name, run:
  # $ nix-env -qaP | grep wget
  home.packages = with pkgs; [
    tmux

    just
    sd
    delta

    # Languages, language servers, formatters, etc.:
    tree-sitter
    sqlite

    nixfmt-rfc-style

    lua

    cargo
  ];

  # This value determines the Home Manager release that your
  # configuration is compatible with. This helps avoid breakage
  # when a new Home Manager release introduces backwards
  # incompatible changes.
  #
  # You can update Home Manager without changing this value. See
  # the Home Manager release notes for a list of state version
  # changes in each release.
  home.stateVersion = "24.11";

  # Let Home Manager install and manage itself.
  programs.home-manager.enable = true;

  imports = [
    ../../configs/bash
    ../../configs/bat
    ../../configs/bottom
    ../../configs/fd
    ../../configs/fzf
    ../../configs/gh
    ../../configs/git
    ../../configs/jq
    ../../configs/mise
    ../../configs/neovim
    ../../configs/ripgrep
    ../../configs/starship
    ../../configs/ssh
    ../../configs/tealdeer
    ../../configs/theme
    ../../configs/tmux
    ../../configs/wezterm
    ../../configs/zsh
    # TODO: Make conditional for macOS only:
    ../../configs/aerospace
  ];
}
