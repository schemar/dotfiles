{
  pkgs,
  lib,
  isDarwin,
  ...
}:
{
  # Home Manager needs a bit of information about you and the
  # paths it should manage.
  home.username = "schemar";
  home.homeDirectory = if isDarwin then /Users/schemar else /home/schemar;

  # List packages installed in user profile. To search by name, run:
  # $ nix-env -qaP | grep wget
  home.packages = with pkgs; [
    just
    sd
    delta

    # Languages, language servers, formatters, etc.:
    tree-sitter
    sqlite

    gdtoolkit_4

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
    ../../configs/jujutsu
    ../../configs/mise
    ../../configs/neovim
    ../../configs/ripgrep
    ../../configs/starship
    ../../configs/ssh
    ../../configs/tealdeer
    ../../configs/theme
    ../../configs/tmux
    ../../configs/wezterm
    ../../configs/yazi
    ../../configs/zsh
  ] ++ lib.optionals isDarwin [ ../../configs/aerospace ];
}
