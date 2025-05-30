{ config, isDarwin, ... }:
{
  programs.neovim = {
    enable = true;
  };

  xdg.configFile."nvim" = {
    # Using "out of store symlink" to enable updates without switching nix-darwin.
    # Requires an absolute path to the target.
    # Hardcoded here, but not ideal.
    source = config.lib.file.mkOutOfStoreSymlink (
      if isDarwin then
        "/Users/schemar/Projects/dotfiles/configs/neovim"
      else
        "/home/schemar/Projects/dotfiles/configs/neovim"
    );
    recursive = true;
  };
}
