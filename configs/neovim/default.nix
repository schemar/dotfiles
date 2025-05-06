{ config, ... }: {
  xdg.configFile."nvim" = {
    # Using "out of store symlink" to enable updates without switching nix-darwin.
    # Requires an absolute path to the target.
    # Hardcoded here, but not ideal.
    source = config.lib.file.mkOutOfStoreSymlink "/Users/schemar/Projects/dotfiles/configs/neovim";
    recursive = true;
  };
}

