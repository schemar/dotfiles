{ ... }:
{
  # The platform the configuration will be used on:
  nixpkgs.hostPlatform = "aarch64-darwin";
  # Include unfree packages from nixpkgs:
  nixpkgs.config.allowUnfree = true;

  # Add ability to use TouchID for sudo authentication in terminal:
  security.pam.services.sudo_local.touchIdAuth = true;
  # This fixes Touch ID for sudo not working inside tmux and screen:
  security.pam.services.sudo_local.reattach = true;

  # TODO: Continue manual at AeroSpace:
  # https://nix-darwin.github.io/nix-darwin/manual/index.html#opt-services.aerospace.enable

  # Required by home-manager:
  users.users.schemar.home = /Users/schemar;
  home-manager = {
    useGlobalPkgs = true;
    useUserPackages = true;

    users.schemar = ../../users/schemar;
  };

  homebrew = {
    enable = true;
    onActivation = {
      cleanup = "zap";
    };

    brews = [
      # Keep coreutils here to have greadlink which I use in ZSH to get the
      # current git repo. (see zsh config)
      "coreutils"
      "neovim"
      # Used by sqlite.lua in nvim (with brew path prefix):
      "sqlite"
    ];
    casks = [
      "gimp"
      "raycast"
      "wezterm"
      "unnaturalscrollwheels"

      # Could probably be done with nix-darwin as well:
      # https://nix-darwin.github.io/nix-darwin/manual/index.html#opt-fonts.packages
      "font-monaspace"
      "font-symbols-only-nerd-font"
    ];
  };
}
