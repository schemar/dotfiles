{ ... }:
{
  # The platform the configuration will be used on.
  nixpkgs.hostPlatform = "aarch64-darwin";

  # Add ability to use TouchID for sudo authentication in terminal
  security.pam.services.sudo_local.touchIdAuth = true;

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

      "font-monaspace"
      "font-symbols-only-nerd-font"
    ];
  };
}
