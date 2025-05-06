{ ... }:
{
  # The platform the configuration will be used on.
  nixpkgs.hostPlatform = "aarch64-darwin";

  # Add ability to use TouchID for sudo authentication in terminal
  security.pam.enableSudoTouchIdAuth = true;

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
      # Keep coreutils here to fix tmux.
      # Maybe move to nixpkgs with nix tmux.
      "coreutils"
      "lsd"
      "lua"
      "markdownlint-cli"
      "mise"
      "neovim"
      "sd"
      "ShellCheck"
      "shfmt"
      "tree-sitter"
      "yamllint"
      "zsh-syntax-highlighting"
    ];
    casks = [
      "font-monaspace"
      "gimp"
      "raycast"
      "wezterm"
      "unnaturalscrollwheels"
    ];
  };
}
