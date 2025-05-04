{ ... }:
{
  homebrew = {
    enable = true;
    onActivation = {
      cleanup = "zap";
    };

    brews = [
      # Keep coreutils here to fix tmux.
      # Maybe move to nixpkgs with nix tmux.
      "coreutils"
      "fzf"
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
