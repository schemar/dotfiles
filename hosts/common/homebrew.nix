{ ... }:
{
  homebrew = {
    enable = true;
    onActivation = {
      cleanup = "zap";
    };

    brews = [
      "bash"
      "cmake"
      "coreutils"
      "curl"
      "fd"
      "fzf"
      "gh"
      "ImageMagick"
      "jq"
      "jsonlint"
      "just"
      "libsecret"
      "libtool"
      "libusb"
      "libvterm"
      "lsd"
      "lua"
      "markdownlint-cli"
      "mise"
      "neovim"
      "openvpn"
      "pandoc"
      "procs"
      "python"
      "ripgrep"
      "sd"
      "ShellCheck"
      "shfmt"
      "tealdeer"
      "tmux"
      "tree-sitter"
      "wget"
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
