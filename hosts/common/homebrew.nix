{ ... }:
{
  homebrew = {
    enable = true;
    onActivation = {
      cleanup = "zap";
    };

    brews = [
      "bash"
      "bat"
      "cmake"
      "coreutils"
      "curl"
      "fd"
      "fzf"
      "gh"
      "git-delta"
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
      "yazi"
      "zsh-syntax-highlighting"
    ];
    casks = [
      "font-monaspace"
      "gimp"
      "raycast"
      "vlc"
      "wezterm"
      "unnaturalscrollwheels"
    ];
  };
}
