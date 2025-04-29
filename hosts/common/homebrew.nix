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
      "direnv"
      "fd"
      "fzf"
      "gh"
      "git"
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
      "qmk/qmk/qmk"
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
      "zoxide"
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
