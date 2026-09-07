{ pkgs, ... }:
{
  programs.nixvim = {
    extraPlugins = [
      (pkgs.vimUtils.buildVimPlugin {
        name = "blueberry-peach.nvim";
        src = pkgs.fetchFromGitHub {
          owner = "schemar";
          repo = "blueberry-peach.nvim";
          rev = "0a288892eae41e6714e68b599be140c6fc3407ca";
          hash = "sha256-TSHd+tosdgGkdejwN2ufdUQ6hzDWkNyq5wRAV6pt/mo=";
        };
      })
    ];
    opts = {
      # [[ Theme ]]
      syntax = "ON"; # str: Allow syntax highlighting
      termguicolors = true; # bool: If term supports ui color then enable
      cursorline = true; # bool: Highlight current line
      # listchars = "space:·,tab:>~,trail:~,extends:>,precedes:<,eol:󰌑"
      listchars = "tab:~~,trail:~";
      list = true;

    };
    colorscheme = "blueberry-peach";
    plugins.lualine.settings.options.theme = "blueberry-peach";
  };
}
