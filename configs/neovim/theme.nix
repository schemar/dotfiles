{ ... }:
{
  programs.nixvim = {
    opts = {
      # [[ Theme ]]
      syntax = "ON"; # str: Allow syntax highlighting
      termguicolors = true; # bool: If term supports ui color then enable
      cursorline = true; # bool: Highlight current line
      # listchars = "space:·,tab:>~,trail:~,extends:>,precedes:<,eol:󰌑"
      listchars = "tab:~~,trail:~";
      list = true;

    };
    colorschemes = {
      catppuccin = {
        enable = true;
        autoLoad = true;
        luaConfig = {
          pre = # lua
            ''
              ---Selects the catppuccin flavour or falls back to the given fallback.
              ---@param fallback "latte"|"frappe"|"macchiato"|"mocha"
              ---@return "latte"|"frappe"|"macchiato"|"mocha"
              function get_theme(fallback)
                local xdg_config_home = vim.env.XDG_CONFIG_HOME
                  or os.getenv("XDG_CONFIG_HOME")
                  or (vim.env.HOME .. "/.config")
                local theme_path = xdg_config_home .. "/current_theme_store"

                local file, err = io.open(theme_path, "r")
                if not file or err then
                  vim.notify("Could not read theme file " .. theme_path, vim.log.levels.WARN)
                  return fallback
                end

                local content = file:read("*a")
                file:close()

                -- Validate content
                if content ~= "light" and content ~= "dark" then
                  vim.notify("Invalid theme value: " .. content, vim.log.levels.ERROR)
                  return fallback
                end

                return content == "light" and "frappe" or "mocha"
              end

              local catppuccin_config = {
                flavour = get_theme("frappe"), -- latte, frappe, macchiato, mocha
                integrations = {
                  beacon = true,
                  blink_cmp = true,
                  copilot_vim = true,
                  diffview = true,
                  fidget = true,
                  flash = true,
                  gitsigns = true,
                  illuminate = {
                    enabled = true,
                    lsp = true,
                  },
                  indent_blankline = {
                    enabled = true,
                  },
                  lsp_trouble = true,
                  markdown = true,
                  mason = true,
                  native_lsp = {
                    enabled = true,
                    virtual_text = {
                      errors = { "italic" },
                      hints = { "italic" },
                      warnings = { "italic" },
                      information = { "italic" },
                      ok = { "italic" },
                    },
                    underlines = {
                      errors = { "underline" },
                      hints = { "underline" },
                      warnings = { "underline" },
                      information = { "underline" },
                      ok = { "underline" },
                    },
                    inlay_hints = {
                      background = true,
                    },
                  },
                  neogit = true,
                  nvimtree = true,
                  render_markdown = true,
                  telescope = { enabled = true },
                  treesitter = true,
                  treesitter_context = true,
                  ufo = true,
                  which_key = true,
                }
              }

              -- Extend with overrides for blueberry-peach theme
              local blueberry_peach_light = require("blueberry_peach.light")
              catppuccin_config = vim.tbl_deep_extend("force", catppuccin_config, blueberry_peach_light.get_overrides("frappe"))
              local blueberry_peach_dark = require("blueberry_peach.dark")
              catppuccin_config = vim.tbl_deep_extend("force", catppuccin_config, blueberry_peach_dark.get_overrides("mocha"))
            '';
        };
        settings = {
          __raw = "catppuccin_config";
        };
      };
    };
  };
}
