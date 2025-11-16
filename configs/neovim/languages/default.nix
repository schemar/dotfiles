{ ... }:
{
  imports = [
    ./elixir.nix
    ./lua.nix
    ./nix.nix
    ./shell.nix
  ];

  programs.nixvim = {
    lsp = {
      servers = {
        cssls.enable = true;
        eslint.enable = true;
        gdscript.enable = true;
        just.enable = true;
        lemminx.enable = true; # XML LSP
        vtsls.enable = true;

        html = {
          enable = true;
          config = {
            filetypes = [
              "html"
              "templ"
              "vue"
            ];
          };
        };

        jsonls.enable = true;
        yamlls.enable = true;
      };
    };
    plugins = {
      lspconfig.enable = true;
      none-ls = {
        enable = true;

        sources = {
          diagnostics = {
            stylelint = {
              enable = true;
              settings = {
                extra_filetypes = [ "vue" ];
              };
            };
            gdlint = {
              enable = true;
            };
          };
          formatting = {
            gdformat = {
              enable = true;
            };
            just = {
              enable = true;
            };
            prettier = {
              enable = true;
              settings = {
                # Always format open files, even if they are ignored.
                # Setting `ignore-path` to empty string means "ignore nothing".
                # By default, prettier will ignore files in .gitignore and .prettierignore.
                extra_args = [
                  "--ignore-path"
                  ""
                ];
              };
            };
            stylelint = {
              enable = true;
              settings = {
                extra_filetypes = [ "vue" ];
              };
            };
          };
        };
      };
      lsp-format = {
        enable = true;

        lspServersToEnable = [
          "cssls"
          "eslint"
          "jsonls"
          "lemminx"
          "yamlls"
          "null-ls"
        ];

        settings = {
          html = {
            order = [ "null-ls" ];
            sync = true;
            # Format HTML only with prettier.
            exclude = [ "html" ];
          };
          javascript = {
            order = [
              "eslint"
              "null-ls"
            ];
            sync = true;
            # Format JS only with eslint and prettier.
            exclude = [ "vtsls" ];
          };
          typescript = {
            order = [
              "eslint"
              "null-ls"
            ];
            sync = true;
            # Format TS only with eslint and prettier.
            exclude = [ "vtsls" ];
          };
          json = {
            order = [
              "jsonls"
              "null-ls"
            ];
            sync = true;
          };
          xml = {
            order = [
              "lemminx"
            ];
            sync = true;
          };
          yaml = {
            order = [
              "yamlls"
              "null-ls"
            ];
            sync = true;
          };
          scss = {
            order = [
              "cssls"
              "null-ls"
            ];
            sync = true;
          };
          vue = {
            order = [ "null-ls" ];
            sync = true;
            # Format only with stylelint and prettier.
            exclude = [ "html" ];
          };
        };
      };
    };
  };

}
