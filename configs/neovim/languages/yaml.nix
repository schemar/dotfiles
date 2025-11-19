{ ... }:
{
  programs.nixvim = {
    lsp = {
      servers = {
        yamlls.enable = true;
      };
    };
    plugins = {
      none-ls = {
        sources = {
          formatting = {
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
          };
        };
      };
      lsp-format = {
        lspServersToEnable = [
          "yamlls"
          "null-ls"
        ];

        settings = {
          yaml = {
            order = [
              "yamlls"
              "null-ls"
            ];
            sync = true;
          };
        };
      };
    };
  };
}
