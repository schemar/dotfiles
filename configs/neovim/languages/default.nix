{ ... }:
{
  imports = [
    ./elixir.nix
    ./gdscript.nix
    ./html.nix
    ./json.nix
    ./just.nix
    ./lua.nix
    ./nix.nix
    ./shell.nix
    ./typescript.nix
    ./vue.nix
    ./xml.nix
    ./yaml.nix
    ./web.nix
  ];

  programs.nixvim = {
    plugins = {
      lsp-format.enable = true;
      lspconfig.enable = true;
      schemastore.enable = true;
      none-ls = {
        sources = {
          formatting = {
            prettier = {
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
    };
  };

}
