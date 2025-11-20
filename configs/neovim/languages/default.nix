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
      none-ls.enable = true;
      schemastore.enable = true;
    };
  };

}
