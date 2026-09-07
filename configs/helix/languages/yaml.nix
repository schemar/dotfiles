{ lib, pkgs, ... }: {
  programs.helix = {
    extraPackages = with pkgs; [
      yamlfmt
      yaml-language-server
    ];

    languages = {
      language = [
        {
          name = "yaml";
          auto-format = true;
          formatter = {
            command = lib.getExe pkgs.yamlfmt;
            args = [ "-" ];
          };
        }
      ];
      grammar = [
        {
          name = "yaml";
          source = {
            git = "https://github.com/tree-sitter-grammars/tree-sitter-yaml";
            rev = "7708026449bed86239b1cd5bce6e3c34dbca6415";
          };
        }
      ];
    };
  };
}
