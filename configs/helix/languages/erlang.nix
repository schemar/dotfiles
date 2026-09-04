{ pkgs, lib, ... }: {
  programs.helix = {
    extraPackages = with pkgs; [
      erlang-language-platform
    ];

    languages = {
      language = [
        {
          name = "erlang";
          auto-format = true;
          formatter = {
            command = "${lib.getExe pkgs.erlfmt}";
            args = [ "-" ];
          };
        }
      ];
    };
  };
}
