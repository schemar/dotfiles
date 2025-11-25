{ pkgs, ... }:
{
  programs.nixvim =
    let
      border = "single";
    in
    {

      extraPlugins = [
        pkgs.vimPlugins.other-nvim
      ];

      extraConfigLua = # lua
        ''
          require("other-nvim").setup({
            -- Should the window show files which do not exist yet based on
            -- pattern matching. Selecting the files will create the file.
            showMissingFiles = false,
            style = {
              border = "${border}",
            },
            -- Map files to other files.
            -- See documentation for details and more options.
            mappings = {
              {
                pattern = "(.*)/(.*).ts",
                target = "%1/%2.vue",
                context = "vue",
              },
              {
                pattern = "(.*)/(.*).vue",
                target = "%1/%2.ts",
                context = "vue",
              },
              {
                pattern = "(.*)/(.*).vue.gen.ts",
                target = "%1/%2.vue",
                context = "gen",
              },
              {
                pattern = "(.*)/(.*).vue",
                target = "%1/%2.vue.gen.ts",
                context = "gen",
              },
              {
                pattern = "(.*)/(.*).ts",
                target = "%1/%2.spec.ts",
                context = "spec",
              },
              {
                pattern = "(.*)/(.*).spec.ts",
                target = "%1/%2.ts",
                context = "spec",
              },
              {
                pattern = "(.*)/(.*).ts",
                target = "%1/%2.test.ts",
                context = "test",
              },
              {
                pattern = "(.*)/(.*).test.ts",
                target = "%1/%2.ts",
                context = "test",
              },
              {
                pattern = "(.*)/(.*).ts",
                target = "%1/%2.unit.test.ts",
                context = "test",
              },
              {
                pattern = "(.*)/(.*).unit.test.ts",
                target = "%1/%2.ts",
                context = "test",
              },
              {
                pattern = "(.*)/(.*).ts",
                target = "%1/%2.firestore.test.ts",
                context = "test",
              },
              {
                pattern = "(.*)/(.*).firestore.test.ts",
                target = "%1/%2.ts",
                context = "test",
              },
              {
                pattern = "(.*)/(.*).ts",
                target = "%1/%2.temporal.test.ts",
                context = "test",
              },
              {
                pattern = "(.*)/(.*).temporal.test.ts",
                target = "%1/%2.ts",
                context = "test",
              },
            },
          })
        '';

    };
}
