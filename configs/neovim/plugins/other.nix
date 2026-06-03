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
                context = "vue template",
              },
              {
                pattern = "(.*)/(.*).vue",
                target = "%1/%2.ts",
                context = "typescript implementation",
              },
              {
                pattern = "(.*)/(.*).vue.gen.ts",
                target = "%1/%2.vue",
                context = "implementation file",
              },
              {
                pattern = "(.*)/(.*).vue",
                target = "%1/%2.vue.gen.ts",
                context = "generated template",
              },
              {
                pattern = "(.*)/(.*).ts",
                target = "%1/%2.spec.ts",
                context = "typescript test file",
              },
              {
                pattern = "(.*)/(.*).spec.ts",
                target = "%1/%2.ts",
                context = "typescript implementation file",
              },
              {
                pattern = "(.*)/(.*).ts",
                target = "%1/%2.test.ts",
                context = "typescript test file",
              },
              {
                pattern = "(.*)/(.*).test.ts",
                target = "%1/%2.ts",
                context = "typescript implementation file",
              },
              {
                pattern = "(.*)/(.*).ts",
                target = "%1/%2.unit.test.ts",
                context = "typescript test file",
              },
              {
                pattern = "(.*)/(.*).unit.test.ts",
                target = "%1/%2.ts",
                context = "typescript implementation file",
              },
              {
                pattern = "(.*)/(.*).ts",
                target = "%1/%2.firestore.test.ts",
                context = "firestore test file",
              },
              {
                pattern = "(.*)/(.*).firestore.test.ts",
                target = "%1/%2.ts",
                context = "firestore implementation file",
              },
              {
                pattern = "(.*)/(.*).ts",
                target = "%1/%2.temporal.test.ts",
                context = "temporal test file",
              },
              {
                pattern = "(.*)/(.*).temporal.test.ts",
                target = "%1/%2.ts",
                context = "temporal implementation file",
              },
              {
                pattern = "lib/(.*).ex$",
                target = "test/%1_test.exs",
                context = "test file",
              },
              {
                pattern = "test/(.*)_test.exs$",
                target = "lib/%1.ex",
                context = "implementation file",
              },
              {
                pattern = "src/(.*).erl$",
                target = "test/%1_SUITE.erl",
                context = "ct test file",
              },
              {
                pattern = "test/(.*)_SUITE.erl$",
                target = "src/%1.erl",
                context = "implementation file",
              },
              {
                pattern = "src/(.*).erl$",
                target = "test/_eunit/%1_tests.erl",
                context = "eunit test file",
              },
              {
                pattern = "test/_eunit/(.*)_tests.erl$",
                target = "src/%1.erl",
                context = "implementation file",
              },
            },
          })
        '';

    };
}
