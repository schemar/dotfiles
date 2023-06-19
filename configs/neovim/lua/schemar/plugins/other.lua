return {
	"rgroli/other.nvim", -- Go to alternative file, e.g. ts<->vue or test
	name = "other-nvim",
	event = { "BufReadPost", "BufNewFile" },
	opts = {
		-- Should the window show files which do not exist yet based on
		-- pattern matching. Selecting the files will create the file.
		showMissingFiles = false,
		-- Map files to other files.
		-- See documentation for details and more options.
		mappings = {
			{ pattern = "(.*)/(.*).ts", target = "%1/%2.vue", context = "vue" },
			{ pattern = "(.*)/(.*).vue", target = "%1/%2.ts", context = "vue" },
			{ pattern = "(.*)/(.*).ts", target = "%1/%2.spec.ts", context = "spec" },
			{ pattern = "(.*)/(.*).spec.ts", target = "%1/%2.ts", context = "spec" },
			{ pattern = "(.*)/(.*).ts", target = "%1/%2.test.ts", context = "test" },
			{ pattern = "(.*)/(.*).test.ts", target = "%1/%2.ts", context = "test" },
			{ pattern = "(.*)/(.*).ts", target = "%1/%2.unit.test.ts", context = "test" },
			{ pattern = "(.*)/(.*).unit.test.ts", target = "%1/%2.ts", context = "test" },
		},
	},
}
