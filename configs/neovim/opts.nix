{ ... }:
{
  programs.nixvim.opts = {
    # [[ Folding ]]
    foldcolumn = "0"; # '0' to hide or '1' to show
    foldlevel = 99; # Using ufo provider need a large value, feel free to decrease the value
    foldlevelstart = 99;
    foldenable = true;

    # [[ Misc ]]
    timeout = true;
    timeoutlen = 300; # num: Timeout, e.g. for which-key
    updatetime = 1000; # num: Timeout for "cursor hold" event
    clipboard = "unnamedplus"; # str: Clipboard integration with macOS
    splitkeep = "cursor"; # The default "screen" moves the cursor wrongly, which leads to problems, e.g. with Trouble
    wrap = false; # bool: Disable line wrapping

    # [[ Context ]]
    colorcolumn = "80"; # str: Show col for max line length
    number = true; # bool: Show line numbers
    scrolloff = 5; # int: Min num lines of context
    signcolumn = "yes"; # str: Show the sign column

    # [[ Filetypes ]]
    encoding = "utf8"; # str: String encoding to use
    fileencoding = "utf8"; # str: File encoding to use

    # [[ Search ]]
    ignorecase = true; # bool: Ignore case in search patterns
    smartcase = true; # bool: Override ignorecase if search contains capitals
    incsearch = true; # bool: Use incremental search

    # [[ Whitespace ]]
    expandtab = true; # bool: Use spaces instead of tabs
    shiftwidth = 2; # num: Size of an indent
    softtabstop = 2; # num: Number of spaces tabs count for in insert mode
    tabstop = 2; # num: Number of spaces tabs count for
  };

}
