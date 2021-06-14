# Neomutt puts temporary mail files into ~/.mutt/tmp/neomutt-...
hook global BufCreate .+tmp/neomutt-.+ %{
    set-option buffer filetype mail
}
