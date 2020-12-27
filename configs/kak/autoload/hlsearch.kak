define-command hlsearch-on %{
    add-highlighter global/hltoggle dynregex '%reg{/}' 0:rgb:2e3440,rgb:ebcb8b
}

define-command hlsearch-off %{
    remove-highlighter global/hltoggle
}
