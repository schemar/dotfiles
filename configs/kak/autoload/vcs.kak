##
## vcs.kak by lenormf
## Auto-detect the path to the root of the current repository
##

declare-option -docstring "name of the VCS detected" str vcs_name
declare-option -docstring "path to the root of the current versioned project" str vcs_root_path

hook global BufCreate .* %{ evaluate-commands %sh{
    for cmd in "git:git rev-parse --show-toplevel" \
               "svn:svn info | awk '/Working Copy Root Path:/ { print substr($0, 25); }'" \
               "mercurial:hg root" \
               "bazaar:bzr root"
    do
        cd "${kak_buffile%/*}" 2>/dev/null || continue

        name="${cmd%%:*}"
        cmd="${cmd#*:}"
        path=$(eval "${cmd}" 2>/dev/null)
        if [ -n "${path}" ]; then
            printf '
                set-option buffer vcs_name %%{%s}
                set-option buffer vcs_root_path %%{%s}
            ' "${name}" "${path}"
            break
        fi
    done
} }

# Auto-highlighting git changes:
hook global WinCreate .* %{ evaluate-commands %sh{
    if [ -n "${kak_opt_vcs_root_path}" ]; then
        case "${kak_opt_vcs_name}" in
            git)
                echo "
                    git show-diff
                    hook global BufWritePost %val{buffile} %{git update-diff}
                    hook global BufReload %val{buffile} %{git update-diff}
                    hook global WinDisplay %val{buffile} %{git update-diff}
                ";;
        esac
    fi
} }
