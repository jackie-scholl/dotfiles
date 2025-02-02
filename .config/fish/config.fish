__init_fisher
__preserve_origs PATH LD_LIBRARY_PATH LD_RUN_PATH LDFLAGS CFLAGS

set -g pisces_only_insert_at_eol 1
set -g __done_min_cmd_duration 30000

set -gx GOPATH ~/.go
set -gx VOLTA_HOME ~/.volta

__add_to_path_if_exists PATH \
    ~/.cargo/bin \
    $VOLTA_HOME/bin \
    ~/.rvm/bin \
    $GOPATH/bin \
    ~/.cabal/bin

if command -q any-nix-shell
    any-nix-shell fish --info-right | source
end

if test -n "$IN_NIX_SHELL"
    if test -z "$NIX_SHELL_DEPTH"
        set -gx NIX_SHELL_DEPTH 1
    else
        set -gx NIX_SHELL_DEPTH (math "$NIX_SHELL_DEPTH + 1")
    end
end

set -gx EDITOR micro

set -gx FISHRC "$HOME/.config/fish/config.fish"
set -gx PYTHONSTARTUP "$HOME/.pythonrc"
set -gx --path LIBRARY_PATH "$LD_LIBRARY_PATH"  # python build uses this
set -gx CPPFLAGS  "$CFLAGS"

alias hg="kitty +kitten hyperlinked_grep"

abbr --add ls lsd
# Adding arguments
abbr -g df 'df -h'

abbr -g edit $EDITOR

direnv hook fish | source
