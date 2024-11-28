set -gx DIRENV_LOG_FORMAT ''
if command -v direnv >/dev/null
    direnv hook fish | source
end
