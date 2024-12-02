export DIRENV_LOG_FORMAT=''

if checkexec direnv; then
    eval "$(direnv hook bash)"
fi
