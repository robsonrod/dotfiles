# bash_profile

if [ -f "$HOME/.bashrc" ]; then
    . "$HOME/.bashrc"
fi

if [ -f "$HOME/.cargo/env" ]; then
    . "$HOME/.cargo/env"
fi

if [ -f "$HOME/.local/share/env" ]; then
    . "$HOME/.local/share/bin/env"
fi
