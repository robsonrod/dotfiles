set -gx STARSHIP_CONFIG "$XDG_CONFIG_HOME/starship/starship.toml"
if string match -r '/dev/pts/*' (tty) > /dev/null
    if command -v starship >/dev/null
        starship init fish | source
    end
end
