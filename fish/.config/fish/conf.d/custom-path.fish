set -gx CUSTOM_BIN_HOME "$HOME/.config/bin"

switch ":$PATH:"
    case "*$CUSTOM_BIN_HOME*"
        # do nothing
        ;;
    case "*"
        set -gx PATH "$PATH:$CUSTOM_BIN_HOME/bin"
        ;;
end
