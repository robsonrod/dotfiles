set -gx CARGO_HOME "$XDG_DATA_HOME/cargo"

switch ":$PATH:"
    case "*$CARGO_HOME*"
        # do nothing
        ;;
    case "*"
        set -gx PATH "$PATH:$CARGO_HOME/bin"
        ;;
end
