set -gx CARGO_HOME "$XDG_DATA_HOME/cargo"

switch ":$PATH:"
    case "*:/cargo/bin:*"
        # do nothing
        ;;
    case "*"
        set -gx PATH "$PATH:$CARGO_HOME/bin"
        ;;
end
