function groot --description "goto root dir"
    set root_dir (command git rev-parse --show-toplevel 2> /dev/null)
    if [ "$root_dir" ]
        if [ "$PWD" != "$root_dir" ]
            cd $root_dir
        end
    end
end
