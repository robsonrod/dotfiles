function gswitch

    function switch_to_branch
        set current_name (git branch --show-current)
        if test "$current_name" = "$argv"
            echo "Already on $current_name"
        else
            git switch "$argv"
        end
    end

    if test (count $argv) -gt 0
        switch_to_branch $argv
    else
        set current_name (git branch --show-current)
        set branch_name (git branch --sort committerdate | fzf --header "Switch Branch" | awk '{ print $2}')
        if ! [ -z $branch_name ]
            switch_to_branch $branch_name
        end
    end
end
