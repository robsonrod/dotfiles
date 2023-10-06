function connect2 --description "Connect to host"
    set selected (rg "Host" ~/.ssh/config | awk '{ print $2 }' | xargs -n 2 | awk '{ print $1 }' | fzf --query "$LBUFFER" --height 50%)
    set ip_adress (rg "Host" ~/.ssh/config | awk '{ print $2 }' | xargs -n 2 | grep "$selected" | awk '{ print $2 }')

    ping $ip_adress -c 1 -W 1 > /dev/null

    if test $status -ne 0
        echo "$selected offline or not accepting SSH logins"
        return
    end

    if test ! -z "$selected"
        ssh -X "$selected"
    end
end
