function sha512 --description "Compute sha512 and encode to base64"
    if not test -f "$argv"
        echo "File does not exist"
        return 1
    end
    set hash (openssl dgst -sha512 $argv | cut -d ' ' -f2)
    set base64 (openssl dgst -binary -sha512 $argv | openssl base64 -A)

    set red "\e[35m"
    set green "\e[32m"
    set endcolor "\e[0m"

    echo -e "SHA512:" "$red$hash$endcolor"
    echo -e "BASE64:" "$green$base64$endcolor"
end
