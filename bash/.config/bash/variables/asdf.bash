asdf_vm() {
    # asdf version manager
    if [ -f ${HOME}/.asdf/asdf.sh ]; then
        source ${HOME}/.asdf/asdf.sh
        source ${HOME}/.asdf/completions/asdf.bash
    fi
}

asdf_vm
unset -f asdf_vm

