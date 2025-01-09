asdf_vm() {
	# asdf version manager
	if [ -f ${HOME}/.asdf/asdf.sh ]; then
		source ${HOME}/.asdf/asdf.sh
		source ${HOME}/.asdf/completions/asdf.bash
	fi
}

asdf_update_zig_env() {
	local zig_bin_path
	zig_bin_path="$(asdf which zig 2>/dev/null)"
	if [[ -n "${zig_bin_path}" ]]; then
		abs_zig_bin_path="$(dirname "${zig_bin_path}")"

		export ZIGROOT
		ZIGROOT="$(dirname "${abs_zig_bin_path}")"

		export ZIGPATH
		ZIGPATH="${ZIGROOT}"

		export ZIGBIN
		ZIGBIN="${ZIGROOT}/bin"

		export PATH="$ZIGBIN:$PATH"
	fi
}

asdf_vm
asdf_update_zig_env

unset -f asdf_vm
unset -f asdf_update_zig_env
