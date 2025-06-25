_colorize_alias() {
    command -v grc >/dev/null || return
	local cmd
    local colorizer='grc'
    local commands=(
		ant
		as
		blkid
		cc
		configure
		curl
		cvs
		df
		dig
		diff
		dnf
		docker
		docker-compose
		docker-machine
		du
		env
		fdisk
		findmnt
		free
		g++
		gas
		gcc
		getfacl
		getsebool
		gmake
		id
		ifconfig
		iostat
		ip
		iptables
		iwconfig
		journalctl
		jq
		kubectl
		last
		ldap
		ld
		lolcat
		ls
		lsattr
		lsblk
		lsmod
		lsof
		lspci
		make
		mount
		mtr
		mvn
		netstat
		nmap
		ntpdate
		php
		ping
		ping6
		proftpd
		ps
		sar
		semanage
		sensors
		showmount
		sockstat
		ss
		stat
		sysctl
		systemctl
		tail
		tcpdump
		traceroute
		traceroute6
		tune2fs
		ulimit
		uptime
		vmstat
		wdiff
		whois
	)

    for cmd in "${commands[@]}"; do
        if ! alias "$cmd" &>/dev/null; then
			alias "$cmd"="$colorizer $cmd"
        fi
	done
}
_colorize_alias
