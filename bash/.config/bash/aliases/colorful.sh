_colorize_alias() {
	local aliases cmd colorizer='grc'
	for cmd in \
		cvs df diff gcc g++ ls ifconfig make mount mtr netstat ping ps tail traceroute wdiff blkid du dnf docker docker-machine env id ip iostat last lsattr lsblk lspci lsmod lsof getfacl getsebool uptime nmap fdisk findmnt free semanage sar ss sysctl systemctl stat showmount tune2fs tcpdump
	do
		aliases+="'$cmd=$colorizer $cmd' "
	done
	eval "alias ${aliases}"
}
_colorize_alias
