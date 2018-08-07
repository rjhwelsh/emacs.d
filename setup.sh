#!/bin/bash

# Quick setup on linux.

LOCALREPO=`dirname $(readlink -f $0)`
CONFIGDIR=~/.emacs.d

patch_config() {
	# Patches config
  sed -i 's|~/emacs.d|'"$LOCALREPO|" personal.org
}

new_install() {
	# Create and install into directory.
	echo "Preparing to copy files:"
	ls $LOCALREPO/{init.el,configuration.org,personal.org,workgroups,agenda-files}

	mkdir -v ${CONFIGDIR}

	cp -nv \
		 $LOCALREPO/{init.el,configuration.org,personal.org,workgroups,agenda-files} \
		 $CONFIGDIR

	mkdir -v $CONFIGDIR/{org,snippet}

}

main() {
	echo "LOCALREPO=${LOCALREPO}"
	echo "CONFIGDIR=${CONFIGDIR}"

	pushd $LOCALREPO
	patch_config
	new_install
	popd
}

usage() {
	echo "$0 [-h]"
	echo " -h    Show this help"
	echo "Run ./setup.sh without any arguments to install this Emacs configuration on a new linux system."
}

[[ "$1" == "-h" || "$1" == "--help" ]] && usage && exit 0
main
