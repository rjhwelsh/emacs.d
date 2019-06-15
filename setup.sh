#!/bin/bash

# Quick setup on linux.

LOCALREPO=`dirname $(readlink -f $0)`
CONFIGDIR=~/.emacs.d

patch_config() {
	# Patches config
  sed -i 's|~/emacs.d|'"$LOCALREPO|" personal.org
}

new_install() {
	CONFIGDIR="$1"
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
	new_install "$CONFIGDIR"
	popd
}

usage() {
	echo "$0 [-htic]"
	echo "Setup/test emacs configuration"
	echo " -c, --clean      Cleanup Tests"
	echo " -t, --test       Test"
	echo " -i, --install    Install (GNU Linux)"
	echo " -h, --help       Show this help"
}

test() {
	HOME="$LOCALREPO/test"
	# Cleanup any old tests
	cleanup && mkdir -v "$HOME"

	pushd "$LOCALREPO"
	patch_config
	new_install "$HOME/.emacs.d"
	popd

	echo "Now executing emacs..."
	env HOME="$HOME" /usr/bin/emacs
}

cleanup() {
	rm -rfv "$LOCALREPO/test"
}

[[ "$1" == "-h" || "$1" == "--help" ]] && usage && exit 0
[[ "$1" == "-t" || "$1" == "--test" ]] && test && exit 0
[[ "$1" == "-c" || "$1" == "--clean" ]] && cleanup && exit 0
[[ "$1" == "-i" || "$1" == "--install" ]] && main && exit 0
usage
