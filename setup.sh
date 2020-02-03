#!/bin/bash

# Quick setup on linux.

LOCALREPO=`dirname $(readlink -f $0)`
CONFIGDIR=~/.emacs.d

patch_config() {
	# Patches config
  sed -i 's|~/emacs.d|'"$LOCALREPO|" personal.org
}

dep_install() {
	CONFIGDIR="$1"

	# Create a directory for storing third-party deps
	D="$LOCALREPO/deps"
	mkdir -vp "$D"

	# Install Harry's sensible defaults
	git clone https://github.com/hrs/sensible-defaults.el "$D"/sensible-defaults.el
	cp -nv "$D"/sensible-defaults.el/sensible-defaults.el "$CONFIGDIR"/sensible-defaults.el
}

new_install() {
	CONFIGDIR="$1"
	shift
	CPCMD="$1"
	# Create and install into directory.
	echo "Preparing to copy files:"
	ls $LOCALREPO/{init.el,configuration.org,personal.org,workgroups,agenda-files}

	${CPCMD} \
		 $LOCALREPO/{init.el,configuration.org,personal.org,workgroups,agenda-files} \
		 $CONFIGDIR

	mkdir -v $CONFIGDIR/{org,snippet}

}

main() {
	CONFIGDIR="$1"
	shift
	[ -z "$1" ] && CPCMD="cp -nv" || CPCMD="$1"
	mkdir -v ${CONFIGDIR}

	echo "LOCALREPO=${LOCALREPO}"
	echo "CONFIGDIR=${CONFIGDIR}"
	echo "CPCMD=${CPCMD}"

	pushd "$LOCALREPO"
	patch_config
	dep_install "$CONFIGDIR"
	new_install "$CONFIGDIR" "$CPCMD"
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
	# Faux home
	HOME="$LOCALREPO/test"

	# Cleanup any old tests
	mkdir -v "$HOME"

	# Install into faux home directory
	main "$HOME/.emacs.d" "cp -fv"

	# Run emacs with faux home
	echo "Now executing emacs..."
	env HOME="$HOME" /usr/bin/emacs --debug-init
}

cleanup() {
	rm -rfv "$LOCALREPO/test"
	rm -rfv "$LOCALREPO/deps"
}

[[ "$1" == "-h" || "$1" == "--help" ]] && usage && exit 0
[[ "$1" == "-t" || "$1" == "--test" ]] && test && exit 0
[[ "$1" == "-c" || "$1" == "--clean" ]] && cleanup && exit 0
[[ "$1" == "-i" || "$1" == "--install" ]] && main "$CONFIGDIR" && exit 0
usage
