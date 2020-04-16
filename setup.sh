#!/bin/bash

# Quick setup on linux.

LOCALREPO=`dirname $(readlink -f $0)`
CONFIGDIR=~/.emacs.d
REPOLINK="rjh" # Relative to CONFIGDIR

patch_config() {
	return 0
}

dep_git_clone_install() {
	URL="$1"
	shift
	DEST="$1"
	shift

	[ -d "$D"/"$DEST" ] &&\
		{ pushd "$D"/"$DEST" && git fetch && git merge origin/master; popd; } ||\
			git clone "$URL" "$D"/"$DEST"

	for f in "$@";
	do

		mkdir -vp `dirname "$CONFIGDIR"/"$DEST"/"$f"`
		cp -iv "$D"/"$DEST"/"$f" "$CONFIGDIR"/"$DEST"/"$f"
	done
}

dep_sensible() {
	# Install Harry's sensible defaults
	dep_git_clone_install https://github.com/hrs/sensible-defaults.el sensible-defaults.el sensible-defaults.el
}

dep_zetteldeft() {
	# Install zetteldeft
	dep_git_clone_install https://github.com/EFLS/zetteldeft zetteldeft zetteldeft.el
}

dep_options() {
	for dep in "$@"; do
		case $dep in
			"all")
				dep_sensible;
				dep_zetteldeft;
				BREAK="1"
				;;
			"sensible-defaults")
				dep_sensible;
				;;
			"zetteldeft")
				dep_zetteldeft;
				;;
			"quit")
				BREAK="1"
				;;
			*)
				echo "Invalid entry."
				BREAK="1"
				;;
		esac
	done
}

dep_install() {
	CONFIGDIR="$1"
	shift

	# Create a directory for storing third-party deps
	D="$LOCALREPO/deps"
	mkdir -vp "$D"

	if [ -n "$1" ];
	then
		dep_options "$@";
	else
		select dep in all \
										sensible-defaults \
										zetteldeft \
										quit
		do
			dep_options $dep;
			if [ -n "$BREAK" ]; then
				break
			fi
		done
	fi
}

new_install() {
	CONFIGDIR="$1"
	shift
	CPCMD="$1"

	# Create and install into directory.
	echo "Preparing to copy files:"
	ls $LOCALREPO/{init.el,workgroups,agenda-files}

	${CPCMD} \
		 $LOCALREPO/{init.el,workgroups,agenda-files} \
		 $CONFIGDIR

	# link to repo inside config dir (or set repo location in init.el)
	[[ ! -d "$CONFIGDIR"/"$REPOLINK" ]] && ln -srv "$LOCALREPO" "$CONFIGDIR"/"$REPOLINK"

	mkdir -v $CONFIGDIR/{org,snippet,private}

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
	# dep_install "$CONFIGDIR"
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

test_prepare() {
	# Faux home
	HOME="$LOCALREPO/test"

	# Cleanup any old tests
	mkdir -v "$HOME"

	# Install into faux home directory
	main "$HOME/.emacs.d" "cp -fv"
	dep_install "$HOME/.emacs.d" all
}

test() {
	test_prepare
	# Run emacs with faux home
	pushd "$HOME"
	{
		echo "Test blank configuration ..."
		env HOME="$HOME" EMACS_CONFIG="" \
				/usr/bin/emacs --debug-init \
				--batch --no-window-system \
				-l ./.emacs.d/init.el \
				> ../test/blank.log 2>&1
		cat ../test/blank.log

		echo "Now testing each emacs config..."
		[ -n "$@" ] && FILES="$@"
		[ -z "$@" ]	&& FILES=`find ../init -iname '*.org' -printf "%p "|\
										sed 's|\.org||g'|\
										sed 's|\.\./init/||g'`;

		for INIT in ${FILES}
		do
			echo "Testing ($INIT) ..."
			mkdir -vp `dirname ../test/"$INIT".log`
			env HOME="$HOME" EMACS_CONFIG="$INIT" \
					/usr/bin/emacs --debug-init \
					--batch --no-window-system \
					-l ./.emacs.d/init.el \
					2>&1 |\
				grep -vF -f ../test/blank.log > ../test/"$INIT".log
			cat ../test/"$INIT".log
			echo "Finished ($INIT)..."
		done
	}
	popd
}

test_interactive() {
	test_prepare
	pushd "$HOME"
	{
		# Run emacs interactively (after testing all config files)
		env HOME="$HOME" EMACS_CONFIG="$*" /usr/bin/emacs --debug-init
	}
	popd
}

cleanup() {
	rm -rfv "$LOCALREPO/test"
	rm -rfv "$LOCALREPO/deps"
}

[[ "$1" == "-h" || "$1" == "--help" ]] && usage && exit 0
[[ "$1" == "-t" || "$1" == "--test" ]] && shift && test "$@" && exit 0
[[ "$1" == "-T" || "$1" == "--test-interactive" ]] && shift && test_interactive "$@" && exit 0
[[ "$1" == "-c" || "$1" == "--clean" ]] && cleanup && exit 0
[[ "$1" == "-d" || "$1" == "--install-deps" ]] && shift && dep_install "$CONFIGDIR" "$@" && exit 0
[[ "$1" == "-i" || "$1" == "--install" ]] && main "$CONFIGDIR" && exit 0
usage
