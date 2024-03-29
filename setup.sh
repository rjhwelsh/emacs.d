#!/bin/bash

# Quick setup on linux.

LOCALREPO=`dirname $(readlink -f $0)`
CONFIGDIR="${CONFIGDIR:-~/.emacs.rjh}"
REPOLINK="rjh" # Relative to CONFIGDIR

patch_config() {
	return 0
}

dep_copy_files() {
    DEST="$1"
    shift

    for f in "$@";
    do

	mkdir -vp `dirname "$CONFIGDIR"/"$DEST"/"$f"`
	[[ -e "$CONFIGDIR"/"$DEST"/"$f" ]] && echo "Warning! ""$CONFIGDIR"/"$DEST"/"$f"' already exists.' >&2
	cp -nv "$D"/"$DEST"/"$f" "$CONFIGDIR"/"$DEST"/"$f"
    done
}

dep_git_clone_install() {
    URL="$1"
    shift
    DEST="$1"
    shift

    [ -d "$D"/"$DEST" ] &&\
	{ pushd "$D"/"$DEST" && git fetch && git merge origin/master; popd; } ||\
	    git clone "$URL" "$D"/"$DEST"

    dep_copy_files "$DEST" "$@"
}

dep_wget() {
    URL="$1"
    shift
    DEST="$1"
    shift

    { [ -d "$D"/"$DEST" ] || mkdir -vp "$D"/"$DEST"; } &&
	{ pushd "$D"/"$DEST" && wget "${URL}"; popd; }

    dep_copy_files "$DEST" "$@"
}

dep_wget_tar() {
    URL="$1"
    shift
    DEST="$1"
    shift
    ARCHIVE="$1"
    shift

    dep_wget "$URL" "$DEST"
    { pushd "$D"/"$DEST" && tar -xf "${ARCHIVE}" "$@"; popd; }
    dep_copy_files "$DEST" "$@"    
}


dep_emacswiki() {
    URL="https://www.emacswiki.org/emacs/download/$1"
    shift
    DEST="$1"
    shift
    f=`basename "${URL}"`

    dep_wget "${URL}" "${DEST}" "${f}"
}

dep_sensible() {
    # Install Harry's sensible defaults
    dep_git_clone_install https://github.com/hrs/sensible-defaults.el sensible-defaults sensible-defaults.el
}

dep_zetteldeft() {
    # Install zetteldeft
    dep_git_clone_install https://github.com/EFLS/zetteldeft zetteldeft zetteldeft.el
}

dep_org_gantt() {
    # Install org-gantt
    dep_git_clone_install https://github.com/swillner/org-gantt org-gantt org-gantt.el
}

dep_session() {
    dep_wget_tar "https://downloads.sourceforge.net/project/emacs-session/session-2.4b.tar.gz" session "session-2.4b.tar.gz" "session.el"
}

dep_options() {
    for dep in "$@"; do
	case $dep in
	    "all")
		dep_sensible;
		dep_zetteldeft;
		dep_org_gantt;
		BREAK="1"
		;;
	    "sensible-defaults")
		dep_sensible;
		;;
	    "zetteldeft")
		dep_zetteldeft;
		;;
	    "org-gantt")
		dep_org_gantt;
		;;
	    "emacswiki")
		dep_emacswiki "notify.el" "emacswiki"
		dep_emacswiki "frame-restore.el" "emacswiki"
		;;
	    "session")
		dep_session;
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
    # Create a directory for storing third-party deps
    D="$LOCALREPO/deps"
    mkdir -vp "$D"

    if [ -n "$1" ];
    then
	dep_options "$@";
    else
	select dep in quit \
			  all \
			  sensible-defaults \
			  zetteldeft \
			  org-gantt \
			  emacswiki \
			  session 
	do
	    dep_options $dep;
	    if [ -n "$BREAK" ]; then
		break
	    fi
	done
    fi
}

new_install() {
    # Create and install into directory.
    echo "Preparing to copy files:"
    ls $LOCALREPO/{init.el,agenda-files}

    ${CPCMD} \
	$LOCALREPO/{init.el,agenda-files} \
	$CONFIGDIR/

    # link to repo inside config dir (or set repo location in init.el)
    [[ ! -e "$CONFIGDIR"/"$REPOLINK" ]] && ln -srv "$LOCALREPO" "$CONFIGDIR"/"$REPOLINK"

    mkdir -v $CONFIGDIR/{org,snippet,private}

}

main() {
    CPCMD=${1:-cp -nv}

    echo "Files are being copied with '$CPCMD'." >&2
    
    mkdir -v ${CONFIGDIR}

    echo "LOCALREPO=${LOCALREPO}"
    echo "CONFIGDIR=${CONFIGDIR}"
    echo "CPCMD=${CPCMD}"

    pushd "$LOCALREPO"
    patch_config
    # dep_install "$CONFIGDIR"
    new_install
    popd
}

test_prepare() {
    # Links for testing:
    # ln -srv ~/.emacs.d/private test/.emacs.d/
    # ln -srv ~/.mu test/
    # ln -srv ~/.mail test/
    # ln -srv ~/.gnupg test/
    # ln -srv ~/.authinfo.gpg test/

    # Faux home
    export  HOME="$LOCALREPO/test"

    # Cleanup any old tests
    mkdir -v "$HOME"

    # Install into faux home directory

    # override environment values
    export CONFIGDIR="$HOME/.emacs.d"
    export CPCMD="cp -fv"

    main
    dep_install all
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
	    2>&1 |\
	    tee ../test/blank.log

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
		grep -vF -f ../test/blank.log |\
		tee ../test/"$INIT".log
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

usage() {
    cat <<EOF
$0 [-htic]
Setup/test emacs configuration

 -i, --install          Install (GNU Linux)
 -d, --install-deps     Install third-party dependencies

 -c, --clean            Cleanup Tests

 -t, --test             [config1 .. configN]
 -T, --test-interactive [config1 .. configN]

 -h, --help             Show this help

The following environment variables can be used.
    CONFIGDIR Overrides the default ~/.emacs.d install location

EOF
}

[[ "$1" == "-h" || "$1" == "--help" ]] && usage && exit 0
[[ "$1" == "-t" || "$1" == "--test" ]] && shift && test "$@" && exit 0
[[ "$1" == "-T" || "$1" == "--test-interactive" ]] && shift && test_interactive "$@" && exit 0
[[ "$1" == "-c" || "$1" == "--clean" ]] && cleanup && exit 0
[[ "$1" == "-d" || "$1" == "--install-deps" ]] && shift && dep_install "$@" && exit 0
[[ "$1" == "-i" || "$1" == "--install" ]] && shift && main && exit 0
usage
