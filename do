#!/usr/bin/env bash
unset CDPATH
set -euo pipefail

## -----------------------------------------------------------------------------
## -- Setup

if [ -z ${CABAL+x} ]; then
    CABAL=cabal
fi

# readlink on os x, doesn't support -f, to prevent the
# need of installing coreutils (e.g. through brew, just
# for readlink, we use the follownig substitute.
#
# source: http://stackoverflow.com/a/1116890
function rl {
    TARGET_FILE="$1"

    cd "$(dirname "$TARGET_FILE")"
    TARGET_FILE="$(basename "$TARGET_FILE")"

    # Iterate down a (possible) chain of symlinks
    while [ -L "$TARGET_FILE" ]
    do
        TARGET_FILE="$(readlink "$TARGET_FILE")"
        cd "$(dirname "$TARGET_FILE")"
        TARGET_FILE="$(basename "$TARGET_FILE")"
    done

    # Compute the canonicalized name by finding the physical path
    # for the directory we're in and appending the target file.
    PHYS_DIR="$(pwd -P)"
    RESULT="$PHYS_DIR/$TARGET_FILE"
    echo "$RESULT"
}

# Optionally redirect standard output to /dev/null if $DEBUG is set. Used to
# make the debug run of cabal new-build quiet. Because this does not redirect
# standard error, this means warnings and build failures come through as
# expected (rather than getting a hidden failure).
function redir_devnull {
    if [ -z ${DEBUG+x} ]; then
        "$@" > /dev/null
    else
        "$@"
    fi
}

## -----------------------------------------------------------------------------
## -- First, start off in the root dir

absoluteRoot="$(dirname "$(rl "$0")")"
cd "$absoluteRoot"

## -----------------------------------------------------------------------------
## -- Two-part version check

if ! type "$CABAL" > /dev/null; then
    echo "Please make sure 'cabal' is in your PATH"
    exit 2
fi

CABVERSTR=$("$CABAL" --numeric-version)
CABVER=( ${CABVERSTR//./ } )
if [ "${CABVER[0]}" -eq 1 -a "${CABVER[1]}" -le 24 ]; then
    echo "Pre-cabal 1.25 detected. Failing."
    exit 1
fi

## -----------------------------------------------------------------------------
## -- Build rules

cd src/mk
redir_devnull \
    "$CABAL" new-build \
        --disable-profiling \
        --disable-documentation \
        -j \
        claap-build:mk

GHCVER="$(awk '/^tested-with:/ { print $4 }' claap-build.cabal)"
PKGVER="$(awk '/^version:/     { print $2 }' claap-build.cabal)"
EXE="./dist-newstyle/build/*/ghc-$GHCVER/"
EXE+="claap-build-${PKGVER}/c/mk/noopt/build/mk/mk"

eval $EXE \
--lint --color              \
--directory "$absoluteRoot" \
"$@"
