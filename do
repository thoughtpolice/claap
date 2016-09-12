#!/usr/bin/env bash

CABAL=cabal
GHC=ghc-7.10.3

set -euo pipefail

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

absoluteRoot="$(dirname "$(rl "$0")")"
cd "$absoluteRoot"

if ! type "$CABAL" > /dev/null; then
    echo "Please make sure 'cabal' is in your PATH"
    exit 2
fi

CABVERSTR=$("$CABAL" --numeric-version)

CABVER=( ${CABVERSTR//./ } )

if [ "${CABVER[0]}" -eq 1 -a "${CABVER[1]}" -ge 24 ]; then
    "$CABAL" new-build --disable-profiling \
                       --disable-documentation \
                       -j \
                       claap-build:mk >/dev/null

    PKGVER="$(awk '/^version:/ { print $2 }' src/mk/claap-build.cabal)"
    EXE="./dist-newstyle/build/*/$GHC/"
    EXE+="claap-build-${PKGVER}/c/mk/noopt/build/mk/mk"

    eval $EXE \
        --lint --color              \
        --directory "$absoluteRoot" \
        "$@"

else
    echo "Pre-cabal 1.24 detected. Failing."
    exit 1
fi
