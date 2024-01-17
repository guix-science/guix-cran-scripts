#!/bin/sh

# Guix CRAN/Bioconductor update script.  The first argument should be
# either "cran" or "bioc", dependent on which channel is supposed to
# be built.

# Bail out on errors, so we don't have to && everything.
set -e

# Git should not ask anything.
export GIT_TERMINAL_PROMPT=0
# `guix import cran` needs a proper locale, otherwise it'll fail.
export LANG=C.utf8

TYPE="$1" # cran or bioc
REPO="git@github.com:guix-science/guix-$TYPE.git"

# Init, if we run the first time.
test ! -d output && \
  git clone $REPO output

pushd output
# Update, in case any manual changes happened.
git pull
popd

# Make sure we only use default Guix channels, no matter how the system
# is configured.
if test "$TYPE" = "bioc"; then
    cat <<EOF > channels.scm
(cons (channel
       (name 'guix-cran)
       (url "https://github.com/guix-science/guix-cran.git"))
      %default-channels)
EOF
else
    cat <<EOF > channels.scm
%default-channels
EOF
fi
guix pull -C channels.scm -p profile

export GUIX_PROFILE=`pwd`/profile
source $GUIX_PROFILE/etc/profile

mkdir -p cache/{description,contents}
rm -f cache/description/*
guix repl import.scm output "guix-$TYPE" "$TYPE"

pushd output
git push origin --all
popd

rm -f profile profile-*-link channels.scm

exit 0
