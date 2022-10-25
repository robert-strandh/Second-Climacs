#!/bin/bash
set -euo pipefail

#
# Add dependencies URLs here
#
DEPENDENCIES="
https://github.com/robert-strandh/Acclimation
https://github.com/robert-strandh/Clump
https://github.com/robert-strandh/cluffer
https://github.com/robert-strandh/cluffer-emacs-compatibility
https://github.com/robert-strandh/SICL
https://github.com/robert-strandh/Stealth-mixin
https://github.com/s-expressionists/Eclector
#
# Uncomment the following two dependencies to build the Climatis
# (non-McClim) version of Second-CLimacs.
#
# https://github.com/robert-strandh/Climatis
# https://github.com/Shinmera/trivial-benchmark
"

PROJECTS_DIRECTORY=${1:-"$HOME/quicklisp/local-projects/"}

if [ ! -d "$PROJECTS_DIRECTORY" ]; then
    cat <<EOF
Usage: $0 [PROJECTS_DIRECTORY].

You did not supply a directory to download dependencies into;
the default directory "$PROJECTS_DIRECTORY" does not exist.

EOF
    exit 1
fi

pushd "$PROJECTS_DIRECTORY"

for url in $DEPENDENCIES; do
    repository_name=`basename $url`

    if [ -d "$repository_name" ]; then
        echo " * pulling updates for $repository_name";
        pushd $repository_name
        git pull
        popd
    else
        echo " * cloning $repository_name";
        git clone $url
    fi
done

cat <<EOF
Done.

Run (ql:register-local-projects) from your REPL to add
any new systems to your environment.

EOF
