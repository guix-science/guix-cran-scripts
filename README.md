# Automated CRAN Guix channel

This script creates the [automated GNU Guix
channel](https://github.com/guix-science/guix-cran). You can run it from
a patched Guix checkout:

    guix shell -D guix
    git clone https://github.com/guix-science/guix-cran output
    mkdir -p cache/{description,contents}
    /path/to/pre-inst-env guile --no-auto-compile -s import.scm output guix-cran

The directory `output` will contain the new channel if the import succeeded.

