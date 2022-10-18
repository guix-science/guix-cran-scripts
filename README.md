# Automated CRAN Guix channel

This script creates the [automated GNU Guix
channel](https://github.com/guix-science/guix-cran). You can run it from
a patched Guix checkout:

    guix shell -D guix
    /path/to/pre-inst-env guile --no-auto-compile -s import.scm output guix-cran

The directory `output` will contain the new channel if the import succeeded.

