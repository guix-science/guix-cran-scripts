# Automated CRAN and Bioconductor Guix channels

This script creates the [automated GNU Guix channel for CRAN
packages](https://github.com/guix-science/guix-cran) and the
[automated GNU Guix channel for Bioconductor
packages](https://github.com/guix-science/guix-bioc).  Since Guix
commit c329eeb3a8a87edfa21675ce67353778a3b24773 you can run it like
this:

    REPO=https://github.com/guix-science/guix-cran
    # REPO=https://github.com/guix-science/guix-bioc
    git clone $REPO output
    mkdir -p cache/{description,contents}
    guix repl import.scm output [guix-cran|guix-bioc] [cran|bioc]

The directory `output` will contain the updated channel if the import
succeeded.

