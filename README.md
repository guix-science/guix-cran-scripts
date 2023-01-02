# Automated CRAN Guix channel

This script creates the [automated GNU Guix
channel](https://github.com/guix-science/guix-cran). Since Guix commit
c329eeb3a8a87edfa21675ce67353778a3b24773 you can run it like this:

    git clone https://github.com/guix-science/guix-cran output
    mkdir -p cache/{description,contents}
    guix repl import.scm output guix-cran

The directory `output` will contain the updated channel if the import
succeeded.

