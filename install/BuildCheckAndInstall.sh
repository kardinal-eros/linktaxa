R CMD build --no-build-vignettes /Users/roli/Documents/linktaxa/pkg
R CMD check linktaxa_0.1-1.tar.gz
R CMD INSTALL -l /Users/roli/Library/R/3.0/library linktaxa_0.1-1.tar.gz
