linktaxa
========

Build status
------------

[![Travis-CI Build Status](https://travis-ci.org/kardinal-eros/linktaxa.svg?branch=master)](https://travis-ci.org/kardinal-eros/linktaxa)
<!-- [![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/linktaxa)](http://cran.r-project.org/package=linktaxa) -->

About
-----

The main function takes two character vectors of scientific taxon names and performs an exhaustive search for possible analogs. The package further provides tools to accommodate various tasks on species names.

Installation
------------

You may directly install the package from GitHub using the below set of commands.

```R
# if not already installed
install.packages("devtools")

library(devtools)

install_github("kardinal-eros/linktaxa/pkg")

library(linktaxa)
```