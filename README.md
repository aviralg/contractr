
<!-- README.md is generated from README.Rmd. Please edit that file -->
contractr
=========

[![Travis Build Status](https://travis-ci.org/aviralg/contractr.svg?branch=master)](https://travis-ci.org/aviralg/contractr) [![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/contractr)](http://cran.r-project.org/package=contractr)

Overview
--------

`contractr` processes annotations on function arguments and body and inserts corresponding contracts in the function body. Since `GNU R` does not support annotations (yet), this package relies on my own version of R, `rant`, which extends R to support annotations on functions, function formals and function body.

Installation
------------

To install this package you have to install `rant` and `annotatr` packages.

``` bash
git clone https://github.com/aviralg/rant
make -j -C rant
```

``` bash
git clone https://github.com/aviralg/annotatr
rant/bin/R CMD INSTALL annotatr
```

``` bash
git clone https://github.com/aviralg/contractr
rant/bin/R CMD INSTALL contractr
```
