# rcmdshlib

[![Project Status: WIP - Initial development is in progress, but there has not yet been a stable, usable release suitable for the public.](http://www.repostatus.org/badges/latest/wip.svg)](http://www.repostatus.org/#wip)
[![Travis-CI Build Status](https://travis-ci.org/richfitz/rcmdshlib.svg?branch=master)](https://travis-ci.org/richfitz/rcmdshlib)
[![AppVeyor Build Status](https://ci.appveyor.com/api/projects/status/github/richfitz/rcmdshlib?branch=master&svg=true)](https://ci.appveyor.com/project/richfitz/rcmdshlib)
[![codecov.io](https://codecov.io/github/richfitz/rcmdshlib/coverage.svg?branch=master)](https://codecov.io/github/richfitz/rcmdshlib?branch=master)

Call `R CMD SHLIB` from R

This is _not_ intended to be used like csnippets, rcpp inline or `Rcpp::evalCpp`.  It is primarily intended to be usd by packages that need to dynamically create shared libraries to load into R.  It is currently used in [`dde`](https://github.com/richfitz/dde), in the documentation.
