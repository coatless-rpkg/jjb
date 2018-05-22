[![Linux Build Status](https://travis-ci.org/coatless/bmisc.svg?branch=master)](https://travis-ci.org/coatless/bmisc)[![CRAN RStudio mirror downloads](http://cranlogs.r-pkg.org/badges/bmisc)](http://www.r-pkg.org/pkg/bmisc)[![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/bmisc)](http://cran.r-project.org/package=bmisc)

# `bmisc` R Package
This repository holds the bmisc R package. This package contains a large amount of common functions I use in day to day analyses. Many folks are interested in obtaining these functions, hence I've placed them in a centrally located package.

# Install Instructions (All platforms)

To install the `bmisc` package, there are two options: CRAN (stable) or GitHub (Developmental)

The installation process with CRAN is the simplest

```r
install.packages("bmisc")
```

Prior to installing with `devtools`, please make sure to have a compiler installed on your system that is compatible with R.

If you have a compiler already installed, then continue on by installing the package dependencies and finally the package itself by doing the following: 

```r
# Install dependencies
install.packages(c("devtools"))

# Install the package from github
devtools::install_github("coatless/bmisc")
```

# Licensing
The license this source code is released under is the MIT License. Please retain the attributions behind the functions listed. 
