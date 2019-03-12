# documenter
An R package for documenting files


[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/documenter)](https://cran.r-project.org/package=documenter)
[![Travis-CI Build Status](https://travis-ci.org/MAMC-DCI/documenter.svg?branch=master)](https://travis-ci.org/MAMC-DCI/documenter#)
[![Build status](https://ci.appveyor.com/api/projects/status/rmci2ek7qdsshrsu?svg=true)](https://ci.appveyor.com/project/mamcdci/documenter)
[![Coverage Status](https://img.shields.io/codecov/c/github/mamc-dci/documenter/master.svg)](https://codecov.io/github/mamc-dci/documenter?branch=master)
[![DOI](https://zenodo.org/badge/153316232.svg)](https://zenodo.org/badge/latestdoi/153316232)


# Installation
The package can be installed via CRAN or GitHub using either of the following commands.
```r
# Install via CRAN.
install.packages("documenter")
```


```r
# Install via GitHub.
install.packages("devtools")
devtools::install_github("mamc-dci/documenter")
```

# Usage
The package can be loaded with the *library* function.
```r
# Load the package.
library(documenter)
```


After loading the package, the files of a directory can be documented using the *document_it* function. This function can be executed as follows:
```r
document_it(
  input_directory = "man",
  output_file = "documentation",
  annotation_file = NULL
)
```


## Disclaimer
The views expressed are those of the author(s) and do not reflect the official policy of the Department of the Army, the Department of Defense or the U.S. Government.
