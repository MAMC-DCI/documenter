# documenter
An R package for documenting files


[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/documenter)](https://cran.r-project.org/package=documenter)
[![Travis-CI Build Status](https://travis-ci.org/zcolburn/documenter.svg?branch=master)](https://travis-ci.org/zcolburn/documenter)
[![AppVeyor Build Status](https://ci.appveyor.com/api/projects/status/github/zcolburn/documenter?branch=master&svg=true)](https://ci.appveyor.com/project/zcolburn/documenter)
[![Coverage Status](https://img.shields.io/codecov/c/github/zcolburn/documenter/master.svg)](https://codecov.io/github/zcolburn/documenter?branch=master)
[![](https://cranlogs.r-pkg.org/badges/documenter)](https://cran.r-project.org/package=documenter)


# Installation
The package can be installed via CRAN or GitHub using either of the following commands.
```r
# Install via CRAN.
install.packages("documenter")
```


```r
# Install via GitHub.
install.packages("devtools")
devtools::install_github("zcolburn/documenter")
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
