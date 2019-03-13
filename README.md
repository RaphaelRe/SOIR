Scalar-on-Image regression using iterative methods
--------------------------------------------------

This package is a wrapper for the Sarim package (also available via github). Note that for Sarim C++11 must be enabled. This can be done in R with:

``` r
# install Sarim:
Sys.setenv("PKG_CXXFLAGS"="-std=c++11")
devtools::install_github("chkue/Sarim")
```

Install SOIR with:

``` r
devtools::install_github("RaphaelRe/SOIR")
```

The SOIR() function can be used within Sarim. This is a short example how to use: ![](README_files/figure-markdown_github/unnamed-chunk-3-1.png)
