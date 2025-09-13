
<!-- README.md is generated from README.Rmd. Please edit that file -->

# `{susneoshiny}`

<!-- badges: start -->

<!-- badges: end -->

## Installation

You can install the development version of `{susneoshiny}` like so:

``` r
# FILL THIS IN! HOW CAN PEOPLE INSTALL YOUR DEV PACKAGE?
```

## Run

You can launch the application by running:

``` r
susneoshiny::run_app()
```

## About

You are reading the doc about version : 0.0.0.9000

This README has been compiled on the

``` r
Sys.time()
#> [1] "2025-09-13 13:00:03 -05"
```

Here are the tests results and package coverage:

``` r
devtools::check(quiet = TRUE)
#> ══ Documenting ═════════════════════════════════════════════════════════════════
#> ℹ Installed roxygen2 version (7.3.2) doesn't match required (7.1.1)
#> ✖ `check()` will not re-document this package
#> ── R CMD check results ───────────────────────────── susneoshiny 0.0.0.9000 ────
#> Duration: 39.2s
#> 
#> ❯ checking for portable file names ... WARNING
#>   Found the following file with a non-portable file name:
#>     data/SAMPLE ASSIGNMENT DATA.csv
#>   These are not fully portable file names.
#>   See section 'Package structure' in the 'Writing R Extensions' manual.
#> 
#> ❯ checking for missing documentation entries ... WARNING
#>   Undocumented code objects:
#>     'SAMPLE ASSIGNMENT DATA'
#>   Undocumented data sets:
#>     'SAMPLE ASSIGNMENT DATA'
#>   All user-level objects in a package should have documentation entries.
#>   See chapter 'Writing R documentation files' in the 'Writing R
#>   Extensions' manual.
#> 
#> ❯ checking for hidden files and directories ... NOTE
#>   Found the following hidden files and directories:
#>     .github
#>   These were most likely included in error. See section 'Package
#>   structure' in the 'Writing R Extensions' manual.
#> 
#> ❯ checking dependencies in R code ... NOTE
#>   Namespace in Imports field not imported from: 'pkgload'
#>     All declared Imports should be used.
#> 
#> 0 errors ✔ | 2 warnings ✖ | 2 notes ✖
#> Error: R CMD check found WARNINGs
```

``` r
covr::package_coverage()
#> susneoshiny Coverage: 51.06%
#> R/mod_dashboard.R: 0.00%
#> R/mod_data_upload.R: 0.00%
#> R/run_app.R: 0.00%
#> R/app_config.R: 100.00%
#> R/app_ui.R: 100.00%
```
