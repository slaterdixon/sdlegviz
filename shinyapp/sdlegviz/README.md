
<!-- README.md is generated from README.Rmd. Please edit that file -->

# `{sdlegviz}`

<!-- badges: start -->

<!-- badges: end -->

## Installation

You can install the development version of `{sdlegviz}` like so:

``` r
# FILL THIS IN! HOW CAN PEOPLE INSTALL YOUR DEV PACKAGE?
```

## Run

You can launch the application by running:

``` r
sdlegviz::run_app()
```

## About

You are reading the doc about version : 0.0.0.9000

This README has been compiled on the

``` r
Sys.time()
#> [1] "2026-10-25 19:49:18 CDT"
```

Here are the tests results and package coverage:

``` r
devtools::check(quiet = TRUE)
#> ══ Documenting ═════════════════════════════════════════════════════════════════
#> ℹ Installed roxygen2 version (8.1.1) doesn't match declared (7.1.1)
#> ✖ `check()` will not re-document this package.
#> ℹ Do you need to re-run `document()`?
#> ── R CMD check results ──────────────────────────────── sdlegviz 0.0.0.9000 ────
#> Duration: 18s
#> 
#> ❯ checking tests ...
#>   See below...
#> 
#> ❯ checking for unstated dependencies in ‘tests’ ... WARNING
#>   '::' or ':::' import not declared from: ‘htmltools’
#> 
#> ❯ checking package subdirectories ... NOTE
#>   Problems with news in ‘NEWS.md’:
#>   No news entries found.
#> 
#> ── Test failures ───────────────────────────────────────────────── testthat ────
#> 
#> > # This file is part of the standard setup for testthat.
#> > # It is recommended that you do not modify it.
#> > #
#> > # Where should you do additional test configuration?
#> > # Learn more about the roles of various files in:
#> > # * https://r-pkgs.org/testing-design.html#sec-tests-files-overview
#> > # * https://testthat.r-lib.org/articles/special-files.html
#> > 
#> > library(testthat)
#> > library(sdlegviz)
#> > 
#> > test_check("sdlegviz")
#> Saving _problems/test-golem-recommended-7.R
#> Loading required package: shiny
#> [ FAIL 1 | WARN 0 | SKIP 1 | PASS 81 ]
#> 
#> ══ Skipped tests (1) ═══════════════════════════════════════════════════════════
#> • rlang_is_interactive() is not TRUE (1): 'test-golem-recommended.R:71:2'
#> 
#> ══ Failed tests ════════════════════════════════════════════════════════════════
#> ── Failure ('test-golem-recommended.R:7:3'): app ui ────────────────────────────
#> Expected `i %in% names(fmls)` to be TRUE.
#> Differences:
#> `actual`:   FALSE
#> `expected`: TRUE 
#> 
#> 
#> [ FAIL 1 | WARN 0 | SKIP 1 | PASS 81 ]
#> Error:
#> ! Test failures.
#> Execution halted
#> 
#> 1 error ✖ | 1 warning ✖ | 1 note ✖
#> Error:
#> ! R CMD check found ERRORs
```

``` r
covr::package_coverage()
#> Error in `loadNamespace()`:
#> ! there is no package called 'covr'
```
