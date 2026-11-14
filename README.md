
<!-- README.md is generated from README.Rmd. Please edit that file -->

# `{sdlegviz}`

<!-- badges: start -->

<!-- badges: end -->


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
#> [1] "2026-10-25 21:47:14 CDT"
```

Here are the tests results and package coverage:

``` r
devtools::check(quiet = TRUE)
#> ── R CMD check results ──────────────────────────────── sdlegviz 0.0.0.9000 ────
#> Duration: 14.8s
#> 
#> ❯ checking tests ...
#>   See below...
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
#> Loading required package: shiny
#> Saving _problems/test-mod_name_of_module1-27.R
#> Saving _problems/test-mod_name_of_module2-27.R
#> [ FAIL 2 | WARN 0 | SKIP 0 | PASS 74 ]
#> 
#> ══ Failed tests ════════════════════════════════════════════════════════════════
#> ── Error ('test-mod_name_of_module1.R:1:1'): (code run outside of `test_that()`) ──
#> Error in `eval(code, test_env)`: object 'mod_name_of_module1_server' not found
#> Backtrace:
#>     ▆
#>  1. └─shiny::testServer(...) at test-mod_name_of_module1.R:1:1
#>  2.   └─shiny:::isModuleServer(app)
#> ── Error ('test-mod_name_of_module2.R:1:1'): (code run outside of `test_that()`) ──
#> Error in `eval(code, test_env)`: object 'mod_name_of_module2_server' not found
#> Backtrace:
#>     ▆
#>  1. └─shiny::testServer(...) at test-mod_name_of_module2.R:1:1
#>  2.   └─shiny:::isModuleServer(app)
#> 
#> [ FAIL 2 | WARN 0 | SKIP 0 | PASS 74 ]
#> Error:
#> ! Test failures.
#> Execution halted
#> 
#> 1 error ✖ | 0 warnings ✔ | 1 note ✖
#> Error:
#> ! R CMD check found ERRORs
```

``` r
covr::package_coverage()
#> Error in `loadNamespace()`:
#> ! there is no package called 'covr'
```
