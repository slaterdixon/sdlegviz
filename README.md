
<figure>
<img src="./docs/README_SS.png" alt="SDLegVizScreenshot" />
<figcaption aria-hidden="true">SDLegVizScreenshot</figcaption>
</figure>

# `{sdlegviz}`

A dashboard for visualizing sub-partisan coalitions in the South Dakota
legislature as unweighted graph networks, based on Zachary Neal’s
Stochastic Degree Sequence Model.

## Structure

This package is divided into two sections:

### Data Pipeline

The data pipeline (`data-raw`) combines data for each legislative from
Plural Policy’s Open States project (downloaded at
`./pipeline-data/raw`) with metadata from the South Dakota Legislative
Research Council to build legislators, bills, and roll call votes tables
for each session. The output is parquet files `inst/extdata`, which
includes projected unweighted backbones output by the `backbone` package
and legislature info.

Legislator info from Openstates is inconsistent, so an authoritative
list of legislators for each session are fetched from the LRC website
and joined to Openstates identifiers using last names. The pipeline
raises if it can not complete this join, requiring hand-keyed
reconciliation via .csv files at `./pipeline-data/handkeyed`

`process_data.R` runs the full pipeline.

### Dashboard

The shiny dashboard presents the extracted backbones as interactive
`visNetwork` graphs, with metadata. The dashboard is created using the
golem framework, which packages the dashboard as an R package.

## Run

``` r
golem::run_dev() # or source("dev/run_dev.R")
```

## Data

`source("data-raw/process_data.R")` rebuilds the app’s data in
`inst/extdata` from the raw OpenStates data, set by `BASE_PATH` in
`.env`.

## Deploy

See `dev/deploy.R`.
