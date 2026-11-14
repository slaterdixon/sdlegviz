# Rebuild the app data first if the raw data changed: source("data-raw/process_data.R")

# Check the package before deploying
devtools::test()
devtools::check()

# Deploy to Posit Connect Cloud. .rscignore keeps the pipeline, dev and test
# files out of the bundle, and .renvignore keeps them out of renv's dependency
# scan.
rsconnect::deployApp(
  appName = desc::desc_get_field("Package"),
  appTitle = "SD Legviz",
  appId = rsconnect::deployments(".")$appID,
  lint = FALSE,
  forceUpdate = TRUE
)
