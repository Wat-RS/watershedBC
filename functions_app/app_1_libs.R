# Programmatically install any missing packages used by the app
required_pkgs <- c(
  "ranger",
  # DATABASE
  "DBI", "RPostgreSQL", "RPostgres", "pool",
  # DATA
  "bcmaps", "bcdata", "elevatr",
  # UTILITIES
  "tictoc",
  # SPATIAL
  "sf", "terra", "rmapshaper","ggspatial","tidyterra",
  # PLOTS
  "ggplot2", "plotly",
  # SHINY
  "shiny", "shinyjs", "shinycssloaders", "callr",
  # LEAFLET
  "leaflet", "leafem", "leafgl",
  # WRANGLING
  "tidyr", "dplyr", "stringr", "DT", "lubridate", "readr", "forcats",
  # MODELS
  "randomForest", "shinyBS",
  # OTHER
  "webshot","webshot2","gridExtra",
  "ellipsis",'fansi','leaflet.extras'
)

if (!nzchar(Sys.getenv("SHINY_PORT"))) {
  missing_pkgs <- required_pkgs[!required_pkgs %in% rownames(installed.packages())]
  if (length(missing_pkgs) > 0) {
    install.packages(missing_pkgs, dependencies = TRUE, repos = "https://cran.rstudio.com")
  }
}

# install.packages("devtools")
# devtools::install_github("krlmlr/plogr")

suppressPackageStartupMessages({
  
  # library(ranger)       # loaded on demand in pred_Q_findRef()
  library(purrr)
  
  # DATABASE
  library(DBI)
  # library(RPostgreSQL)  # not used — pool uses RPostgres::Postgres()
  library(pool)
  
  # DATA
  # library(bcmaps)        # not used in app code — remove if confirmed
  # library(bcdata)        # all calls use bcdata::
  # library(elevatr)       # only used in pred_Q_prepWS() via elevatr::get_aws_points()
  
  # UTILITIES
  library(tictoc)
  
  # SPATIAL
  library(sf)
  library(terra)
  library(rmapshaper)
  # library(tidyterra)     # geom_spatraster() called as tidyterra:: (lazy, ~0.75s saved)
  # library(ggnewscale)    # not used in current code
  
  # PLOTS
  library(ggplot2)
  library(plotly)
  library(ggspatial)       # for annotation_scale(), annotation_north_arrow()
  
  # SHINY
  library(shiny)
  library(shinyjs)
  library(shinycssloaders)
  # library(callr)         # not used in current code
  
  # LEAFLET
  library(leaflet)
  # library(leafem)       # replaced by addMouseCoordinates() in app_5_leaflet.R (pulls in raster/sp, ~2s)
  # library(leafgl)        # not used in current code (addGlPoints etc.)
  
  # WRANGLING
  library(tidyr)
  library(dplyr)
  library(stringr)
  # library(DT)            # not used — tables use renderTable not renderDT
  library(lubridate)
  library(readr)
  library(forcats)
  
  # MODELS
  # library(randomForest)  # REMOVED — ranger is used for prediction, not randomForest
  # If your RDS models were trained with randomForest::randomForest(),
  # you'll need to keep this. Test and confirm.
  library(shinyBS)
  
  # library(webshot)       # only needed for PDF — load lazily if needed
  # webshot::install_phantomjs()
  
})

# ── Lazy-load helpers for packages only needed during processing ───────────────
# If elevatr is removed above, ensure app_4 calls elevatr::get_aws_points()
# with the namespace prefix instead of relying on library(elevatr).
