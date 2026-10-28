library(sf)

setwd("D:/My Drive/")

# tids <- list.files(path = "BC_2026_20", pattern = "*.tif$")

outdir  <- tempdir()

lapply(2025, function(y){
  
  in_dir  <- paste0("BC_",y,"_20")
  
  vrt     <- file.path(outdir, paste0(in_dir, "_mosaic3.vrt"))
  out_tif <- file.path(outdir, paste0(in_dir, "_mosaic3.tif"))
  
  tifs <- list.files(in_dir, pattern = "\\.tif$", full.names = TRUE)
  
  # 1. Build the virtual raster (fast, no data copied)
  gdal_utils("buildvrt", source = tifs, destination = vrt,
             quiet = FALSE)
  
  # 2. Write a COG: tiled, compressed, with internal overviews
  gdal_utils(
    "translate", source = vrt, destination = out_tif,
    options = c(
      "-of", "COG",
      "-co", "COMPRESS=JPEG", 
      "-co", "QUALITY=85",
      "-co", "BIGTIFF=YES",
      "-co", "NUM_THREADS=ALL_CPUS",
      "-co", "OVERVIEW_RESAMPLING=AVERAGE"
    ),
    config_options = c(GDAL_CACHEMAX = "4096", GDAL_NUM_THREADS = "ALL_CPUS"),
    quiet = FALSE
    )
})
