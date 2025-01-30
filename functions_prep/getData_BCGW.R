rm(list=ls())

library(janitor)
library(bcmaps)
library(bcdata)
library(sf)
library(DBI)
library(RPostgreSQL)
library(tidyr)
library(dplyr)
library(ggplot2)

# CONNECT TO DB ####

refresh <- function(){
  dbConnect(RPostgres::dbDriver("PostgreSQL"),
            dbname = Sys.getenv("aw_dbname"),
            host = Sys.getenv("aw_host"),
            port = Sys.getenv("aw_port"),
            user = Sys.getenv("aw_user"),
            password = Sys.getenv("aw_password"))}
conn <- refresh()

setwd("Z:/Data_watershedBC/")

# UPLOAD BASINS

  l <- list.files("ws_basins",full.names = T, pattern = "v4.sqlite")

  lapply(l, function(file){
    print(file)
    p <- st_read(file, quiet = T)
    st_write(p, dsn = conn, layer = "basinsv4", delete_dsn = F, append = T, quiet = T)
    return(file)
  })

  p <- st_read("ws_basins/basin_100_ws_v4.sqlite")
  st_write(p, dsn = conn, layer = "basins", delete_dsn = F, append = T)
  st_read(conn, query = "SELECT COUNT(*) FROM basins")

# UPLOAD FIRES ####

  fire_hist <- st_read("forest_wildfire/fire_historical.sqlite") %>% select(fire_year, fire_number)
  fire_curr <- st_read("forest_wildfire/fire_current.sqlite") %>% select(fire_year, fire_number)
  # bcdc_search("fire perimeter")
  fire_new_1 <- bcdc_query_geodata("bc-wildfire-fire-perimeters-historical") %>% filter(FIRE_YEAR > 2022) %>% collect() %>% filter(FIRE_YEAR > 2023)
  fire_new_2 <- bcdc_query_geodata("bc-wildfire-fire-perimeters-current") %>% filter(FIRE_YEAR > 2022) %>% collect() %>% filter(FIRE_YEAR > 2023)
  fire_new <- bind_rows(fire_new_1,fire_new_2)
  fire <- fire_new %>% select(fire_year = FIRE_YEAR, fire_number = FIRE_NUMBER, GEOMETRY = geometry) %>% bind_rows(fire_hist %>% bind_rows(fire_curr)) %>% mutate(area_m2 = as.numeric(st_area(.))) %>%
    select(fire_year, fire_number, area_m2, geom = GEOMETRY) %>%
    clean_names()
  fire %>% st_drop_geometry() %>% group_by(fire_year) %>% summarize(area_m2 = sum(area_m2, na.rm = T)) %>% ggplot() + geom_col(aes(fire_year, area_m2))
  st_write(fire, dsn = conn, layer = "fire", delete_dsn = T)

# UPLOAD CUTBLOCKS

  blocks <- st_read("forest_cutblocks/forest_cutblocks.gpkg", query = "SELECT HARVEST_YEAR, geom FROM cut_block_all_bc")
  blocks <- blocks %>% mutate(area_m2 = as.numeric(st_area(.))) %>%
    select(harvest_year = HARVEST_YEAR, area_m2, geom) %>%
    clean_names()
  st_write(blocks, dsn = conn, layer = "cutblocks", delete_dsn = F)

# UPLOAD FWA ####

  l <- st_layers("hydro_fwa/FWA_BC.gdb")
  l$name

  # NAMED WATERSHEDS
  named <- st_read("hydro_fwa/FWA_BC.gdb", layer = "FWA_NAMED_WATERSHEDS_POLY")
  named <- named %>% mutate(area_m2 = as.numeric(st_area(.))) %>%
    select(GNIS_NAME, GNIS_ID, STREAM_ORDER, STREAM_MAGNITUDE, area_m2, geom=GEOMETRY) %>%
    clean_names()
  st_write(named, dsn = conn, layer = "fwa_named", delete_dsn = F)
  st_read(conn, query = "SELECT COUNT(*) FROM fwa_named")

  # GLACIERS
  glaciers <- st_read("hydro_fwa/FWA_BC.gdb", layer = "FWA_GLACIERS_POLY")
  glaciers <- glaciers %>%
    mutate(area_m2 = as.numeric(st_area(.))) %>%
    select(WATERBODY_TYPE , area_m2, geom=GEOMETRY) %>%
    clean_names()
  st_write(glaciers, dsn = conn, layer = "fwa_glaciers", delete_dsn = T)
  st_read(conn, query = "SELECT COUNT(*) FROM fwa_glaciers")

  # WETLANDS
  wetlands <- st_read("hydro_fwa/FWA_BC.gdb", layer = "FWA_WETLANDS_POLY")
  wetlands <- wetlands %>%
    mutate(area_m2 = as.numeric(st_area(.))) %>%
    select(WATERBODY_TYPE , area_m2, geom=GEOMETRY) %>%
    clean_names()
  st_write(wetlands, dsn = conn, layer = "fwa_wetlands", delete_dsn = T)
  st_read(conn, query = "SELECT COUNT(*) FROM fwa_wetlands")

  # LAKES
  lakes <- st_read("hydro_fwa/FWA_BC.gdb", layer = "FWA_LAKES_POLY")
  lakes <- lakes %>%
    mutate(area_m2 = as.numeric(st_area(.))) %>%
    select(WATERBODY_TYPE , area_m2, geom=GEOMETRY) %>%
    clean_names()
  st_write(lakes, dsn = conn, layer = "fwa_lakes", delete_dsn = T)
  st_read(conn, query = "SELECT COUNT(*) FROM fwa_lakes")


library(terra)
dem <- bcmaps::cded_stars(bcmaps::bc_bound(), check_tiles = F)
dem_pts <- dem %>% as.data.frame()

dbWriteTable(conn, "cded_dem", dem, append = TRUE, row.names = FALSE, overwrite = FALSE, geom = "rast", driver = "PostgreSQL")
dbDisconnect(conn)


# UPDATE CUTBLOCKS
bcdc_search("cutblocks")
# cb_2024 <-
  bcdc_query_geodata("harvested-areas-of-bc-consolidated-cutblocks-") %>%
  filter(HARVEST_YEAR == 2024)
