library(bcdata)

wd <- "functions_prep/data/"
dir.create(wd)

bcdc_search("harvest")
cb <- bcdc_query_geodata("harvested-areas-of-bc-consolidated-cutblocks-") |> collect()
st_write(cb, paste0(wd,"cb_",gsub("-","",Sys.Date()),".gpkg"))

bcdc_search("fire polygon")
fire_current <- bcdc_query_geodata("bc-wildfire-fire-perimeters-current") |> collect()
fire_historical <- bcdc_query_geodata("bc-wildfire-fire-perimeters-historical") |> collect()
fire <- bind_rows(fire_current |> select(-VERSION_NUMBER), 
                  fire_historical |> select(-VERSION_NUMBER))
st_write(fire, paste0(wd,"fire_",gsub("-","",Sys.Date()),".gpkg"))

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
            user = Sys.getenv("aw_prep_user"),
            password = Sys.getenv("aw_prep_password"))
  }
conn <- refresh()

fire <- fire %>% select(fire_year = FIRE_YEAR, fire_number = FIRE_NUMBER, GEOMETRY = geometry) %>% 
  mutate(area_m2 = as.numeric(st_area(.))) %>%
  select(fire_year, fire_number, area_m2, geom = GEOMETRY) %>%
  clean_names()
fire %>% st_drop_geometry() %>% group_by(fire_year) %>% summarize(area_m2 = sum(area_m2, na.rm = T)) %>% ggplot() + geom_col(aes(fire_year, area_m2))
st_write(fire, dsn = conn, layer = "fire", delete_dsn = T)
st_read(dsn = conn, layer = "fire")

cb <- cb %>% select(harvest_year = HARVEST_START_YEAR_CALENDAR , 
              geom = geometry) %>% 
  mutate(area_m2 = as.numeric(st_area(.))) %>%
  select(harvest_year, area_m2, geom) %>%
  clean_names()
cb %>% st_drop_geometry() %>% group_by(harvest_year) %>% summarize(area_m2 = sum(area_m2, na.rm = T)) %>% ggplot() + geom_col(aes(harvest_year, area_m2))
st_write(cb, dsn = conn, layer = "cutblocks", delete_dsn = T)
st_read(dsn = conn, layer = "cutblocks")
