# NAMES FOR WATERSHED MENU ####

names <- dbReadTable(conn, "fwa_named_names")

# WSC STATIONS ####
# Use a simple bounding box for BC+Yukon instead of loading the complex
# bc_bound_buffer_100 geometry. This avoids an st_read() + WKT serialization
# of a large polygon and the spatial filter still captures all BC/YT stations.

bc_bbox_wkt <- "POLYGON((-141 48, -112 48, -112 62, -141 62, -141 48))"

q <- paste0(
  "SELECT stationnum, status, name, geom FROM wsc_stationloc
     WHERE ST_Intersects(geom, ST_GeomFromText('", bc_bbox_wkt, "', 4326))")
wsc_pp <- st_read(conn, query = q, quiet = TRUE)
wsc_pp <- wsc_pp %>%
  mutate(lon = st_coordinates(.)[, 1], lat = st_coordinates(.)[, 2])

# WSC TRAINING STATION IDS ####

stn_name_reg <-
  dbGetQuery(conn,
             "SELECT station_number FROM wsc_monthly_with_attri_dams_reg_20240204") %>%
  mutate(regulated = "Regulated Flow")
stn_name_nat <-
  dbGetQuery(conn,
             "SELECT station_number FROM wsc_monthly_with_attri_dams_nat_20240204") %>%
  mutate(regulated = "Natural Flow")

stn_training <- bind_rows(
  stn_name_reg %>%
    left_join(
      wsc_pp %>% rename(station_number = stationnum) %>% select(station_number, name),
      by = "station_number"
    ) %>%
    mutate(total_name = paste0(station_number, " - ", name, " - ", regulated)),
  stn_name_nat %>%
    left_join(
      wsc_pp %>% rename(station_number = stationnum) %>% select(station_number, name),
      by = "station_number"
    ) %>%
    mutate(total_name = paste0(station_number, " - ", name, " - ", regulated))
)

# 
# # NAMES FOR WATERSHED MENU ####
# 
#   names <- dbReadTable(conn, "fwa_named_names")
# 
# # BC BOUND ####
# 
#   bc_buffer <-
#     st_read(conn, layer = "bc_bound_buffer_100") %>% st_geometry() %>% st_as_text()
# 
# # WSC STATIONS ####
# 
#   q <-
#     paste0(
#       "SELECT stationnum, status, name, geom FROM wsc_stationloc WHERE ST_Intersects(geom,ST_GeomFromText('",
#       bc_buffer,
#       "', 4326))"
#     )
#   wsc_pp <- st_read(conn, query = q)
#   wsc_pp <-
#     wsc_pp %>% mutate(lon = st_coordinates(.)[, 1], lat = st_coordinates(.)[, 2])
# 
# # WSC TRAINING STATION IDS ####
# 
#   stn_name_reg <-
#     dbGetQuery(conn,
#                "SELECT station_number FROM wsc_monthly_with_attri_dams_reg_20240204") %>%
#     mutate(regulated = "Regulated Flow")
#   stn_name_nat <-
#     dbGetQuery(conn,
#                "SELECT station_number FROM wsc_monthly_with_attri_dams_nat_20240204") %>% mutate(regulated = "Natural Flow")
# 
#   stn_training <- bind_rows(
#     stn_name_reg %>%
#       left_join(
#         wsc_pp %>% rename(station_number = stationnum) %>% select(station_number, name),
#         by = "station_number"
#       ) %>%
#       mutate(total_name = paste0(station_number, " - ", name, " - ", regulated)),
#     stn_name_nat %>%
#       left_join(
#         wsc_pp %>% rename(station_number = stationnum) %>% select(station_number, name),
#         by = "station_number"
#       ) %>%
#       mutate(total_name = paste0(station_number, " - ", name, " - ", regulated))
#   )
# 
#   # GBR WS
#   
# #   ifw <- st_read(conn,query = paste0(
# #     "SELECT * FROM dcwbm_bsn_typed_ifw_1991_2020_100m"))
# #   
# #   # (coast <- st_read(conn, query = paste0(
# #   #   'SELECT * FROM dcwbm_bsn_typed_1991_2020_100m 
# #   #    WHERE "ORDER" > '))  )
# #   # 
# #   # coast <- st_read(conn, query = 
# #   #                    'SELECT ST_UnaryUnion(ST_Collect(geom)) AS geometry 
# #   #  FROM dcwbm_bsn_typed_1991_2020_100m 
# #   #  WHERE "ORDER" > 1')
# #   # 
# # source("functions_app/app_2_db.R")
# # 
# # dbExecute(conn, "SET statement_timeout = '1200s';")
# #   
# # coast <- st_read(conn, query = 'SELECT ST_UnaryUnion(geom) AS geometry 
# #                  FROM dcwbm_bsn_typed_1991_2020_100m
# #                  WHERE "ORDER" > 1')
# # 
# # mapview::mapview(coast)
# #   