# DATABASE
library(DBI)
library(RPostgreSQL)
library(pool)

# SPATIAL
library(sf)

# CONNECT DATABASE ####

conn <- pool::dbPool(
  drv = RPostgres::dbDriver("PostgreSQL"),
  dbname = "postgres",
  host = "bcbasin.cycer7x7atqu.ca-central-1.rds.amazonaws.com",
  port = 5432,
  user = "bcgov_wlrs_morrisj",
  password = "PGisB3st!")


# STEP 1 ####

  # OPTION 1: SEARCH BY NAME FROM FWA NAMED WS

    my_named_ws <- st_read(conn, query = "SELECT * FROM fwa_named WHERE gnis_name = 'Birkenhead River'")
    mapview::mapview(my_named_ws)

  # OPTION 2: SEARCH BY COORDINATE FROM FWA 'ROLL UP' DATASET

    # my_named_ws <- st_read(conn, query = paste0("
    #   SELECT * FROM fwa_rollup
    #   WHERE ST_Intersects(geom, ST_Transform(ST_SetSRID(ST_MakePoint(",-122.725320,",",50.332266,"), 4326),3005))
    #   ORDER BY area_m2 ASC
    #   LIMIT 1"))
    # mapview::mapview(my_named_ws)

# STEP 2 ####

  # GET ALL SUB UNITS

    my_named_ws_wkt <- st_as_text(st_geometry(my_named_ws %>% ms_explode()))

      q <- paste0("SELECT w.*, ST_Intersection(w.geom,ST_GeomFromText('",my_named_ws_wkt,"', 3005)) AS geom FROM
                  fwa_rollup w WHERE ST_Intersects(w.geom,ST_GeomFromText('", my_named_ws_wkt,"', 3005))")

      (o <-  st_read(conn, query = q, quiet = T))

      ggplot() +
        geom_sf(data = o, aes(fill = as.numeric(WATERSHED_ORDER)), color = NA) +
        geom_sf(data = my_named_ws, fill = NA, color = "red") +
        scale_fill_gradientn(colours = hcl.colors(n = 10)) +
        labs(fill = "Order")
