# cons <- dbListConnections(
#   drv = RPostgres::dbDriver("PostgreSQL"),
#   dbname = Sys.getenv("aw_dbname"),
#   host = Sys.getenv("aw_host"),
#   port = Sys.getenv("aw_port"),
#   user = Sys.getenv("aw_user"),
#   password = Sys.getenv("aw_password")
# )
#
# lapply(cons, function(cc){print(cc)
#   dbDisconnect(cc)})



# CONNECT DATABASE ####

# conn <- pool::dbPool(
#   drv = RPostgres::dbDriver("PostgreSQL"),
#   dbname = Sys.getenv("aw_dbname"),
#   host = Sys.getenv("aw_host"),
#   port = Sys.getenv("aw_port"),
#   user = Sys.getenv("aw_user"),
#   password = Sys.getenv("aw_password")
  # )
conn <- pool::dbPool(
  drv = RPostgres::Postgres(),
  dbname = Sys.getenv("aw_dbname"),
  host = Sys.getenv("aw_host"),
  port = Sys.getenv("aw_port"),
  user = Sys.getenv("aw_user"),
  password = Sys.getenv("aw_password"),
  minSize = 1,
  maxSize = 10
)
# POINT ####

postgis_get_point <-
  
  function(to_clip = "dams",
           my_wkt = new_ws2_wkt) {
    q <-
      paste0(
        "SELECT w.*, ST_Intersection(w.geom,ST_GeomFromText('",
        my_wkt,
        "', 3005)) AS geom
                    FROM ",
        to_clip,
        " w WHERE ST_Intersects(w.geom,ST_GeomFromText('",
        my_wkt,
        "', 3005))"
      )
    o <-  st_read(conn, query = q, quiet = T)
    if (nrow(o) == 0) {
      o <-
        st_as_sf(data.frame(lon = -111, lat = 55),
                 coords = c("lon", "lat"),
                 crs = 4326) %>% mutate(type = "test") %>% filter(type != "test")
    }
    
    return(o)
  }

# LINE ####

postgis_get_line <-
  function(to_clip = "dra", my_wkt = new_ws2_wkt) {
    q <-
      paste0(
        "SELECT w.*, ST_SimplifyPreserveTopology(ST_Intersection(w.geom,ST_GeomFromText('",
        my_wkt,
        "', 3005)),5) AS geom
                    FROM ",
        to_clip,
        " w WHERE ST_Intersects(w.geom,ST_GeomFromText('",
        my_wkt,
        "', 3005))"
      )
    o <-  st_read(conn, query = q, quiet = T)
    if (nrow(o) > 0) {
      o <- o %>% filter(st_geometry_type(.) %in% c("LINESTRING")) %>%
        mutate(clipped_length_m = as.numeric(st_length(.)))
    } else{
      o <-
        st_as_sf(data.frame(lon = -111, lat = 55),
                 coords = c("lon", "lat"),
                 crs = 4326) %>%
        mutate(type = "test") %>% filter(type != "test")
    }
    
    return(o)
  }

# POL ####

postgis_get_pol <- function(to_clip = "fire",
                            to_clip_cols_to_keep = "*",
                            elev = "T",
                            my_wkt = new_ws2_wkt,
                            min_area_km2 = 0.01,
                            my_dem = dem,
                            grid_sizes = c(0, 0.001, 0.01, 0.1, 1)) {
  
  # ---- helper: build the query for a given precision grid ---------------
  build_q <- function(gs) {
    # gs = 0  -> no explicit grid (full double precision)
    # gs > 0  -> snap both inputs to a gs-metre grid (EPSG:3005 is metres)
    clip_geom <- if (gs > 0) {
      paste0("ST_ReducePrecision(ST_MakeValid(ST_GeomFromText('", my_wkt,
             "', 3005)), ", gs, ")")
    } else {
      paste0("ST_MakeValid(ST_GeomFromText('", my_wkt, "', 3005))")
    }
    
    w_geom <- if (gs > 0) {
      paste0("ST_ReducePrecision(ST_MakeValid(w.geom), ", gs, ")")
    } else {
      "ST_MakeValid(w.geom)"
    }
    
    paste0(
      "WITH clip AS (SELECT ", clip_geom, " AS geom) ",
      "SELECT ", to_clip_cols_to_keep, ", ",
      "  ST_CollectionExtract( ",
      "    ST_MakeValid( ST_Intersection(", w_geom, ", c.geom) ), 3) AS clip_geom ",
      "FROM ", to_clip, " w, clip c ",
      "WHERE ST_Intersects(w.geom, c.geom) ",
      "  AND NOT ST_IsEmpty(",
      "        ST_CollectionExtract(",
      "          ST_MakeValid( ST_Intersection(", w_geom, ", c.geom) ), 3))"
    )
  }
  
  # ---- read with escalating precision grid ------------------------------
  o <- NULL
  for (gs in grid_sizes) {
    o <- tryCatch(
      st_read(conn, query = build_q(gs), geometry_column = "clip_geom", quiet = TRUE),
      error = function(e) {
        message("ST_Intersection failed at gridSize = ", gs, ": ",
                conditionMessage(e))
        NULL
      }
    )
    if (!is.null(o)) break
  }
  
  empty_out <- function() {
    st_as_sf(data.frame(lon = -111, lat = 55),
             coords = c("lon", "lat"), crs = 4326) %>%
      mutate(type = "test") %>% filter(type != "test")
  }
  
  if (is.null(o) || nrow(o) == 0) return(empty_out())
  
  # drop the source geometry column if `*` pulled it in, keep the clipped one
  if ("geom" %in% names(o) && "clip_geom" %in% names(o)) {
    o <- o %>% dplyr::select(-geom)
  }
  names(o)[names(o) == "clip_geom"] <- "geom"
  o <- st_set_geometry(o, "geom")
  
  o <- o %>%
    st_make_valid() %>%
    filter(!st_is_empty(.)) %>%
    filter(st_geometry_type(.) %in% c("POLYGON", "MULTIPOLYGON")) %>%
    ms_explode() %>%
    mutate(clipped_area_m2 = as.numeric(st_area(.))) %>%
    filter(clipped_area_m2 > min_area_km2 * (1000 * 1000))
  
  if (nrow(o) == 0) return(empty_out())
  
  if (elev == "T") {
    o <- o %>%
      bind_cols(
        terra::extract(
          my_dem,
          o %>% st_centroid(of_largest_polygon = TRUE) %>% st_transform(4326)
        ) %>%
          dplyr::select(COP_GLO30_FOR_watershedBC_cog) %>%
          rename(elevation = COP_GLO30_FOR_watershedBC_cog)
      )
  }
  
  o <- o %>%
    st_cast("POLYGON") %>%
    mutate(type = to_clip) %>%
    arrange(-clipped_area_m2)
  
  return(o)
}
# postgis_get_pol <- function(to_clip = "fire",
#                             to_clip_cols_to_keep = "*",
#                             elev = "T",
#                             my_wkt = new_ws2_wkt,
#                             min_area_km2 = 0.01,
#                             my_dem = dem) {
#   
#   q <- paste0(
#     "SELECT w.*, ST_Intersection(w.geom,ST_GeomFromText('",
#     my_wkt,
#     "', 3005)) AS geom
#                    FROM ",
#     to_clip,
#     " w WHERE ST_Intersects(w.geom,ST_GeomFromText('",
#     my_wkt,
#     "', 3005))"
#   )
#   
#   o <-  st_read(conn, query = q, quiet = T)
#   
#   if (nrow(o) > 0) {
#     o <-
#       o %>% filter(st_geometry_type(.) %in% c("POLYGON", "MULTIPOLYGON")) %>%
#       ms_explode() %>% mutate(clipped_area_m2 = as.numeric(st_area(.))) %>%
#       filter(clipped_area_m2 > min_area_km2 * (1000 * 1000))
#     # print(o %>% head())
#     if (nrow(o) > 0) {
#       if (elev == "T") {
#         o <-
#           o %>%
#           bind_cols(
#             terra::extract(
#               my_dem,
#               o %>% st_centroid(of_largest_polygon = T) %>% st_transform(4326)
#             ) %>%
#               dplyr::select(COP_GLO30_FOR_watershedBC_cog) %>%
#               rename(elevation = COP_GLO30_FOR_watershedBC_cog)
#           )
#       }
#       o <- o  %>% #st_buffer(1) %>%
#         st_cast("POLYGON")
#     }
#   }
#   
#   if (nrow(o) > 0) {
#     o <- o %>% filter(clipped_area_m2 > 10) %>%
#       mutate(type = to_clip) %>%
#       arrange(-clipped_area_m2)
#     return(o)
#   } else{
#     o <-
#       st_as_sf(data.frame(lon = -111, lat = 55),
#                coords = c("lon", "lat"),
#                crs = 4326) %>% mutate(type = "test") %>% filter(type != "test")
#   }
#   
#   return(o)
# }

