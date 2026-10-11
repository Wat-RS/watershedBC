
# ws <- st_read("C:/Users/bevin/OneDrive - UNBC/1_RESEARCH/Watersheds - Great Bear Rainforest/CONTRACT - Bill/watershed_typing/coast/dcwbm_bsn_typed_1991_2020_100m.gpkg")
# ifw <- st_read("C:/Users/bevin/OneDrive - UNBC/1_RESEARCH/Watersheds - Great Bear Rainforest/CONTRACT - Bill/watershed_typing/ifw/ifw/dcwbm_bsn_typed_ifw_1991_2020_100m.gpkg")
ifw_3005 <- ifw |> st_transform(3005)

# st_write(ws, dsn = conn, layer = "dcwbm_bsn_typed_1991_2020_100m", delete_dsn = T)
# DBI::dbExecute(conn, statement = "CREATE INDEX dcwbm_bsn_typed_1991_2020_100m_geom_gist ON dcwbm_bsn_typed_1991_2020_100m USING gist (geom)")
# 
# st_write(ifw, dsn = conn, layer = "dcwbm_bsn_typed_ifw_1991_2020_100m", delete_dsn = T)
# DBI::dbExecute(conn, statement = "CREATE INDEX dcwbm_bsn_typed_ifw_1991_2020_100m_geom_gist ON dcwbm_bsn_typed_ifw_1991_2020_100m USING gist (geom)")




