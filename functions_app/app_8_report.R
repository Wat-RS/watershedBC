tempdir_path <- tempdir()
time         <- format(Sys.time(), "%Y%m%d_%H%M%S")
pdf_path     <- paste0(tempdir_path, "/", time, "_report.pdf")

render_report <- function(my_params) {
  withProgress(message = "Preparing PDF report...", max = 3, {
    incProgress(1, detail = "Building PDF...")
    
    if (file.exists(pdf_path)) file.remove(pdf_path)
    
    pdf(pdf_path, width = 11, height = 8.5, title = "watershedBC Report")
    
    # ── Cover page ──────────────────────────────────────────────────────────
    plot.new()
    text(0.5, 0.75, my_params$watershed_name, cex = 2.5, font = 2, adj = 0.5)
    text(0.5, 0.65, paste0("Area: ", round(my_params$watershed_area_km2, 1), " km²"), cex = 1.4, adj = 0.5)
    text(0.5, 0.58, paste0("Source: ", my_params$watershed_source), cex = 1.2, adj = 0.5)
    text(0.5, 0.50, paste0("Modules: ", paste(my_params$run_modules, collapse = ", ")), cex = 1, adj = 0.5)
    text(0.5, 0.42, paste0("Generated: ", format(Sys.time(), "%Y-%m-%d %H:%M")), cex = 1, adj = 0.5)
    text(0.5, 0.10, "watershedBC (v0.1 PROTOTYPE) — results not validated", cex = 0.8, col = "grey50", adj = 0.5)
    
    # ── Helper: print a ggplot if not null ───────────────────────────────────
    pg <- function(p) { if (!is.null(p)) print(p) }
    
    # ── Map ──────────────────────────────────────────────────────────────────
    pg(my_params$gg_map_static)
    
    # ── Streamflow & Freshwater ──────────────────────────────────────────────
    if ("Streamflow and Freshwater" %in% my_params$run_modules) {
      pg(my_params$gg_discharge)
      pg(my_params$gg_hyps)
      pg(my_params$gg_fwa)
    }
    
    # ── Forest Disturbance ───────────────────────────────────────────────────
    if ("Forest Disturbance" %in% my_params$run_modules) {
      pg(my_params$gg_timeseries)
      pg(my_params$gg_timeseries_cumsum)
      pg(my_params$gg_eca)
      pg(my_params$gg_elevbins)
      pg(my_params$gg_dra)
    }
    
    # ── CEF Human Disturbance ────────────────────────────────────────────────
    if ("CEF Human Disturbance - 2021" %in% my_params$run_modules) {
      pg(my_params$gg_cef_group)
      pg(my_params$gg_cef_flag)
    }
    
    # ── Stream Profile ───────────────────────────────────────────────────────
    if ("Stream Profile" %in% my_params$run_modules) {
      pg(my_params$gg_profile)
    }
    
    # ── Water Allocations ────────────────────────────────────────────────────
    if ("Water Allocations" %in% my_params$run_modules) {
      pg(my_params$gg_water_auth)
    }
    
    # ── ClimateBC ────────────────────────────────────────────────────────────
    if ("ClimateBC" %in% my_params$run_modules) {
      pg(my_params$gg_mat)
      pg(my_params$gg_map_clim)
      pg(my_params$gg_pas)
      pg(my_params$gg_cmd)
    }
    
    # ── Snow Remote Sensing ──────────────────────────────────────────────────
    if ("Snow Remote Sensing" %in% my_params$run_modules) {
      pg(my_params$p_snow_map)
      pg(my_params$p_snow_elev)
    }
    
    # ── Landslide Zonation ───────────────────────────────────────────────────
    if ("Landslide Zonation" %in% my_params$run_modules) {
      pg(my_params$gg_rlf_map)
      pg(my_params$gg_rlf_mapz)
      pg(my_params$gg_rlf_slp)
      pg(my_params$gg_rlf_slpz)
      pg(my_params$gg_rlf_ls)
    }
    
    # ── Satellite Imagery ────────────────────────────────────────────────────
    if ("Satellite Imagery" %in% my_params$run_modules) {
      if (!is.null(my_params$r1985)) terra::plotRGB(my_params$r1985, r=1, g=2, b=3, main="Landsat TM 1985–1990")
      if (!is.null(my_params$r2020)) terra::plotRGB(my_params$r2020, r=1, g=2, b=3, main="Landsat OLI 2020–2023")
      if (!is.null(my_params$r2023)) terra::plotRGB(my_params$r2023, r=1, g=2, b=3, main="Sentinel-2 2023")
    }
    
    dev.off()
    
    incProgress(2, detail = "Done.")
  })
}
# 
# # ── Report paths (set once at app startup) ────────────────────────────────────
# tempdir_path <- tempdir()
# time         <- format(Sys.time(), "%Y%m%d_%H%M%S")
# pdf_path     <- paste0(tempdir_path, "/", time, "_report.pdf")
# 
# # ── Main render function ───────────────────────────────────────────────────────
# render_report <- function(my_params) {
#   withProgress(message = "Preparing PDF report...", max = 3, {
#     incProgress(1, detail = "Rendering PDF...")
#     if (file.exists(pdf_path)) file.remove(pdf_path)
#     rmarkdown::render(
#       input       = "functions_report/report.Rmd",
#       output_dir  = tempdir_path,
#       output_file = paste0("/", time, "_report.pdf"),
#       params      = my_params,
#       envir       = new.env(parent = globalenv())
#     )
#     incProgress(2, detail = "Done.")
#   })
# }