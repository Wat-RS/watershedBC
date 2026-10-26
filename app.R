# LOAD FUNCTIONS #######################################################################

rm(list=ls())
source("functions_app/app_1_libs.R")
source("functions_app/app_2_db.R")
source("functions_app/app_3_load_dat.R")
source("functions_app/app_4_discharge_matchStn_forceUpdate.R")
source("functions_app/app_5_leaflet.R")
source("functions_app/app_7_tooltips.R")
source("functions_app/app_8_report.R")
source("functions_app/app_9_lidar_profile.R")
source("functions_monitoring/usage.R")

# Remote (S3 /vsicurl) raster reads: don't let GDAL list the bucket "directory" on every open,
# and cache fetched byte ranges within the process
terra::setGDALconfig("GDAL_DISABLE_READDIR_ON_OPEN", "EMPTY_DIR")
terra::setGDALconfig("CPL_VSIL_CURL_ALLOWED_EXTENSIONS", ".tif,.vrt")
terra::setGDALconfig("VSI_CACHE", "TRUE")
terra::setGDALconfig("AWS_NO_SIGN_REQUEST", "YES")
terra::setGDALconfig("AWS_DEFAULT_REGION", "ca-central-1")

# Snow probability VRT over 73 remote day-of-year COGs. Building it opens every file header,
# so it's built once per R process (on first use) and reused by every run.
snow_days <- str_pad(seq(1, 365, 5), 3, "left", 0)
snow_vrt_file <- NULL
get_snow_vrt <- function() {
  if (is.null(snow_vrt_file) || !file.exists(snow_vrt_file)) {
    f <- tempfile(fileext = ".vrt")
    terra::vrt(paste0("/vsicurl/https://bcbasin.s3.ca-central-1.amazonaws.com/snow_prob_bc/cal_doy_", snow_days, ".tif"),
               f, options = "-separate")
    snow_vrt_file <<- f
  }
  snow_vrt_file
}

# Distinct-visit count for the footer: one query per process every 10 min instead of a
# full scan of the usage table on every app open
visit_cache <- new.env()
get_visit_count <- function(max_age_s = 600) {
  if (is.null(visit_cache$n) || difftime(Sys.time(), visit_cache$at, units = "secs") > max_age_s) {
    n <- tryCatch(dbGetQuery(conn, "SELECT count(DISTINCT session_token)::int AS n FROM usage")$n,
                  error = function(e) NA)
    if (!is.na(n)) { visit_cache$n <- n; visit_cache$at <- Sys.time() }
  }
  visit_cache$n %||% NA   # on a failed refresh, keep showing the last good count
}

onStop(function() {
  cat("Closing Database Connections")
  pool::poolClose(conn)
})

# LANDSLIDE LAYER (load once at startup) ###############################################

library(leafgl)
library(htmlwidgets)

# Cached as RDS (much faster than parsing the CSV); delete the .rds to rebuild from the CSV
ls_csv <- "data/landslides/Canadian_landslide_database_May2026_version14.csv"
ls_rds <- sub("\\.csv$", ".rds", ls_csv)
if (file.exists(ls_rds)) {
  ls_pts <- readRDS(ls_rds)
} else {
  ls_pts <- read.csv(ls_csv) |>
    st_as_sf(coords = c("Longitude","Latitude"), crs = 4326) |> st_zm()
  ls_pts$Type[is.na(ls_pts$Type) | ls_pts$Type == ""] <- "Unknown"
  ls_pts$lab <- sprintf(
    "<b>%s</b><br>ID: %s<br>Type: %s<br>Material: %s<br>Timing: %s<br>Trigger: %s<br>Reference: %s",
    ls_pts$Name, ls_pts$LS_ID, ls_pts$Type, ls_pts$Material, ls_pts$Timing, ls_pts$Trigger, ls_pts$Reference)
  ls_pts <- ls_pts[, c("Type", "lab")]   # only these are used by the map
  try(saveRDS(ls_pts, ls_rds))
}

ls_pal <- colorFactor("viridis", domain = ls_pts$Type)

ls_xy         <- st_coordinates(ls_pts)
ls_levels     <- sort(unique(ls_pts$Type))
ls_tally_data <- list(
  lng    = unname(ls_xy[,1]), lat = unname(ls_xy[,2]), type = ls_pts$Type,
  levels = ls_levels,         colors = unname(ls_pal(ls_levels)))

# The ~31k landslide points (~5 MB) are only sent to the browser the first time the layer
# is switched on (the layer is hidden by default). The map JS asks for them via input$ls_load.
LS_GROUP <- "Landslides v14 (Brideau et al. 2026)"

add_landslide_points <- function(map) {
  leafgl::addGlPoints(map,
    data = ls_pts, group = LS_GROUP,
    fillColor = ls_pal(ls_pts$Type), radius = 10, fillOpacity = 0.8,
    popup = ls_pts$lab)
}

# Map groups ────────────────────────────────────────────────────────────────
BASE_GROUPS    <- c("BC Basemap","WorldImagery","WorldTopoMap")
REF_OVERLAYS   <- c(LS_GROUP, "WSC Active", "WSC Discontinued")
# Layers added by a run; cleared on each new selection/run
RESULT_GROUPS  <- c("Sub-basins","FWA Wetland","FWA Lake","FWA Glacier","Glacier 1985","Glacier 2021",
                    "Fire","Cutblock","Roads","Water Rights","Water Approvals")

# Display-only simplification (~10 m) before sending result geometry to the browser;
# analysis and downloads keep the full-resolution objects
map_simplify <- function(x, tol = 10) {
  x <- sf::st_as_sf(dplyr::ungroup(x))
  if (nrow(x) == 0) return(st_transform(x, 4326))
  x %>% st_transform(3005) %>% st_simplify(preserveTopology = TRUE, dTolerance = tol) %>% st_transform(4326)
}

# leaflet errors on a zero-length label vector (empty layer), so pass NULL instead
map_label <- function(x) if (length(x) == 0) NULL else x

# Helper: landslide in-view tally + lazy-load hook + collapsible legends for any leaflet map
add_landslides <- function(map) {
  map |>
    htmlwidgets::onRender("
      function(el, x) {
        var map = this, data = null, colMap = {}, requested = false;
        var LS_GROUP = 'Landslides v14 (Brideau et al. 2026)';

        // Tally data arrives with the points (session$sendCustomMessage('ls_tally', ...))
        window._lsOnData = function(d) {
          data = d; colMap = {};
          for (var i = 0; i < data.levels.length; i++) colMap[data.levels[i]] = data.colors[i];
          run();
        };
        if (!window._lsHandler && window.Shiny) {
          window._lsHandler = true;
          Shiny.addCustomMessageHandler('ls_tally', function(d) { if (window._lsOnData) window._lsOnData(d); });
        }

        // ---- Landslide tally control (collapsible) ----
        var ctl = L.control({position: 'topright'});
        ctl.onAdd = function() {
          var d = L.DomUtil.create('div', 'ls-tally leaflet-control');
          d.style.cssText = 'background:rgba(255,255,255,0.92);border-radius:4px;' +
            'box-shadow:0 1px 4px rgba(0,0,0,0.3);font:12px/1.4 sans-serif;min-width:170px';
          L.DomEvent.disableScrollPropagation(d);
          L.DomEvent.disableClickPropagation(d);
          var hdr = L.DomUtil.create('div', '', d);
          hdr.style.cssText = 'cursor:pointer;padding:6px 10px;font-weight:bold;' +
            'display:flex;align-items:center;justify-content:space-between';
          var title = L.DomUtil.create('span', '', hdr);
          var caret = L.DomUtil.create('span', '', hdr);
          caret.style.marginLeft = '8px'; caret.innerHTML = '&#9662;';
          var body = L.DomUtil.create('div', '', d);
          body.style.cssText = 'padding:0 10px 8px 10px;max-height:300px;overflow-y:auto';
          var collapsed = window.innerWidth <= 768;   // start collapsed on phones
          if (collapsed) { body.style.display = 'none'; caret.innerHTML = '&#9656;'; }
          hdr.onclick = function() {
            collapsed = !collapsed;
            body.style.display = collapsed ? 'none' : 'block';
            caret.innerHTML = collapsed ? '&#9656;' : '&#9662;';
          };
          this._title = title; this._body = body;
          return d;
        };
        ctl.addTo(map);

        function update() {
          if (!data) { ctl._title.textContent = 'Loading landslides...'; ctl._body.innerHTML = ''; return; }
          var b = map.getBounds(), counts = {}, total = 0;
          for (var i = 0; i < data.lat.length; i++) {
            if (b.contains([data.lat[i], data.lng[i]])) {
              counts[data.type[i]] = (counts[data.type[i]] || 0) + 1; total++;
            }
          }
          var rows = Object.keys(counts).sort(function(a,b){return counts[b]-counts[a];});
          ctl._title.textContent = 'Landslides in view: ' + total.toLocaleString();
          var html = '';
          rows.forEach(function(t){
            html += '<div style=\"display:flex;align-items:center;margin:2px 0\">' +
              '<span style=\"width:10px;height:10px;border-radius:50%;margin-right:6px;' +
              'background:' + (colMap[t]||'#999') + ';display:inline-block\"></span>' +
              '<span style=\"flex:1\">' + t + '</span>' +
              '<b style=\"margin-left:8px\">' + counts[t].toLocaleString() + '</b></div>';
          });
          ctl._body.innerHTML = html;
        }
        var run = update.bind(ctl);
        map.on('moveend', run); run();

        // ---- Request the points from the server the first time the layer is switched on ----
        map.on('overlayadd', function(e){
          if (e.name === LS_GROUP && !requested && window.Shiny) {
            requested = true;
            Shiny.setInputValue('ls_load', Date.now(), {priority: 'event'});
          }
        });

        // ---- Tie tally visibility to the Landslides layer toggle (off by default) ----
        var tallyEl = ctl.getContainer();
        tallyEl.style.display = 'none';
        map.on('overlayadd',    function(e){ if (e.name === 'Landslides v14 (Brideau et al. 2026)'){ tallyEl.style.display = 'block'; run(); } });
        map.on('overlayremove', function(e){ if (e.name === 'Landslides v14 (Brideau et al. 2026)'){ tallyEl.style.display = 'none'; } });

        // ---- Make standard leaflet legends collapsible (lidar, Points, Polygons, etc.) ----
        function makeLegendsCollapsible() {
          var legs = el.querySelectorAll('.leaflet-control.legend, .info.legend');
          for (var i = 0; i < legs.length; i++) {
            (function(lg){
              if (lg._collapsible) return;
              lg._collapsible = true;
              lg.style.position = 'relative';
              var wrap = document.createElement('div');
              while (lg.firstChild) wrap.appendChild(lg.firstChild);
              var caret = document.createElement('div');
              caret.innerHTML = '&#9662;';
              caret.style.cssText = 'position:absolute;top:2px;right:6px;cursor:pointer;' +
                'font-weight:bold;z-index:1';
              lg.appendChild(caret); lg.appendChild(wrap);
              var collapsed = window.innerWidth <= 768;   // start collapsed on phones
              wrap.style.display = collapsed ? 'none' : 'block';
              caret.innerHTML = collapsed ? '&#9656;' : '&#9662;';
              L.DomEvent.disableClickPropagation(caret);
              caret.onclick = function(){
                collapsed = !collapsed;
                wrap.style.display = collapsed ? 'none' : 'block';
                caret.innerHTML = collapsed ? '&#9656;' : '&#9662;';
              };
            })(legs[i]);
          }
        }
        makeLegendsCollapsible();
        map.on('overlayadd', makeLegendsCollapsible);
        map.on('overlayremove', makeLegendsCollapsible);
        // legends added after a run arrive via leafletProxy, so watch the control corners too
        var corners = el.querySelector('.leaflet-control-container');
        if (corners && window.MutationObserver)
          new MutationObserver(makeLegendsCollapsible).observe(corners, {childList: true, subtree: true});
      }
    ")
}

# Session expiry countdown (footer). The server keepalive holds the session for keepalive_s;
# after that shinyapps.io drops it after idle_timeout_s with no traffic (instance idle
# timeout setting), so any input/output resets the clock. This is an estimate, client-side.
keepalive_s    <- 15 * 60
idle_timeout_s <- 15 * 60
session_countdown_js <- tags$script(HTML(sprintf("
  (function(){
    var keepaliveEnd = null, lastActivity = Date.now(), timer = null;
    var idleMs = %d * 1000;
    function touch(){ lastActivity = Date.now(); }
    function tick(){
      var el = document.getElementById('session-countdown');
      if (!el || keepaliveEnd === null) return;
      if (Date.now() < keepaliveEnd) { el.textContent = ''; return; }   // hidden while keepalive holds the session
      var left = Math.max(0, Math.max(keepaliveEnd, lastActivity) + idleMs - Date.now());
      var m = Math.floor(left / 60000), s = Math.floor(left %% 60000 / 1000);
      el.textContent = ' · session expires in ~' + m + ':' + (s < 10 ? '0' : '') + s;
      el.style.color = left < 5 * 60000 ? '#c0392b' : '';
    }
    $(document).on('shiny:connected', function(){
      keepaliveEnd = Date.now() + %d * 1000; touch(); tick();
      timer = setInterval(tick, 1000);
    });
    $(document).on('shiny:inputchanged shiny:value', touch);
    $(document).on('shiny:disconnected', function(){
      clearInterval(timer);
      var el = document.getElementById('session-countdown');
      if (el) el.textContent = ' · session expired';
    });
  })();", idle_timeout_s, keepalive_s)))

# UI ###########################################################################

# new_ws2 <- st_read(conn, query = "SELECT * FROM fwa_named WHERE gnis_name = 'Bowser River'") %>% mutate(area_km2 = area_m2/(1000*1000))
# new_ws2 <- st_read(conn, query = "SELECT * FROM fwa_named WHERE gnis_name = 'Bowron River'") %>% mutate(area_km2 = area_m2/(1000*1000))
# new_ws2 <- st_read(conn, query = "SELECT * FROM fwa_named WHERE gnis_name = 'Nithi River'") %>% mutate(area_km2 = area_m2/(1000*1000))
# new_ws2 <- st_read(conn, query = "SELECT * FROM fwa_named WHERE gnis_name = 'Parsnip River'") %>% mutate(area_km2 = area_m2/(1000*1000))

ui <- function(request)   # function of request so URL bookmarks restore inputs

  tagList(useShinyjs(),
          disconnectMessage(text = "Your session timed out or lost its connection. Reload to return to your selected watershed.",
                            refresh = "Reload"),
          navbarPage(theme = "css/wrsl.css",
                     title = tags$img(src = "images/watershedbc-wordmark.svg", class = "navbar-logo",
                                      style = "max-height:30px;", alt = "watershedBC"),
                     windowTitle = "watershedBC",
                     collapsible = TRUE,
                     header = tagList(
                     tags$head(
                       map_head_tags,
                       tags$script(HTML("document.addEventListener('DOMContentLoaded', function(){
                                        if (window.innerWidth < 768) document.body.classList.add('side-collapsed'); });")),
                       tags$link(rel = "icon", href = "images/watrs-favicon.png"),
                       session_countdown_js),
                     tags$style(HTML(".table-scroll {max-height: 200px; overflow-y: auto;}
                              .nav-tabs > li > a { font-size: 13px; padding: 6px 10px; }
                              .caption-box { background: #f8f9fa; border-left: 4px solid #337ab7;
                                            padding: 10px 14px; margin: 6px 0 18px 0;
                                            font-size: 12px; color: #444; line-height: 1.5; }
                              .btn-default.shiny-download-link {display: block; width: 100%; margin-bottom: 4px;
                                                                font-size: 12px; padding: 4px 10px; text-align: left;}
                              downloadAllSpatial {background-color: #337ab7; color: #fff; border-color: #2e6da4;
                                                   font-weight: bold; margin-bottom: 8px; }
                              .downloads-section h5 { margin-top: 10px; margin-bottom: 6px; }"))),
                     
                     tabPanel(title = "watershedBC Prototype",
                              
                              # modalDialog(HTML(tooltip_startup_popup),
                              #             title = "Disclaimer: watershedBC is a prototype",
                              #             size = "m", easyClose = FALSE,
                              #             footer = modalButton("I accept the disclaimer")),
                              
                              #### ROW ####
                              shiny::fluidRow(
                                
                                # ── LEFT SIDEBAR ──────────────────────────────────────────────────────
                                shiny::column(width = 2, 
                                              id = "side_col",
                                              shiny::h3("Get started"),
                                              shiny::HTML("<b>1</b> — Click anywhere on the map<br>
                                                           <b>2</b> — Confirm the delineation pop-up<br>
                                                           <b>3</b> — Select modules and click <b>Run Report</b>"), br(), br(),
                                              
                                              shiny::selectizeInput(inputId = "psql_zoom_to_name",
                                                                    label = "Or search by watershed name",
                                                                    choices = NULL, selected = "", multiple = FALSE),
                                              shiny::actionButton(inputId = "zoom_to_button", label = "Zoom to..",
                                                                  icon("paper-plane"),
                                                                  style = "color: #fff; background-color: #337ab7; border-color: #2e6da4"),
                                              br(), br(),
                                              
                                              shiny::radioButtons(
                                                inputId  = "watershed_source",
                                                label    = "Watershed data source",
                                                selected = "Freshwater Atlas Named Watersheds",
                                                choices  = c("Freshwater Atlas Named Watersheds",
                                                             "Freshwater Atlas by Stream Order",
                                                             "Custom Basin at Point of Interest",
                                                             "Water Survey of Canada Basins",
                                                             "Coastal Watersheds with Typing",
                                                             "Coastal Important Fisheries Watersheds")),
                                              
                                              # ── Watershed source tooltips ────────────────────────────────────── ####
                                              radioTooltip(id = "watershed_source",
                                                           choice = "Freshwater Atlas Named Watersheds",
                                                           title  = paste0("Named drainage polygons from BC's Freshwater Atlas (FWA). ",
                                                                           "~11,500 named systems province-wide. Each polygon represents the area ",
                                                                           "draining to a named reach. Best starting point for most analyses. ",
                                                                           "Click the map to select the smallest named watershed at that point."),
                                                           placement = "right", trigger = "hover"),
                                              
                                              radioTooltip(id = "watershed_source",
                                                           choice = "Freshwater Atlas by Stream Order",
                                                           title  = paste0("FWA watershed units summarised by Strahler stream order (~1.35 million units). ",
                                                                           "Smaller polygons in headwaters, larger polygons near mainstems. ",
                                                                           "Use for detailed sub-watershed delineation where named polygons are too coarse."),
                                                           placement = "right", trigger = "hover"),
                                              
                                              radioTooltip(id = "watershed_source",
                                                           choice = "Custom Basin at Point of Interest",
                                                           title  = paste0("Pre-delineated drainage basins at ~1 km pour-point spacing across BC (~946k basins). ",
                                                                           "Delineated with WhiteboxTools from the Copernicus GLO-30 DEM. ",
                                                                           "Use when your point of interest does not coincide with a named stream or WSC gauge."),
                                                           placement = "right", trigger = "hover"),
                                              
                                              radioTooltip(id = "watershed_source",
                                                           choice = "Water Survey of Canada Basins",
                                                           title  = paste0("Official drainage basins for Water Survey of Canada (WSC) hydrometric stations ",
                                                                           "(~2,900 gauged basins in BC and Yukon). ",
                                                                           "Clicking the map selects the nearest active or discontinued WSC gauge basin. ",
                                                                           "Best for comparing modelled discharge against observed streamflow records."),
                                                           placement = "right", trigger = "hover"),
                                              
                                              radioTooltip(id = "watershed_source",
                                                           choice = "Coastal Watersheds with Typing",
                                                           title  = paste0("Coastal watershed basins from the Detailed Coastal Watershed Basin Model (DCWBM) ",
                                                                           "with hydrological regime typing (rain-dominated, hybrid, nival, glacial) ",
                                                                           "based on 1991–2020 climate normals at 100 m resolution. ",
                                                                           "Developed by the BC Ministry of Forests for the coastal hydrology program."),
                                                           placement = "right", trigger = "hover"),
                                              
                                              radioTooltip(id = "watershed_source",
                                                           choice = "Coastal Important Fisheries Watersheds",
                                                           title  = paste0("Important Fisheries Watersheds (IFW) along the BC coast — a subset of the DCWBM ",
                                                                           "coastal basins identified as priority systems for Pacific salmon and other fish species. ",
                                                                           "Includes watershed typing and key biological attributes."),
                                                           placement = "right", trigger = "hover"),
                                              
                                              br(),
                                              
                                              # ── Module checkboxes ────────────────────────────────────────────── ####
                                              shiny::checkboxGroupInput(
                                                inputId  = "run_modules",
                                                label    = "Modules to include in report",
                                                selected = c("Streamflow and Freshwater", "Forest Disturbance"
                                                             # "CEF Human Disturbance - 2021", "Stream Profile",
                                                             # "Water Allocations", "ClimateBC",
                                                             # "Snow Remote Sensing", "Landslide Zonation", "Satellite Imagery"
                                                ),
                                                choices  = c("Streamflow and Freshwater", "Forest Disturbance",
                                                             "CEF Human Disturbance - 2021", "Stream Profile",
                                                             "Water Allocations", "ClimateBC",
                                                             "Snow Remote Sensing", "Landslide Zonation", "Satellite Imagery"
                                                )),
                                              
                                              # ── Module tooltips ──────────────────────────────────────────────── ####
                                              radioTooltip(id = "run_modules", choice = "Streamflow and Freshwater",
                                                           title = paste0("Estimates mean annual and daily discharge by scalling a similar WSC gauge to the watershed of interest."),
                                                           placement = "right", trigger = "hover"),
                                              
                                              radioTooltip(id = "run_modules", choice = "Forest Disturbance",
                                                           title = paste0("Retrieves wildfire perimeters and consolidated cutblock polygons from 1900–present. ",
                                                                          "Computes annual and cumulative disturbed area, and a Simplified Equivalent Clearcut ",
                                                                          "Area (ECA) index assuming 60-year recovery for both fire and harvest."),
                                                           placement = "right", trigger = "hover"),
                                              
                                              radioTooltip(id = "run_modules", choice = "CEF Human Disturbance - 2021",
                                                           title = paste0("The Cumulative Effects Framework (CEF) consolidated human disturbance footprint for 2021. ",
                                                                          "Integrates roads, cutblocks, pipelines, transmission lines, urban areas, and other ",
                                                                          "anthropogenic features into a single spatial layer. Source: BC Data Catalogue."),
                                                           placement = "right", trigger = "hover"),
                                              
                                              radioTooltip(id = "run_modules", choice = "Stream Profile",
                                                           title = paste0("Extracts longitudinal elevation profiles for the main stem and up to five major tributaries ",
                                                                          "using XYZ vertex coordinates embedded in the Freshwater Atlas stream network. "),
                                                           placement = "right", trigger = "hover"),
                                              
                                              radioTooltip(id = "run_modules", choice = "Water Allocations",
                                                           title = paste0("Retrieves current water licences (Water Rights Licences) and approvals (Water Approval Points) ",
                                                                          "within the watershed from the BC Water Licensing system. "),
                                                           placement = "right", trigger = "hover"),
                                              
                                              radioTooltip(id = "run_modules", choice = "ClimateBC",
                                                           title = paste0("Extracts watershed-averaged climate normals for four periods (1961–1990 to 1991–2020) ",
                                                                          "and future projections under SSP1-2.6, SSP2-4.5, SSP3-7.0, and SSP5-8.5 scenarios from ",
                                                                          "ClimateBC (Wang et al. 2016) at 800 m resolution. Variables: MAT, MAP, PAS, CMD."),
                                                           placement = "right", trigger = "hover"),
                                              
                                              radioTooltip(id = "run_modules", choice = "Snow Remote Sensing",
                                                           title = paste0("Day-of-year snow cover probability by elevation and snow season duration from MODIS"),
                                                           placement = "right", trigger = "hover"),
                                              
                                              radioTooltip(id = "run_modules", choice = "Landslide Zonation",
                                                           title = paste0("Preliminary rule-based landslide susceptibility model combining mean annual precipitation and terrain slope (%)"),
                                                           placement = "right", trigger = "hover"),
                                              
                                              radioTooltip(id = "run_modules", choice = "Satellite Imagery",
                                                           title = paste0("Cloud-free satellite imagery from 1985–1990, 2020–2023 and 2023"),
                                                           placement = "right", trigger = "hover"),
                                              br(),
                                              shiny::actionButton(inputId = "run_button", label = "Run Report",
                                                                  icon("paper-plane"),
                                                                  style = "color: #fff; background-color: #337ab7; border-color: #2e6da4"),
                                              br(), br(),
                                              textOutput(outputId = "ws_run"),
                                              textOutput(outputId = "ws_selection_pred_time"),
                                              br(), br(),
                                              
                                              shinyjs::hidden(downloadButton("downloadPDF",         "Export Report PDF")),
                                              shinyjs::hidden(downloadButton("downloadAllSpatial",  "Download all spatial data (.zip)"))
                                ),
                                
                                # ── MAIN PANEL ──────────────────────────────────────────────────────── ####
                                shiny::column(width = 10, id = "main_col",
                                              
                                              tags$button(id = "toggle_side", class = "btn btn-default btn-sm",
                                                          style = "margin-bottom:6px;",
                                                          onclick = "document.body.classList.toggle('side-collapsed');
                                                                     setTimeout(function(){ window.dispatchEvent(new Event('resize')); }, 50);",
                                                          icon("bars"), "Menu"),
                                              
                                              leafletOutput("mymap", height = "700px") %>% withSpinner(color = "steelblue"),
                                              
                                              map_toolbar,
                                              profile_panel_ui,
                                              # NOTE: "Delineation on map click" checkbox removed.
                                              # Clicking the map now triggers a confirmation pop-up automatically.
                                              
                                              br(),
                                              h3(htmlOutput(outputId = "ws_selection")),
                                              
                                              #### RESULTS TABSET ####
                                              tabsetPanel(id = "module_tabs", type = "tabs",
                                                          
                                                          # ── Streamflow & Freshwater ──────────────────────────────────────####
                                                          tabPanel("Streamflow & Freshwater",
                                                                   br(),
                                                                   htmlOutput("warn_discharge_ref"),
                                                                   radioButtons("discharge_step", NULL, choices = c("Daily", "Weekly", "Monthly"),
                                                                                inline = TRUE),
                                                                   plotlyOutput("plot_discharge") %>% withSpinner(color = "steelblue"),
                                                                   htmlOutput("text_plot_discharge"),
                                                                   br(),
                                                                   shiny::selectInput(inputId = "plot_discharge_site_sel",
                                                                                      label   = "WSC reference station (ranked by model match score)",
                                                                                      choices = setNames(stn_training$station_number, stn_training$total_name),
                                                                                      width = "600px"),
                                                                   htmlOutput("text_discharge_ref"),
                                                                   div(class = "table-scroll", tableOutput("table_discharge_ref")),
                                                                   leafletOutput("map_discharge_ref", height = "400px", width = "600px") %>% withSpinner(color = "steelblue"),
                                                                   br(),
                                                                   div(class = "table-scroll", tableOutput("table_named")),
                                                                   htmlOutput("text_table_named"),
                                                                   br(),
                                                                   plotlyOutput("plot_hypsometry") %>% withSpinner(color = "steelblue"),
                                                                   htmlOutput("text_plot_hypsometry"),
                                                                   br(),
                                                                   plotlyOutput("plot_fwa") %>% withSpinner(color = "steelblue"),
                                                                   htmlOutput("text_plot_fwa"),
                                                                   br(), br()
                                                          ),
                                                          # ── Water Allocations ────────────────────────────────────────────####
                                                          tabPanel("Water Allocations",
                                                                   br(),
                                                                   plotlyOutput("plot_auth") %>% withSpinner(color = "steelblue"),
                                                                   htmlOutput("text_plot_auth"),
                                                                   br(), br()
                                                          ),
                                                          # ── Stream Profile ───────────────────────────────────────────────####
                                                          tabPanel("Stream Profile",
                                                                   br(),
                                                                   plotlyOutput("plot_profile") %>% withSpinner(color = "steelblue"),
                                                                   htmlOutput("text_plot_profile"),
                                                                   br(), br()
                                                          ),
                                                          # ── Forest Disturbance ───────────────────────────────────────────####
                                                          tabPanel("Forest Disturbance",
                                                                   br(),
                                                                   plotlyOutput("plot_timeseries") %>% withSpinner(color = "steelblue"),
                                                                   htmlOutput("text_plot_timeseries"),
                                                                   br(),
                                                                   plotlyOutput("plot_timeseries_cumsum") %>% withSpinner(color = "steelblue"),
                                                                   htmlOutput("text_plot_timeseries_cumsum"),
                                                                   br(),
                                                                   plotlyOutput("plot_eca") %>% withSpinner(color = "steelblue"),
                                                                   htmlOutput("text_plot_eca"),
                                                                   br(),
                                                                   plotlyOutput("plot_elevbins") %>% withSpinner(color = "steelblue"),
                                                                   htmlOutput("text_plot_elevbins"),
                                                                   br(),
                                                                   plotlyOutput("plot_dra") %>% withSpinner(color = "steelblue"),
                                                                   htmlOutput("text_plot_dra"),
                                                                   br(), br()
                                                          ),
                                                          
                                                          # ── CEF Human Disturbance ────────────────────────────────────────####
                                                          tabPanel("CEF Human Disturbance",
                                                                   br(),
                                                                   plotlyOutput("plot_cef_group") %>% withSpinner(color = "steelblue"),
                                                                   plotlyOutput("plot_cef_group_flag") %>% withSpinner(color = "steelblue"),
                                                                   htmlOutput("text_plot_cef_group_flag"),
                                                                   br(), br()
                                                          ),
                                                          
                                                          # ── ClimateBC ────────────────────────────────────────────────────####
                                                          tabPanel("ClimateBC",
                                                                   br(),
                                                                   plotlyOutput("plot_mat") %>% withSpinner(color = "steelblue"), htmlOutput("text_plot_mat"), br(),
                                                                   plotlyOutput("plot_map") %>% withSpinner(color = "steelblue"), htmlOutput("text_plot_map"), br(),
                                                                   plotlyOutput("plot_pas") %>% withSpinner(color = "steelblue"), htmlOutput("text_plot_pas"), br(),
                                                                   plotlyOutput("plot_cmd") %>% withSpinner(color = "steelblue"), htmlOutput("text_plot_cmd"),
                                                                   br(), br()
                                                          ),
                                                          
                                                          # ── Snow Remote Sensing ──────────────────────────────────────────####
                                                          tabPanel("Snow Remote Sensing",
                                                                   br(),
                                                                   plotlyOutput("plot_snowrs_plot", width = 800, height = 600) %>% withSpinner(color = "steelblue"),
                                                                   htmlOutput("text_plot_snowrs_elev"),
                                                                   br(),
                                                                   plotOutput("plot_snowrs_map",  width = 800, height = 600) %>% withSpinner(color = "steelblue"),
                                                                   htmlOutput("text_plot_snowrs_map"),
                                                                   br(), br()
                                                          ),
                                                          
                                                          # ── Landslide Zonation ─────────────────────────────────────────── ####
                                                          tabPanel("Landslide Zonation",
                                                                   br(),
                                                                   fluidRow(plotOutput("plot_rlf_map",  width = 800, height = 400) %>% withSpinner(color = "steelblue"),
                                                                            plotOutput("plot_rlf_mapz", width = 800, height = 400) %>% withSpinner(color = "steelblue")),
                                                                   htmlOutput("text_plot_rlf_map"),
                                                                   br(),
                                                                   fluidRow(plotOutput("plot_rlf_slp",  width = 800, height = 400) %>% withSpinner(color = "steelblue"),
                                                                            plotOutput("plot_rlf_slpz", width = 800, height = 400) %>% withSpinner(color = "steelblue")),
                                                                   htmlOutput("text_plot_rlf_slp"),
                                                                   br(),
                                                                   plotOutput("plot_rlf_ls",   width = 800, height = 400) %>% withSpinner(color = "steelblue"),
                                                                   htmlOutput("text_plot_rlf"),
                                                                   br(), br()
                                                          ),
                                                          
                                                          # ── Satellite Imagery ──────────────────────────────────────────── ####
                                                          tabPanel("Satellite Imagery",
                                                                   br(),
                                                                   plotOutput("plot_landsat_1985",  width = 800, height = 800) %>% withSpinner(color = "steelblue"),
                                                                   htmlOutput("text_plot_landsat_1985"),
                                                                   br(),
                                                                   plotOutput("plot_landsat_2020",  width = 800, height = 800) %>% withSpinner(color = "steelblue"),
                                                                   htmlOutput("text_plot_landsat_2020"),
                                                                   br(),
                                                                   plotOutput("plot_sentinel_2023", width = 800, height = 800) %>% withSpinner(color = "steelblue"),
                                                                   htmlOutput("text_plot_sentinel_2023"),
                                                                   br(), br()
                                                          )
                                                          
                                              ) # end tabsetPanel
                                )   # end column width=10
                              )),
                     tabPanel(title = "Disclaimer", HTML(tooltip_startup_popup)),
                     
                     tabPanel(title = "Usage",
                              shiny::fluidRow(
                                shiny::column(width = 12,
                                              plotOutput("usage_visits", width = 800, height = 400) %>% withSpinner(color = "steelblue"),
                                              plotOutput("usage_recent", width = 800, height = 800) %>% withSpinner(color = "steelblue"),
                                              plotOutput("usage_top",    width = 800, height = 800) %>% withSpinner(color = "steelblue"),
                                              plotOutput("usage_total",  width = 800, height = 800) %>% withSpinner(color = "steelblue")))),
                     
                     navbarMenu("Other experimental apps",
                                HTML("<a href='http://bcgov-env.shinyapps.io/nbchydro/' target='_blank'>Northern BC Hydrology Research</a>"),
                                HTML("<a href='http://bcgov-env.shinyapps.io/nbcclim/' target='_blank'>Northern BC Climate Research</a>"),
                                HTML("<a href='http://bcgov-env.shinyapps.io/bcws_wx/' target='_blank'>BC Wildfire Service Weather Station Data Visualization</a>"),
                                HTML("<a href='http://bcgov-env.shinyapps.io/bc_gw_tools/' target='_blank'>Groundwater Well Tools</a>")),    # end fluidRow
                     
                     #### FOOTER ####
                     footer = tagList(
                     shiny::fluidRow(
                       shiny::column(width = 12,
                                     HTML("watershedBC is a <b>prototype</b> — results are not validated and should not be used for regulatory or engineering decisions.<br>
                                                     <b>More info:</b> <a href='https://github.com/Wat-RS/watershedBC/' target='_blank'>https://github.com/Wat-RS/watershedBC/</a> |
                                                     Contact: <i>alex.bevington@unbc.ca</i>"), br())),
                     
                     shiny::fluidRow(
                       shiny::column(width = 12, class = "wrsl-footer",
                                     HTML("<a href='https://watershedlab.ca' target='_blank'>Watershed Remote Sensing Lab</a>,
                                           University of Northern British Columbia"),
                                     textOutput("visit_count", inline = TRUE),
                                     tags$span(id = "session-countdown"))))))
          # shiny::fluidRow(
          #              shiny::column(width = 12,
          #                            style = "background-color:#003366; border-top:2px solid #fcba19;",
          #                            tags$footer(class = "footer",
          #                                        tags$div(class = "container",
          #                                                 style = "display:flex; justify-content:center; flex-direction:column; text-align:center; height:46px;",
          #                                                 tags$ul(style = "display:flex; flex-direction:row; flex-wrap:wrap; margin:0; list-style:none; align-items:center; height:100%;",
          #                                                         tags$li(a(href="https://www2.gov.bc.ca/gov/content/home","Home",style="font-size:1em;font-weight:normal;color:white;padding-left:5px;padding-right:5px;border-right:1px solid #4b5e7e;")),
          #                                                         tags$li(a(href="https://www2.gov.bc.ca/gov/content/home/disclaimer","Disclaimer",style="font-size:1em;font-weight:normal;color:white;padding-left:5px;padding-right:5px;border-right:1px solid #4b5e7e;")),
          #                                                         tags$li(a(href="https://www2.gov.bc.ca/gov/content/home/privacy","Privacy",style="font-size:1em;font-weight:normal;color:white;padding-left:5px;padding-right:5px;border-right:1px solid #4b5e7e;")),
          #                                                         tags$li(a(href="https://www2.gov.bc.ca/gov/content/home/accessibility","Accessibility",style="font-size:1em;font-weight:normal;color:white;padding-left:5px;padding-right:5px;border-right:1px solid #4b5e7e;")),
          #                                                         tags$li(a(href="https://www2.gov.bc.ca/gov/content/home/copyright","Copyright",style="font-size:1em;font-weight:normal;color:white;padding-left:5px;padding-right:5px;border-right:1px solid #4b5e7e;")),
          #                                                         tags$li(a(href="https://www2.gov.bc.ca/StaticWebResources/static/gov3/html/contact-us.html","Contact",style="font-size:1em;font-weight:normal;color:white;padding-left:5px;padding-right:5px;")))))))))


caption <- function(...) {
  HTML(paste0('<div class="caption-box">', ..., '</div>'))
}

# SERVER #######################################################################

server <- function(input, output, session) {
  
  lidar_profile_server(input, output, session)
  
  # 0. Module tab map ───────────────────────────────────────────────────────────── ####
  MODULE_TABS <- c(
    "Streamflow and Freshwater"    = "Streamflow & Freshwater",
    "Forest Disturbance"           = "Forest Disturbance",
    "CEF Human Disturbance - 2021" = "CEF Human Disturbance",
    "Stream Profile"               = "Stream Profile",
    "Water Allocations"            = "Water Allocations",
    "ClimateBC"                    = "ClimateBC",
    "Snow Remote Sensing"          = "Snow Remote Sensing",
    "Landslide Zonation"           = "Landslide Zonation",
    "Satellite Imagery"            = "Satellite Imagery"
  )
  
  # Hide all module tabs on startup
  observe({ for (tab in MODULE_TABS) hideTab(inputId = "module_tabs", target = tab) })
  
  # 1. SETUP ───────────────────────────────────────────────────────────────────── ####
  
  updateSelectizeInput(session, "psql_zoom_to_name", choices = c("", names$name), server = TRUE)
  
  session_start <- Sys.time() %>% format("%Y-%m-%d %H:%M:%S %Z", tz = "UTC")
  session_token <- session$token

  # Visitor counter: log this app open, then show distinct sessions in the footer
  tryCatch(
    dbExecute(conn, "INSERT INTO usage (date_time, session_token, action) VALUES ($1, $2, 'app open')",
              params = list(session_start, session_token)),
    error = function(e) message("Visit log failed: ", conditionMessage(e)))
  output$visit_count <- renderText({
    n <- get_visit_count()
    if (is.na(n)) "" else paste0(" · ", format(n, big.mark = ","), " visits since Dec 2023")
  })
  
  new_ws           <- reactiveVal()
  basin_source     <- reactiveVal()
  new_ws2_forRF    <- reactiveVal()
  gg_plot_discharge2 <- reactiveVal()
  pending_click    <- reactiveVal(NULL)   # stores lat/lng awaiting confirmation
  discharge_obs    <- NULL                # reference-station observer, recreated each run
  ws_point         <- reactiveVal(NULL)   # lat/lng of the delineated watershed, saved in the URL bookmark
  restore_trigger  <- reactiveVal(NULL)   # set on bookmark restore to re-run delineation

  # Keepalive: stops shinyapps.io dropping idle sessions, for the first keepalive_s only
  # (after that the normal idle timeout applies, so forgotten tabs don't burn active hours)
  keepalive_until <- Sys.time() + keepalive_s
  observe({ if (Sys.time() < keepalive_until) invalidateLater(60000); NULL })

  # 1b. URL BOOKMARK ──────────────────────────────────────────────────────────── ####
  #     Only the delineation point is stored; watershed_source and run_modules come
  #     along as ordinary inputs. Everything transient is excluded from the URL.
  onBookmark(function(state) {
    state$values$ws_point <- ws_point()
  })
  onBookmarked(function(url) {
    updateQueryString(url)
  })
  onRestore(function(state) {
    pt <- state$values$ws_point
    if (!is.null(pt$lat) && !is.null(pt$lng)) {
      pending_click(list(lat = as.numeric(pt$lat), lng = as.numeric(pt$lng)))
      restore_trigger(Sys.time())
    }
  })
  bookmark_selection <- function() {
    setBookmarkExclude(c("btn_confirm_delineate", "zoom_to_button", "run_button", "ls_load",
                         "psql_zoom_to_name", "plot_discharge_site_sel",
                         "profile_mode", "profile_line", "profile_close",
                         grep("^mymap_|^plotly_|^\\.clientValue", names(input), value = TRUE)))
    session$doBookmark()
  }
  
  output$mymap <- renderLeaflet({
    initial_map %>%
      add_profile_tool() %>%
      add_landslides() %>%
      addLayersControl(
        baseGroups    = BASE_GROUPS,
        overlayGroups = REF_OVERLAYS,
        options = layersControlOptions(collapsed = TRUE)) %>%
      hideGroup("Landslides v14 (Brideau et al. 2026)") %>%
      fitBounds(-140, 48, -113, 60)
  })

  # Landslide points are sent only when the layer is first switched on (per map render)
  observeEvent(input$ls_load, {
    leafletProxy("mymap") %>% add_landslide_points()
    session$sendCustomMessage("ls_tally", ls_tally_data)
  })

  # 2. MAP CLICK → CONFIRMATION MODAL ─────────────────────────────────────────── ####
  #    Replaces the old "Watershed Delineation on Map Click" checkbox.
  #    A click on the map stores the coordinates and shows a modal asking the
  #    user to confirm before triggering the (potentially slow) PostGIS query.
  
  observeEvent(input$mymap_click, {
    
    if (isTRUE(input$profile_mode)) return()   # drawing an elevation profile line
    point <- input$mymap_click
    pending_click(list(lat = point$lat, lng = point$lng))
    
    showModal(modalDialog(
      title = "Delineate watershed at this location?",
      HTML(paste0(
        "<b>Clicked location</b><br>",
        "Lat: ", round(point$lat, 5), " &nbsp; Lon: ", round(point$lng, 5), "<br><br>",
        "<b>Data source</b><br>",
        input$watershed_source, "<br><br>",
        "<small><i>Selecting a watershed may take a few seconds depending on the data source.</i></small>")),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("btn_confirm_delineate", "Delineate here", style = "color: #fff; background-color: #337ab7; border-color: #2e6da4")),
      easyClose = TRUE,
      size = "s"
    ))
  })
  
  coast_ifw_data   <- reactiveVal(NULL)
  
  observeEvent(input$watershed_source, {
    
    coastal_sources <- c("Coastal Important Fisheries Watersheds")
    
    if (input$watershed_source %in% coastal_sources) {
      
      # Load from PostGIS on first use only
      if (is.null(coast_ifw_data())) {
        withProgress(message = "Loading coastal watersheds...", max = 1, {
          incProgress(0.3, detail = "Querying database...")
          
          ifw <- st_read(conn,
                         query = 'SELECT "WSNAME", area_km2,
                            ST_SimplifyPreserveTopology(geom, 100) AS geom
                     FROM dcwbm_bsn_typed_ifw_1991_2020_100m',
                         quiet = TRUE) %>%
            st_transform(4326)
          
          coast_ifw_data(ifw)
          incProgress(0.7, detail = "Done.")
        })
      }
      
      # Add to map and show
      leafletProxy("mymap") %>%
        clearGroup("Coast - IFW") %>%
        addPolygons(data        = coast_ifw_data(),
                    weight      = 2,
                    fillColor   = "green",
                    fillOpacity = 0,
                    color       = "grey",
                    group       = "Coast - IFW",
                    label       = coast_ifw_data()$WSNAME) %>%
        showGroup("Coast - IFW")
      
    } else {
      # Hide when a non-coastal source is selected
      leafletProxy("mymap") %>% hideGroup("Coast - IFW")
    }
  })
  
  
  
  # 3. CONFIRMED MAP CLICK → WATERSHED SELECTION ──────────────────────────────── ####
  
  # Same delineation runs for a confirmed click and for a bookmark restore
  observeEvent(input$btn_confirm_delineate, { removeModal(); delineate_pending() })
  observeEvent(restore_trigger(), { delineate_pending() })

  delineate_pending <- function() {

    point_df <- as.data.frame(pending_click())
    
    withProgress(message = "Finding watershed...", max = 1, {
      
      tic()
      incProgress(1, detail = "Querying database...")
      
      if (input$watershed_source == "Freshwater Atlas Named Watersheds") {
        basin_source("FWA")
        bas <- st_read(conn, query = paste0(
          "SELECT * FROM fwa_named
           WHERE ST_Intersects(geom, ST_Transform(ST_SetSRID(ST_MakePoint(",
          point_df$lng, ",", point_df$lat, "), 4326), 3005))
           ORDER BY area_m2 ASC LIMIT 1"))
        if (nrow(bas) > 0) {
          bas <- bas %>% st_cast("POLYGON", warn = FALSE) %>%
            mutate(area_km2 = area_m2 / 1e6)
        }
      }
      
      if (input$watershed_source == "Freshwater Atlas by Stream Order") {
        basin_source("FWA Order")
        bas <- st_read(conn, query = paste0(
          "SELECT * FROM fwa_rollup
           WHERE ST_Intersects(geom, ST_Transform(ST_SetSRID(ST_MakePoint(",
          point_df$lng, ",", point_df$lat, "), 4326), 3005))
           ORDER BY area_m2 ASC LIMIT 1"))
        bas <- bas %>% st_cast("POLYGON", warn = FALSE)
        bas$overl <- bas %>% st_intersects(
          point_df %>% st_as_sf(coords = c("lng","lat"), crs = 4326) %>% st_transform(st_crs(bas)),
          sparse = FALSE)
        bas <- bas[bas$overl == TRUE, ] %>% mutate(area_m2 = as.numeric(st_area(.)))
        if (nrow(bas) > 0)
          bas <- bas %>% mutate(area_km2 = area_m2 / 1e6) %>%
          rename(gnis_name = id, gnis_id = iFWA)
      }
      
      if (input$watershed_source == "Custom Basin at Point of Interest") {
        basin_source("basinsv4")
        bas <- st_read(conn, query = paste0(
          "SELECT * FROM basinsv4
           WHERE ST_Intersects(geom, ST_Transform(ST_SetSRID(ST_MakePoint(",
          point_df$lng, ",", point_df$lat, "), 4326), 3005))
           ORDER BY area_m2 ASC LIMIT 1"))
        if (nrow(bas) > 0)
          bas <- bas %>% mutate(area_km2 = area_m2 / 1e6) %>%
          rename(gnis_name = id, gnis_id = basin) %>%
          ms_simplify(keep = 0.5)
      }
      
      if (input$watershed_source == "Water Survey of Canada Basins") {
        basin_source("WSC")
        point_sf  <- st_as_sf(point_df, coords = c("lng","lat"), crs = 4326)
        wsc_pp_id <- wsc_pp %>%
          mutate(dist = as.numeric(st_distance(., point_sf))) %>%
          filter(dist == min(dist)) %>% pull(stationnum)
        bas <- st_read(conn, query = paste0(
          "SELECT stationnum,name,area,geometry FROM wsc_drainagebasin_clean_3005
           WHERE stationnum = '", wsc_pp_id, "'")) %>% rename(geom = geometry)
        bas <- bas %>% rmapshaper::ms_simplify(keep = 0.8)
        if (nrow(bas) > 0)
          bas <- bas %>%
          mutate(gnis_name = paste0(stationnum, " ", name),
                 gnis_id   = stationnum,
                 area_km2  = area / 1e6) %>%
          ms_simplify(keep = 0.5) %>%
          st_transform(crs = 3005)
      }
      
      if (input$watershed_source == "Coastal Watersheds with Typing") {
        basin_source("COAST")
        bas <- st_read(conn, query = paste0(
          "SELECT * FROM dcwbm_bsn_typed_1991_2020_100m
           WHERE ST_Intersects(geom, ST_Transform(ST_SetSRID(ST_MakePoint(",
          point_df$lng, ",", point_df$lat, "), 4326), 3005))
           ORDER BY area_km2 ASC LIMIT 1"))
        if (nrow(bas) > 0)
          bas <- bas %>% rename(gnis_name = iFWA) %>%
          mutate(gnis_id = gnis_name) %>%
          ms_simplify(keep = 0.5)
      }
      
      if (input$watershed_source == "Coastal Important Fisheries Watersheds") {
        basin_source("COAST_IFW")
        bas <- st_read(conn, query = paste0(
          "SELECT * FROM dcwbm_bsn_typed_ifw_1991_2020_100m
           WHERE ST_Intersects(geom, ST_Transform(ST_SetSRID(ST_MakePoint(",
          point_df$lng, ",", point_df$lat, "), 4326), 3005))
           ORDER BY area_km2 ASC LIMIT 1"))
        if (nrow(bas) > 0)
          bas <- bas %>% rename(gnis_name = WSNAME) %>%
          mutate(gnis_id = gnis_name) %>%
          ms_simplify(keep = 0.5)
      }
      
      if (nrow(bas) > 0) {
        bas_4326 <- bas %>% st_transform(4326)
        new_ws(bas)
        
        output$ws_selection <- renderText({
          base_text <- paste0("You selected <b>", new_ws()$gnis_name, "</b> (",
                              format(round(as.numeric(new_ws()$area_km2), 0), big.mark = ","),
                              " km²)")
          if ("regime" %in% names(new_ws()))
            paste0(base_text, "<br>Regime: ", new_ws()$regime)
          else base_text
        })
        output$ws_selection_pred_time <- renderText({
          paste0("Estimated run time ~ ",
                 0.5 + round((new_ws()$area_km2 * 0.03) / 60, 1), " min")
        })
        
        bbbb <- st_bbox(bas_4326)
        leafletProxy("mymap") %>% leaflet::clearGroup("Watershed")
        leafletProxy("mymap") %>%
          leaflet::clearGroup(RESULT_GROUPS) %>% removeControl("legend_points") %>% removeControl("legend_polygons") %>%
          addPolygons(data = bas_4326, weight = 2, fillOpacity = 0,
                      color = "blue", group = "Watershed") %>%
          fitBounds(bbbb$xmin[[1]], bbbb$ymin[[1]], bbbb$xmax[[1]], bbbb$ymax[[1]])
        
        tictoc <- toc(quiet = TRUE)$callback_msg
        output$ws_run <- renderText({ tictoc })
        
        dbWriteTable(conn, "usage",
                     data.frame(date_time        = as.character(session_start),
                                session_token    = session_token,
                                gnis_name        = new_ws()$gnis_name,
                                gnis_id          = new_ws()$gnis_id,
                                processing_time  = tictoc,
                                action           = "select watershed",
                                area_km2         = round(new_ws()$area_km2, 1),
                                basin_source     = basin_source()),
                     append = TRUE)

        ws_point(as.list(point_df))
        bookmark_selection()
      } else {
        showModal(modalDialog(
          title  = "No watershed found",
          "No watershed polygon was found at that location for the selected data source. Try a different source or click a different location.",
          easyClose = TRUE, footer = modalButton("OK")
        ))
      }
    })
  }
  
  # 4. SELECT FROM LIST ────────────────────────────────────────────────────────── ####
  
  observeEvent(input$zoom_to_button, {
    req(input$psql_zoom_to_name != "")
    tic(quiet = TRUE)
    basin_source("FWA")
    
    withProgress(message = "Finding watershed...", max = 1, {
      split_name <- strsplit(strsplit(input$psql_zoom_to_name, "id:")[[1]][2], ")")[[1]][1]
      bas  <- st_read(conn, query = paste0("SELECT * FROM fwa_named WHERE gnis_id = ", split_name)) %>%
        mutate(area_km2 = area_m2 / 1e6)
      bas4326 <- bas %>% st_transform(4326)
      new_ws(bas)
      
      if (nrow(bas) > 0) {
        output$ws_selection <- renderText({
          paste0("You selected <b>", new_ws()$gnis_name, "</b> (",
                 format(round(as.numeric(new_ws()$area_km2), 0), big.mark = ","), " km²)")
        })
        output$ws_selection_pred_time <- renderText({
          paste0("Estimated run time ~ ",
                 0.5 + round((new_ws()$area_km2 * 0.03) / 60, 1), " min")
        })
        
        bbbb <- st_bbox(bas4326)
        leafletProxy("mymap") %>% leaflet::clearGroup("Watershed")
        leafletProxy("mymap") %>%
          leaflet::clearGroup(RESULT_GROUPS) %>% removeControl("legend_points") %>% removeControl("legend_polygons") %>%
          addPolygons(data = bas4326, fillOpacity = 0, weight = 2, color = "blue", group = "Watershed") %>%
          fitBounds(bbbb$xmin[[1]], bbbb$ymin[[1]], bbbb$xmax[[1]], bbbb$ymax[[1]])
        
        tictoc <- toc()$callback_msg
        output$ws_run <- renderText({ tictoc })
        dbWriteTable(conn, "usage",
                     data.frame(date_time       = as.character(session_start),
                                session_token   = session_token,
                                gnis_name       = new_ws()$gnis_name,
                                gnis_id         = new_ws()$gnis_id,
                                processing_time = tictoc,
                                action          = "select watershed",
                                area_km2        = round(new_ws()$area_km2, 1),
                                basin_source    = basin_source()),
                     append = TRUE)
      }
    })
  })
  
  # RUN REPORT ####
  
  observeEvent(input$run_button, {
    
    for (tab in MODULE_TABS) hideTab(inputId = "module_tabs", target = tab)
    
    for (btn in c("downloadWatershed", "downloadSubbasins", "downloadDEM", "downloadFire", "downloadCutblocks", "downloadRoads",
                  "downloadLakes", "downloadWetlands", "downloadGlaciers", "downloadImagery1985", "downloadImagery2020", "downloadImagery2023",
                  "downloadPDF", "downloadAllSpatial")) {shinyjs::hide(btn)}
    
    if (!is.null(new_ws())) {
      
      new_ws2 <- new_ws()
      print(new_ws2 %>% st_drop_geometry() %>% select(starts_with("gnis")))
      
      if (new_ws2$area_km2 >= 20000) {
        output$ws_run <- renderText({
          "Watershed is too large (>20,000 km²). Please select a smaller watershed."
        })
      }
      
      if (new_ws2$area_km2 < 20000) {
        
        tic()
        new_ws2_wkt  <- st_as_text(st_geometry(new_ws2 %>% ms_explode()))
        new_ws2_4326 <- new_ws2 %>% st_transform(4326)
        
        withProgress(message = "Processing...", max = 20, {
          
          # DEM / OVERVIEW ──────────────────────────────────────────────────────────────── ####
          
          incProgress(1, detail = "Get DEM...")
          
          dem <- terra::rast("/vsicurl/https://bcbasin.s3.ca-central-1.amazonaws.com/COP_GLO30_FOR_watershedBC_cog.tif",
                             win = terra::ext(new_ws2_4326))
          dem <- terra::mask(dem, vect(new_ws2_4326))
          dem_v     <- dem %>% as_tibble() %>% pull(COP_GLO30_FOR_watershedBC_cog)
          dem_quant <- data.frame(dem   = quantile(dem_v, probs = seq(0,1,0.01), na.rm = TRUE),
                                  quant = rev(seq(0,1,0.01)) * 100)
          min_coords <- xyFromCell(dem, which.min(values(dem)))
          min_pt     <- data.frame(x = min_coords[1,1], y = min_coords[1,2]) %>%
            st_as_sf(coords = c("x","y"), crs = 4326)
          min_coords <- min_pt %>% st_transform(3005) %>% filter(row_number() == 1) %>% st_coordinates()
          new_ws2    <- new_ws2 %>%
            mutate(LONGITUDE = min_coords[1,1][[1]], LATITUDE = min_coords[1,2][[1]])
          
          dem_min <- dem_quant %>% filter(quant == 100) %>% pull(dem)
          dem_max <- dem_quant %>% filter(quant ==   0) %>% pull(dem)
          ws_area <- new_ws2$area_km2
          melton  <- ((dem_max / 1000) - (dem_min / 1000)) / sqrt(ws_area)
          h60     <- dem_quant %>% filter(quant == 60) %>% pull(dem)
          
          gg_hyps <- dem_quant %>%
            ggplot() +
            geom_hline(yintercept = h60, linetype = 2, color = "black") +
            geom_line(aes(quant, dem), color = "steelblue") +
            geom_text(data = data.frame(x = 90, y = h60 * 1.05,
                                        label = paste0("h60 = ", h60, " m")),
                      aes(x, y, label = label), inherit.aes = FALSE, size = 3.5) +
            theme_bw() +
            scale_x_continuous(expand = c(0,0)) +
            scale_y_continuous(expand = c(0,0)) +
            labs(x = "Cumulative watershed area (%)", y = "Elevation (m a.s.l.)",
                 title = paste0("Basin Hypsometry  |  Melton Ratio = ", round(melton, 2),
                                "  |  Elev. range ", dem_min, "–", dem_max, " m"))
          
          output$plot_hypsometry <- renderPlotly({ ggplotly(gg_hyps, dynamicTicks = TRUE, width = 800) })
          
          output$text_plot_hypsometry <- renderUI({caption(
            "<b>Hypsometric curve</b> — shows the cumulative distribution of elevation within the watershed, ",
            "expressed as the percentage of total watershed area lying <i>above</i> a given elevation. ",
            "The dashed line marks <b>h60</b>: the elevation at which 60% of watershed area lies above, a key ",
            "index used in snowmelt hydrology to characterise the snow accumulation zone (Stanton 1966). ",
            "The <b>Melton Ratio</b> (R<sub>me</sub> = ΔH / √A) is a dimensionless relief index; values >0.6 are ",
            "commonly associated with debris-flow-prone catchments (Melton 1965; Wilford et al. 2004). ",
            "Elevation range and minimum outlet elevation are also shown.<br>",
            "<b>Source:</b> Copernicus DEM GLO-30, 30 m resolution (ESA / Copernicus, 2021). ")})
          
          output$text_table_named <- renderUI({caption(
            "<b>Named watersheds table</b> — lists Freshwater Atlas named watershed polygons that intersect the ",
            "selected watershed, classified as upstream (smaller area), downstream (larger area), or the ",
            "watershed of interest. Useful for understanding the catchment hierarchy.<br>",
            "<b>Source:</b> BC Freshwater Atlas Named Watersheds (BC Ministry of Water, Land and Resource Stewardship; ",
            "BC Data Catalogue open licence OGL-BC 2.0).")})
          
          # STREAMFLOW AND FRESHWATER ──────────────────────────────────────────────────── ####
          
          if ("Streamflow and Freshwater" %in% input$run_modules) {
            
            showTab(inputId = "module_tabs", target = "Streamflow & Freshwater")
            incProgress(1, detail = "Get Watersheds")
            
            my_named <- postgis_get_pol("fwa_named", "*", elev = FALSE,
                                        my_wkt = new_ws2_wkt,
                                        min_area_km2 = new_ws2$area_km2 * 0.01)
            if (nrow(my_named) > 0) {
              my_named <- my_named %>%
                mutate(area_km2 = area_m2 / 1e6) %>%
                dplyr::select(gnis_name, area_km2) %>%
                arrange(-area_km2) %>%
                mutate(Location = case_when(
                  gnis_name == new_ws2$gnis_name ~ "Watershed of Interest",
                  area_km2  > new_ws2$area_km2   ~ "Downstream",
                  area_km2  < new_ws2$area_km2   ~ "Upstream",
                  TRUE ~ ""))
              output$table_named <- renderTable(
                my_named %>% st_drop_geometry() %>%
                  mutate(area_km2 = round(area_km2, 1)) %>%
                  rename(Name = gnis_name, `Area (km²)` = area_km2),
                digits = 2)
            }
            
            incProgress(1, detail = "Get Wetlands")
            my_wl <- postgis_get_pol("fwa_wetlands", "waterbody_type", my_wkt = new_ws2_wkt, my_dem = dem)
            if (nrow(my_wl) == 0)
              my_wl <- st_as_sf(data.frame(clipped_area_m2=0,waterbody_type="",elevation=0,area_m2=0,lat=0,long=0,type="fwa_wetlands"), coords=c("long","lat"), crs=3005)
            
            incProgress(1, detail = "Get Lakes")
            my_lk <- postgis_get_pol("fwa_lakes", "waterbody_type", my_wkt = new_ws2_wkt, my_dem = dem)
            if (nrow(my_lk) == 0)
              my_lk <- st_as_sf(data.frame(clipped_area_m2=0,waterbody_type="",elevation=0,area_m2=0,lat=0,long=0,type="fwa_lakes"), coords=c("long","lat"), crs=3005)
            
            incProgress(1, detail = "Get Glaciers")
            my_gl <- postgis_get_pol("fwa_glaciers", "waterbody_type", my_wkt = new_ws2_wkt, my_dem = dem)
            if (nrow(my_gl) == 0)
              my_gl <- st_as_sf(data.frame(clipped_area_m2=0,waterbody_type="",elevation=0,area_m2=0,lat=0,long=0,type="fwa_glaciers"), coords=c("long","lat"), crs=3005)
            
            my_gl_1985 <- bcdata::bcdc_query_geodata("a4c778e3-837d-4fd3-9cb2-a9b25ab46d1d") %>%
              select(GBA_GLHIST_SYSID, GLACIER_ID, SOURCE_YEAR, FEATURE_AREA_SQM) %>%
              filter(INTERSECTS(new_ws2)) %>% collect()
            if (nrow(my_gl_1985) > 0)
              my_gl_1985 <- my_gl_1985 %>% st_intersection(new_ws2) %>% mutate(clipped_area_m2 = as.numeric(st_area(.)))
            else
              my_gl_1985 <- st_as_sf(data.frame(GLACIER_ID=0,FEATURE_AREA_SQM=0,lat=0,long=0,SOURCE_YEAR=1985), coords=c("long","lat"), crs=3005)
            
            my_gl_2021 <- bcdata::bcdc_query_geodata("832b4523-7853-4ec5-8f9c-4314a0154927") %>%
              select(GLACIER_ID, SOURCE_YEAR, FEATURE_AREA_SQM) %>%
              filter(INTERSECTS(new_ws2)) %>% collect()
            if (nrow(my_gl_2021) > 0)
              my_gl_2021 <- my_gl_2021 %>% st_intersection(new_ws2) %>% mutate(clipped_area_m2 = as.numeric(st_area(.)))
            else
              my_gl_2021 <- st_as_sf(data.frame(GLACIER_ID=0,FEATURE_AREA_SQM=0,lat=0,long=0,SOURCE_YEAR=2021), coords=c("long","lat"), crs=3005)
            
            my_gl_1985 <- my_gl_1985 |> mutate(type = "gl_1985")
            my_gl_2021 <- my_gl_2021 |> mutate(type = "gl_2021")
            
            my_fwa <- bind_rows(
              data.frame(type=c("W","L","G","G 1985","G 2021"), area_m2=c(0,0,0,0,0)),
              my_wl %>% st_drop_geometry(),
              my_lk %>% st_drop_geometry(),
              my_gl %>% st_drop_geometry(),
              my_gl_1985 %>% st_drop_geometry(),
              my_gl_2021 %>% st_drop_geometry()) %>%
              st_drop_geometry() %>%
              mutate(type = case_when(
                type == "fwa_wetlands" ~ "Wetland (FWA)",
                type == "fwa_glaciers" ~ "Glacier (FWA)",
                type == "fwa_lakes"    ~ "Lake (FWA)",
                type == "gl_1985"    ~ "Glacier (1985)",
                type == "gl_2021"    ~ "Glacier (2021)"))
            
            (gg_my_fwa <- my_fwa %>%
                group_by(type) %>%
                dplyr::summarize(area_km2 = sum(clipped_area_m2, na.rm = TRUE) / 1e6) %>%
                filter(!is.na(type)) %>%
                ggplot() +
                geom_col(aes(type, area_km2, fill = type), color = "black") +
                theme_bw() +
                labs(x = "", y = "Area (km²)", title = "Freshwater Atlas Water Body Coverage", fill = "") +
                # scale_fill_manual(values = c("darkblue","blue","steelblue","grey2")) +
                scale_y_continuous(n.breaks = 8))
            
            output$plot_fwa <- renderPlotly({ ggplotly(gg_my_fwa, dynamicTicks = TRUE, width = 800) })
            
            output$text_plot_fwa <- renderUI({ caption(
              "<b>Freshwater Atlas (FWA) water body coverage</b> — total area of lakes, wetlands, and glaciers ",
              "clipped to the watershed boundary. FWA glacier extents reflect mapping circa 2000–2010 and may ",
              "not represent current conditions — compare with the 1985 and 2021 glacier inventories shown ",
              "on the interactive map.<br>",
              "<b>Sources:</b> BC Freshwater Atlas (lakes, wetlands, glaciers — BC Ministry of Water, Land and ",
              "Resource Stewardship); BC Glacier Historical database (1985 — BC Data Catalogue ID: a4c778e3); ",
              "BC Glacier Inventory (2021 — BC Data Catalogue ID: 832b4523). All data served under OGL-BC 2.0.")})
            
            # DAMS ───────────────────────────────────────────────────────────────────── ####
            incProgress(1, detail = "Dams...")
            dams <- postgis_get_point("dams", new_ws2_wkt)
            if (nrow(dams) == 0)
              dams <- st_as_sf(data.frame(total_licence_storage=0,long=0,lat=0), coords=c("long","lat"), crs=3005)
            new_ws2 <- new_ws2 %>%
              mutate(total_licence_storage = sum(dams$total_licence_storage),
                     regulated_percent     = 100 * (total_licence_storage / 1e9) / area_km2,
                     regulated             = case_when(regulated_percent > 0.003 ~ "regulated", TRUE ~ "unregulated"))
            
            # DISCHARGE ──────────────────────────────────────────────────────────────── ####
            incProgress(1, detail = "Estimating discharge...")
            new_ws2_forRF(pred_Q_prepWS(w = new_ws2, my_wetlands = my_wl, my_glaciers = my_gl, my_lakes = my_lk))
            ref_rank <- pred_Q_rankRef(w = new_ws2_forRF())
            ref_stn  <- ref_rank$station_number[1]
            updateSelectInput(inputId = "plot_discharge_site_sel", choices = ref_choices(ref_rank), selected = ref_stn)
            
            output$text_plot_discharge <- renderUI({ caption(
              "<b>Estimated discharge</b> — daily mean discharge estimates generated by ",
              "regression model trained on historical Water Survey of Canada (WSC) streamflow records. ",
              "The model identifies the most hydrologically similar ",
              "reference gauge based on watershed descriptors (ClimateBC climate normals, terrain indices ",
              "from the Copernicus GLO-30 DEM, Freshwater Atlas water body fractions, and dam storage / water licenses). ",
              "Discharge at the ungauged watershed is estimated by scaling the reference gauge record using ",
              "a simple area–precipitation index ratio (Morin 2002; Oudin et al. 2010).<br>",
              "<b>⚠ This model is a prototype and has NOT been validated.</b> Estimates may be unreliable.",
              "Do not use for regulatory, engineering, or water management decisions.<br>",
              "<b>Reference gauge:</b> The dropdown below lists candidate stations ranked by the model, with a ",
              "match (100% = as similar as the station's own basin); the top-ranked station is used unless you ",
              "choose another. The map shows the watershed ",
              "of interest (black), the reference gauge basin (green) and the top five candidate gauges.<br>",
              "<b>Sources:</b> WSC HYDAT database (Environment and Climate Change Canada); ",
              "ClimateBC v7.3 (Wang et al. 2016, <i>PLOS ONE</i>); Copernicus GLO-30 DEM."
            )})
            
            # Discharge plot + reference basin map for a given reference station ────────── ####
            discharge_stn <- NULL
            render_discharge <- function(my_force_station) {
              withProgress(message = "Updating reference station...", max = 2, {
                ref_stn_ws <- st_read(conn, query = paste0(
                  "SELECT stationnum,name,area,geometry FROM wsc_drainagebasin_clean_3005 WHERE stationnum = '",
                  my_force_station, "'")) %>% rename(geom = geometry)
                bbbbb <- st_bbox(bind_rows(ref_stn_ws %>% st_transform(4326), new_ws() %>% st_transform(4326)))
                
                ref_lbl  <- paste0("Reference basin: ", my_force_station)
                ref_pt   <- stn_training %>% filter(station_number == my_force_station) %>% slice(1)
                cand_pts <- ref_rank %>% filter(rank <= 5, station_number != my_force_station, !is.na(lon))
                ref_q    <- ref_quality(ref_rank, new_ws2_forRF(), my_force_station)
                output$warn_discharge_ref  <- renderUI(ref_warning_html(ref_q, my_force_station))
                output$text_discharge_ref  <- renderUI(ref_explain_html(ref_rank, new_ws2_forRF(), my_force_station))
                output$table_discharge_ref <- renderTable(ref_compare_table(ref_rank, new_ws2_forRF(), my_force_station),
                                                          align = "l", spacing = "xs", na = "")
                
                output$map_discharge_ref <- renderLeaflet({
                  wsc_pp_ac <- wsc_pp %>% filter(status == "active")      %>% mutate(id = stationnum)
                  wsc_pp_dc <- wsc_pp %>% filter(status == "discontinued") %>% mutate(id = stationnum)
                  leaflet() %>%
                    addWMSTiles("http://maps.gov.bc.ca/arcserver/rest/services/province/roads_wm/MapServer/tile/{z}/{y}/{x}",
                                layers = "GRB_BSK", options = WMSTileOptions(format="image/png", transparent=TRUE)) %>%
                    addMeasure(primaryLengthUnit="kilometers", secondaryLengthUnit="meters",
                               primaryAreaUnit="hectares", secondaryAreaUnit="sqmeters", position="topleft") %>%
                    addCircleMarkers(data=wsc_pp_ac, lng=wsc_pp_ac$lon, lat=wsc_pp_ac$lat,
                                     color="steelblue", radius=1, group="WSC Active",
                                     label=paste0(wsc_pp_ac$name," - ",wsc_pp_ac$stationnum," [active]")) %>%
                    addCircleMarkers(data=wsc_pp_dc, lng=wsc_pp_dc$lon, lat=wsc_pp_dc$lat,
                                     color="grey", radius=1, group="WSC Discontinued",
                                     label=paste0(wsc_pp_dc$name," - ",wsc_pp_dc$stationnum," [discontinued]")) %>%
                    addPolygons(data = ref_stn_ws %>% st_transform(4326), opacity=1, fillOpacity=0.2,
                                fillColor="green", weight=2, color="green", group=ref_lbl,
                                label=paste0("Reference basin: ", my_force_station, " - ", ref_pt$name)) %>%
                    addPolygons(data = new_ws() %>% st_transform(4326), opacity=1, fillOpacity=0, weight=2,
                                color="black", group="Watershed of Interest", label="Watershed of Interest") %>%
                    addCircleMarkers(lng=cand_pts$lon, lat=cand_pts$lat, radius=6, weight=2, color="darkorange",
                                     fillOpacity=0.8, group="Other top-5 candidates",
                                     label=paste0("#", cand_pts$rank, " ", cand_pts$station_number, " - ", cand_pts$name,
                                                  " (match ", fmt_score(cand_pts$match), ")")) %>%
                    addCircleMarkers(lng=ref_pt$lon, lat=ref_pt$lat, radius=7, weight=2, color="darkgreen",
                                     fillColor="green", fillOpacity=1, group=ref_lbl,
                                     label=paste0("Reference gauge: ", my_force_station, " - ", ref_pt$name)) %>%
                    fitBounds(bbbbb$xmin[[1]], bbbbb$ymin[[1]], bbbbb$xmax[[1]], bbbbb$ymax[[1]]) %>%
                    addLegend(position = "bottomright", opacity = 1,
                              colors = c("black", "green", "darkorange", "steelblue", "grey"),
                              labels = c("Watershed of interest", "Reference basin + gauge",
                                         "Other top-5 candidates", "WSC active", "WSC discontinued")) %>%
                    addLayersControl(overlayGroups = c("Watershed of Interest", ref_lbl, "Other top-5 candidates",
                                                       "WSC Active", "WSC Discontinued"),
                                     options = layersControlOptions(collapsed=TRUE)) %>%
                    addMouseCoordinates()
                })
                
                w_rf    <- new_ws2_forRF()
                wsc_obs <- if (input$watershed_source == "Water Survey of Canada Basins") new_ws()$gnis_id
                make_discharge_plot <- function(step) {
                  gg <- pred_Q_rf(w = w_rf, force_station = my_force_station, wsc_STATION_NUMBER = wsc_obs, step = step)
                  # PDF copy carries the match warning (the app shows it as a banner above the plot)
                  gg_plot_discharge2(if (ref_q$level == "good") gg else
                    gg + labs(subtitle = str_wrap(paste0(
                      if (ref_q$level == "poor") "WARNING: poor reference station match - " else "Caution: weak reference station match - ",
                      paste(ref_q$issues, collapse = "; ")), 100)))
                  gg
                }
                # Fill the PDF copy now, in case the plot is never viewed (hidden outputs don't render)
                make_discharge_plot(isolate(input$discharge_step))
                # Re-renders when the Daily / Weekly / Monthly toggle changes
                output$plot_discharge <- renderPlotly({
                  plotly::ggplotly(make_discharge_plot(input$discharge_step), dynamicTicks=TRUE, width=800)
                })
              })
              discharge_stn <<- my_force_station
            }
            
            # Render directly: if the predicted station equals the dropdown's current value,
            # updateSelectInput() doesn't fire the observer and the previous watershed's plot stayed
            render_discharge(ref_stn)
            
            # Reference station override. Replace (not stack) the observer from any previous run;
            # skip when the dropdown just echoes the station already rendered.
            if (!is.null(discharge_obs)) discharge_obs$destroy()
            discharge_obs <<- observeEvent(input$plot_discharge_site_sel, ignoreInit = TRUE, {
              my_force_station <- input$plot_discharge_site_sel
              if (!identical(my_force_station, discharge_stn)) render_discharge(my_force_station)
            })
            
          } 
          
          # FOREST DISTURBANCE ─────────────────────────────────────────────────────────── ####
          
          if ("Forest Disturbance" %in% input$run_modules) {
            
            showTab(inputId = "module_tabs", target = "Forest Disturbance")
            incProgress(1, detail = "Get Roads")
            
            dra <- postgis_get_line(to_clip = "dra", my_wkt = new_ws2_wkt)
            if (nrow(dra) > 0) {
              dra <- dra %>% st_make_valid() %>%
                mutate(length_km = as.numeric(st_length(.)) / 1000,
                       type = "dra", group = transport_line_surface_code_desc)
            } else {
              dra <- st_as_sf(data.frame(lon=-111,lat=55,length_km=0), coords=c("lon","lat"), crs=4326) %>%
                mutate(type="test",group="test") %>% filter(type != "test")
            }
            
            gg_dra <- dra %>% st_drop_geometry() %>% group_by(group) %>%
              dplyr::summarize(length_km = sum(length_km)) %>%
              ggplot() + geom_col(aes(group, length_km)) + theme_bw() +
              labs(x="", y="Length (km)",
                   title=paste0("Road network — total ", round(sum(dra$length_km),1), " km")) +
              scale_y_continuous(n.breaks=8)
            output$plot_dra <- renderPlotly({ ggplotly(gg_dra, dynamicTicks=TRUE, width=800) })
            output$text_plot_dra <- renderUI({ caption(
              "<b>Road network length by surface type</b> — total road length within the watershed, broken down by ",
              "surface classification (paved, loose, overgrown, etc.). Roads are a key driver of hydrological ",
              "connectivity, hillslope erosion, and fine-sediment delivery to streams. Road density ",
              "(km road per km² watershed) is used in some cumulative effects frameworks as an indicator ",
              "of landscape-level disturbance.<br>",
              "<b>Source:</b> BC Digital Road Atlas (DRA), BC Ministry of Transportation and Infrastructure. ",
              "BC Data Catalogue — updated continuously. Note: the DRA may not capture all resource roads ",
              "or recently decommissioned roads. For fine-scale road analysis, cross-reference with ",
              "Lidar-derived or remotely sensed road layers."
            )})
            
            incProgress(1, detail = "Get Wildfires")
            my_wf <- postgis_get_pol("fire", "fire_year", my_wkt = new_ws2_wkt, my_dem = dem)
            incProgress(1, detail = "Get Cutblocks")
            my_cb <- postgis_get_pol("cutblocks", "harvest_year", my_wkt = new_ws2_wkt, my_dem = dem)
            
            if (nrow(my_wf) == 0)
              my_wf <- st_as_sf(data.frame(clipped_area_m2=0,fire_year=1900,elevation=0,area_m2=0,lat=0,long=0,type="fire"), coords=c("long","lat"), crs=3005)
            if (nrow(my_cb) == 0)
              my_cb <- st_as_sf(data.frame(clipped_area_m2=0,harvest_year=1900,elevation=0,area_m2=0,lat=0,long=0,type="cutblock"), coords=c("long","lat"), crs=3005)
            
            # Annual disturbance ──────────────────────────────────────────────────────── 
            gg_timeseries <- bind_rows(
              my_wf %>% st_drop_geometry() %>% rename(year=fire_year) %>% dplyr::select(clipped_area_m2,year) %>% mutate(type="Wildfire"),
              my_cb %>% st_drop_geometry() %>% rename(year=harvest_year) %>% dplyr::select(clipped_area_m2,year) %>% mutate(type="Cutblock"),
              data.frame(year=seq(1900,lubridate::year(Sys.Date()),1), clipped_area_m2=0, type="Cutblock"),
              data.frame(year=seq(1900,lubridate::year(Sys.Date()),1), clipped_area_m2=0, type="Wildfire")) %>%
              group_by(type,year) %>%
              dplyr::summarize(area_km2 = sum(clipped_area_m2)/1e6) %>%
              ggplot() +
              geom_col(aes(year, area_km2, fill=type)) + theme_bw() +
              labs(x="", y="Area (km²)", title="Annual forest disturbance", fill="") +
              scale_fill_manual(values=c("darkgreen","orange")) +
              scale_y_continuous(n.breaks=8) + scale_x_continuous(n.breaks=10)
            output$plot_timeseries <- renderPlotly({ ggplotly(gg_timeseries, dynamicTicks=TRUE, width=800) })
            
            output$text_plot_timeseries <- renderUI({ caption(
              "<b>Annual forest disturbance history</b> — area of wildfire and harvesting within the watershed ",
              "by year, from 1900 to present. Bars represent new disturbance (not cumulative).<br>",
              "<b>Known limitations:</b> Cutblocks do not include forest removal on private land; cutblock ",
              "mapping is inconsistent before ~1950; older fire polygons are often poorly georeferenced; ",
              "wildfire polygons do not account for unburned islands within the perimeter.<br>",
              "<b>Sources:</b> BC Consolidated Cutblocks ",
              "(<a href='https://catalogue.data.gov.bc.ca/dataset/harvested-areas-of-bc-consolidated-cutblocks-' target='_blank'>BC Data Catalogue</a>); ",
              "BC Historical Wildfire Perimeters ",
              "(<a href='https://catalogue.data.gov.bc.ca/dataset/fire-perimeters-historical' target='_blank'>BC Data Catalogue</a>). ",
              "Both datasets are updated annually."
            )})
            
            # Cumulative disturbance ────────────────────────────────────────────────────
            a <- bind_rows(
              my_wf %>% st_drop_geometry() %>% rename(year=fire_year) %>% dplyr::select(clipped_area_m2,year) %>% mutate(type="Wildfire"),
              my_cb %>% st_drop_geometry() %>% rename(year=harvest_year) %>% dplyr::select(clipped_area_m2,year) %>% mutate(type="Cutblock"))
            gg_timeseries_cumsum <- a %>% filter(!is.na(year)) %>%
              bind_rows(data.frame(year=seq(1900,lubridate::year(Sys.Date()),1), clipped_area_m2=0, type="Cutblock"),
                        data.frame(year=seq(1900,lubridate::year(Sys.Date()),1), clipped_area_m2=0, type="Wildfire")) %>%
              group_by(type,year) %>%
              dplyr::summarize(area_km2=sum(clipped_area_m2)/1e6) %>%
              arrange(year) %>% mutate(cumsum=cumsum(area_km2)) %>%
              ggplot() +
              geom_col(aes(year, 100*(cumsum/new_ws2$area_km2), fill=type)) + theme_bw() +
              labs(x="", y="Cumulative disturbance (% of watershed)", title="Cumulative forest disturbance", fill="") +
              scale_fill_manual(values=c("darkgreen","orange")) +
              scale_y_continuous(n.breaks=8) + scale_x_continuous(n.breaks=10)
            output$plot_timeseries_cumsum <- renderPlotly({ ggplotly(gg_timeseries_cumsum, dynamicTicks=TRUE, width=800) })
            output$text_plot_timeseries_cumsum <- renderUI({ caption(
              "<b>Cumulative forest disturbance</b> — total area disturbed by wildfire and harvest, accumulated ",
              "over time as a percentage of total watershed area. Values can exceed 100% where the same area ",
              "has been disturbed more than once (e.g. harvested, regrown, then burned). ",
              "This metric complements the annual disturbance chart by showing the long-term footprint of ",
              "disturbance independent of recovery. It does NOT account for forest regeneration — see ",
              "Simplified ECA below for a recovery-adjusted index.<br>",
              "<b>Sources:</b> same as annual disturbance chart above."
            )})
            
            # ECA ───────────────────────────────────────────────────────────────────────
            eca <- bind_rows(
              my_wf %>% mutate(type="WILDFIRE") %>% rename(year=fire_year) %>% st_drop_geometry(),
              my_cb %>% mutate(type="CUTBLOCK") %>% rename(year=harvest_year) %>% st_drop_geometry()) %>%
              as_tibble() %>%
              mutate(ws_area = as.numeric(st_area(new_ws2[1,])) / 1e6,
                     eca = 100)
            
            # One recovery row per polygon per year since disturbance (vectorised; same rows as
            # binding a data frame per polygon)
            slope   <- 100 / 60
            eca_yrs <- Map(`:`, eca$year + 1, lubridate::year(lubridate::now()))
            idx     <- rep(seq_len(nrow(eca)), lengths(eca_yrs))
            yrs     <- unlist(eca_yrs)
            e <- bind_rows(
              eca,
              tibble(year    = yrs,
                     type    = eca$type[idx],
                     area    = eca$clipped_area_m2[idx] / 1e6,
                     ws_area = eca$ws_area[idx],
                     eca     = pmax(eca$eca[idx] - (yrs - eca$year[idx]) * slope, 0)))  # last: tibble() would mask `eca`
            
            gg_eca <- e %>%
              mutate(eca_area=(eca/100)*area, eca_area_ws_perc=100*(eca_area/ws_area)) %>%
              group_by(type,year) %>%
              summarize(eca=sum(eca_area_ws_perc, na.rm=TRUE)) %>%
              mutate(eca=signif(eca,3)) %>%
              group_by(year) %>% mutate(total_eca=sum(eca)) %>%
              ggplot() +
              geom_col(aes(year, eca, fill=type, group=total_eca)) + theme_bw() +
              scale_y_continuous(n.breaks=8) + scale_x_continuous(n.breaks=10) +
              labs(x="Year", y="Simplified ECA (%)", title="Simplified Equivalent Clearcut Area (ECA)") +
              scale_fill_manual(values=c("darkgreen","orange"))
            output$plot_eca <- renderPlotly({ ggplotly(gg_eca, dynamicTicks=TRUE, width=800) })
            
            output$text_plot_eca <- renderUI({ caption(
              "<b>Simplified Equivalent Clearcut Area (ECA)</b> — a basin-wide index of effective hydrological ",
              "disturbance that accounts for forest recovery. ECA expresses the area of forest disturbance ",
              "as a fraction of total watershed area, with a recovery factor applied to reduce the contribution ",
              "of older disturbances as vegetation regrows. Here, both wildfire and cutblocks are assumed to ",
              "recover linearly over 60 years (slope = 100/60 %/yr). There is significant uncertainty on these recovery times.<br>",
              "<b>Important caveats:</b> This Simplified ECA does not account for: non-forested areas (alpine, ",
              "wetlands, rock); permanent disturbances (roads, powerlines); variable recovery rates by site ",
              "or species; or forest health disturbances (mountain pine beetle, root disease). It should be ",
              "interpreted as an order-of-magnitude index only, and is <b>not equivalent</b> to a formally ",
              "audited ECA as defined under BC forest practice regulations.<br>",
              "<b>Reference:</b> Winkler, R.D. &amp; Boon, S. (2017). Equivalent Clearcut Area as an indicator ",
              "of hydrologic change in snow-dominated watersheds of southern BC. ",
              "<i>BC Ministry of Forests, Lands and NRO, Extension Note EN-118.</i>")})
            
            # Elevation bins ────────────────────────────────────────────────────────────
            incProgress(1, detail = "Polygon elevations")
            gg_elevbins <- bind_rows(
              my_wl %>% st_drop_geometry() %>% dplyr::select(type,clipped_area_m2,elevation),
              my_lk %>% st_drop_geometry() %>% dplyr::select(type,clipped_area_m2,elevation),
              my_gl %>% st_drop_geometry() %>% dplyr::select(type,clipped_area_m2,elevation),
              my_wf %>% st_drop_geometry() %>% dplyr::select(type,clipped_area_m2,elevation),
              my_cb %>% st_drop_geometry() %>% dplyr::select(type,clipped_area_m2,elevation)) %>%
              as_tibble() %>%
              mutate(bin=cut(elevation, breaks=seq(0,5000,100), labels=seq(0,4900,100))) %>%
              group_by(type,bin) %>%
              dplyr::summarize(area_km2=sum(clipped_area_m2/1e6)) %>%
              mutate(bin=as.numeric(as.character(bin))) %>%
              ggplot() +
              geom_col(aes(bin, area_km2, fill=type)) +
              labs(x="Centroid elevation (m)", y="Area (km²)", title="Land cover types by elevation band") +
              theme_bw() + coord_flip() +
              scale_fill_manual(values=c("darkgreen","orange","grey60","steelblue","#aec7e8"))
            output$plot_elevbins <- renderPlotly({ ggplotly(gg_elevbins, dynamicTicks=TRUE, width=800) })
            output$text_plot_elevbins <- renderUI({ caption(
              "<b>Land cover by elevation band</b> — distribution of wildfire, cutblocks, lakes, wetlands, and ",
              "glaciers across 100 m elevation bins, based on the centroid elevation of each polygon. ",
              "This allows rapid identification of which elevation zones are most affected by disturbance, ",
              "and how water body coverage (important for streamflow buffering) varies with elevation. ",
              "Note that centroid elevation is an approximation — large polygons spanning multiple elevation ",
              "bands are assigned a single value, which may underrepresent high-elevation or low-elevation margins.<br>",
              "<b>Elevation source:</b> Copernicus GLO-30 DEM sampled at polygon centroids."
            )})
            
          } # end Forest Disturbance
          
          # CEF HUMAN DISTURBANCE ──────────────────────────────────────────────────────── ####
          
          if ("CEF Human Disturbance - 2021" %in% input$run_modules) {
            
            showTab(inputId = "module_tabs", target = "CEF Human Disturbance")
            incProgress(1, detail = "Get CEF disturbance")
            
            bc_cef_2021 <- postgis_get_pol("bc_cef_2021", "*", my_wkt = new_ws2_wkt, elev = FALSE)
            
            bc_cef_2021_sum <- bc_cef_2021 %>%
              mutate(area_km22 = as.numeric(st_area(.))/1e6) %>%
              st_drop_geometry() %>%
              group_by(cef_disturb_group, cef_human_disturb_flag) %>%
              summarize(area=sum(area_km22))
            
            output$plot_cef_group <- renderPlotly({
              plot_ly(bc_cef_2021_sum,
                      labels=~paste0(gsub("_"," ",gsub("BTM - ","",cef_disturb_group))),
                      values=~area, type="pie", hole=0.6, width=800,
                      textinfo="percent", hoverinfo="text",
                      text=~paste0(cef_human_disturb_flag,"\n",cef_disturb_group,"\n",round(area,1)," km²"),
                      showlegend=TRUE) %>%
                layout(title="CEF Human Disturbance 2021 — by disturbance group",
                       xaxis=list(showgrid=FALSE,zeroline=FALSE,showticklabels=FALSE),
                       yaxis=list(showgrid=FALSE,zeroline=FALSE,showticklabels=FALSE))
            })
            
            bc_cef_2021_sum_flag <- bc_cef_2021 %>%
              mutate(area_km22=as.numeric(st_area(.))/1e6) %>%
              st_drop_geometry() %>%
              group_by(cef_human_disturb_flag) %>%
              summarize(area=sum(area_km22))
            
            output$plot_cef_group_flag <- renderPlotly({
              plot_ly(bc_cef_2021_sum_flag,
                      labels=~paste0(gsub("_"," ",gsub("BTM - ","",cef_human_disturb_flag))),
                      values=~area, type="pie", hole=0.6, width=800,
                      textinfo="percent", hoverinfo="text",
                      text=~paste0(cef_human_disturb_flag,"\n",round(area,1)," km²"),
                      showlegend=TRUE) %>%
                layout(title="CEF Human Disturbance 2021 — summarised by flag",
                       xaxis=list(showgrid=FALSE,zeroline=FALSE,showticklabels=FALSE),
                       yaxis=list(showgrid=FALSE,zeroline=FALSE,showticklabels=FALSE))
            })
            
            output$text_plot_cef_group_flag <- renderUI({ caption(
              "<b>Cumulative Effects Framework (CEF) Human Disturbance 2021</b> — a consolidated spatial ",
              "layer integrating multiple anthropogenic disturbance types into a single provincial-scale ",
              "footprint dataset. The top chart breaks down disturbance by detailed group (e.g. harvested ",
              "areas, roads, mines, urban, agriculture). The lower chart summarises into broad flags ",
              "(e.g. 'Human Disturbance', 'Natural Landbase'). Areas are shown as km² and as a percentage ",
              "of total watershed area.<br>",
              "<b>Disturbance types included:</b> harvested areas, roads and linear features, pipelines, ",
              "transmission lines, mines and quarries, agriculture, urban and industrial development, ",
              "and other anthropogenic footprints mapped as of 2021.<br>",
              "<b>Source:</b> BC CEF Human Disturbance Footprint 2021 ",
              "(<a href='https://catalogue.data.gov.bc.ca/dataset/ce-disturbance-2021/' target='_blank'>BC Data Catalogue</a>). ",
              "Developed by the BC Ministry of Forests for Cumulative Effects Framework assessments. ",
              "Methodology: ",
              "<a href='https://www2.gov.bc.ca/gov/content/environment/natural-resource-stewardship/cumulative-effects-framework' target='_blank'>CEF program page</a>."
            )})
            
          } # end CEF
          
          # STREAM PROFILE ───────────────────────────────────────────────────────────────####
          
          if ("Stream Profile" %in% input$run_modules) {
            
            showTab(inputId = "module_tabs", target = "Stream Profile")
            incProgress(1, detail = "Getting stream profile")
            
            SO <- if (new_ws2$area_km2 < 1000) 1 else 2
            
            my_stream_network <- bcdata::bcdc_query_geodata("freshwater-atlas-stream-network") %>%
              filter(INTERSECTS(new_ws2)) %>%
              filter(STREAM_ORDER >= SO) %>%
              filter(!is.na(GNIS_NAME)) %>%
              collect()
            
            if (nrow(my_stream_network) == 0)
              my_stream_network <- bcdata::bcdc_query_geodata("freshwater-atlas-stream-network") %>%
              filter(INTERSECTS(new_ws2)) %>%
              filter(STREAM_ORDER >= SO) %>%
              collect() %>% mutate(GNIS_NAME = new_ws2$gnis_name)
            
            my_stream_network_l <- my_stream_network %>%
              st_intersection(new_ws2) %>%
              group_by(GNIS_NAME) %>% summarize() %>%
              mutate(length_km = round(as.numeric(st_length(.)/1000), 1)) %>%
              st_drop_geometry()
            
            if (nrow(my_stream_network_l) > 0) {
              my_stream_network_pt <- my_stream_network %>%
                st_cast(to = "POINT") %>%
                dplyr::select(geometry, BLUE_LINE_KEY, GNIS_NAME) %>%
                mutate(coords=st_coordinates(.), X=coords[,"X"], Y=coords[,"Y"], Z=coords[,"Z"]) %>%
                dplyr::select(-coords) %>% arrange(Z) %>%
                st_intersection(new_ws2)
              my_stream_network_pt <- my_stream_network_pt[seq(1, nrow(my_stream_network_pt), 10), ]
              
              keys <- my_stream_network %>% st_drop_geometry() %>% filter(!is.na(GNIS_NAME)) %>%
                group_by(BLUE_LINE_KEY, GNIS_NAME) %>% dplyr::summarize(n=n()) %>%
                arrange(-n) %>% ungroup() %>% filter(row_number() <= 5) %>% pull(BLUE_LINE_KEY)
              
              max_magnitude <- my_stream_network %>% st_drop_geometry() %>%
                filter(!is.na(GNIS_NAME), BLUE_LINE_KEY %in% keys) %>%
                group_by(BLUE_LINE_KEY, GNIS_NAME) %>%
                dplyr::summarize(STREAM_MAGNITUDE_MAX=max(STREAM_MAGNITUDE)) %>%
                arrange(-STREAM_MAGNITUDE_MAX)
              
              my_stream_network_main_pt <- my_stream_network_pt %>%
                filter(BLUE_LINE_KEY == max_magnitude[1,]$BLUE_LINE_KEY) %>%
                mutate(dist_seg_m = replace_na(as.numeric(st_distance(geometry,lag(geometry),by_element=TRUE)),0),
                       dist_tot_m = cumsum(dist_seg_m))
              
              if (nrow(max_magnitude) > 1) {
                my_stream_network_tribs <- do.call(bind_rows, lapply(2:nrow(max_magnitude), function(i) {
                  trib_pt <- my_stream_network_pt %>%
                    filter(BLUE_LINE_KEY == max_magnitude[i,]$BLUE_LINE_KEY) %>%
                    mutate(dist_seg_m=replace_na(as.numeric(st_distance(geometry,lag(geometry),by_element=TRUE)),0),
                           dist_tot_m=cumsum(dist_seg_m))
                  trib_low_buf <- trib_pt %>% filter(dist_tot_m==0) %>% st_buffer(2000)
                  candidates <- my_stream_network_main_pt %>% st_intersection(trib_low_buf %>% dplyr::select(geometry))
                  candidates <- candidates %>%
                    mutate(distt=as.numeric(st_distance(candidates, trib_pt %>% filter(dist_tot_m==0)))) %>%
                    arrange(distt) %>% filter(distt==min(.$distt), dist_seg_m==min(.$dist_seg_m)) %>%
                    filter(BLUE_LINE_KEY==filter(max_magnitude,BLUE_LINE_KEY %in% .$BLUE_LINE_KEY)[1,"BLUE_LINE_KEY"]%>%pull())
                  trib_pt %>% mutate(dist_tot_m = dist_tot_m + candidates[1,]$dist_tot_m)
                }))
                my_stream_network_main_pt <- bind_rows(my_stream_network_main_pt, my_stream_network_tribs)
              }
              
              my_stream_network_main_pt <- my_stream_network_main_pt %>%
                full_join(max_magnitude) %>% mutate(name=GNIS_NAME)
              
              t <- my_stream_network_main_pt %>%
                left_join(my_stream_network_l %>% st_drop_geometry()) %>%
                mutate(label=paste0(GNIS_NAME," (",round(length_km,1)," km)"))
              
              t$label <- with(t, reorder(label, -length_km))
              
              gg_profile <- ggplot(t) +
                geom_line(aes(dist_tot_m/1000, Z, color=label)) + theme_bw() +
                labs(x="Distance from outlet (km)", y="Elevation (m a.s.l.)",
                     color="Stream", title="Longitudinal stream profile")
              output$plot_profile <- renderPlotly({ ggplotly(gg_profile, dynamicTicks=TRUE, width=800) })


            } else {
              output$plot_profile <- renderPlotly({ req(FALSE) })   # no streams: blank, stops the spinner
            }
            
            output$text_plot_profile <- renderUI({ caption(
              "<b>Longitudinal stream profile</b> — elevation plotted against distance from the watershed outlet for the ",
              "main stem and up to five major named tributaries. Profile shape reveals geomorphic structure: ",
              "steep upper reaches indicate high-energy headwater zones; concave-up profiles suggest graded ",
              "equilibrium conditions; knickpoints (abrupt elevation steps) indicate lithological boundaries, ",
              "faults, or recent incision following glacial retreat or base-level change.<br>",
              "Tributaries are positioned by snapping their confluence to the nearest main-stem vertex, so ",
              "tributary zero-distance corresponds to the confluence point on the x-axis.<br>",
              "Stream order filter: order ≥1 (watershed &lt;1,000 km²) or ≥2 (≥1,000 km²) to reduce noise. ",
              "Only streams with GNIS names are shown.<br>",
              "<b>Source:</b> BC Freshwater Atlas Stream Network — XYZ vertex coordinates are sourced from ",
              "the provincial DEM compilation. BC Data Catalogue (OGL-BC 2.0)."
            )})
            
          } # end Stream Profile
          
          # CLIMATEBC ────────────────────────────────────────────────────────────────────####
          
          if ("ClimateBC" %in% input$run_modules) {
            
            showTab(inputId = "module_tabs", target = "ClimateBC")
            incProgress(1, detail = "Get ClimateBC")
            
            base_url <- "https://bcbasin.s3.ca-central-1.amazonaws.com/climateBC_cog/"
            vars    <- c("PAS","MAT","MAP","CMD")
            periods <- c("Normal_1961_1990","Normal_1971_2000","Normal_1981_2010","Normal_1991_2020",
                         "13GCMs_ensemble_ssp126_2011-2040","13GCMs_ensemble_ssp126_2041-2070","13GCMs_ensemble_ssp126_2071-2100",
                         "13GCMs_ensemble_ssp245_2011-2040","13GCMs_ensemble_ssp245_2041-2070","13GCMs_ensemble_ssp370_2071-2100",
                         "13GCMs_ensemble_ssp370_2011-2040","13GCMs_ensemble_ssp585_2041-2070","13GCMs_ensemble_ssp585_2071-2100")
            combos <- expand.grid(period=periods, var=vars, stringsAsFactors=FALSE)
            
            climateBC <- rast(lapply(seq_len(nrow(combos)), function(i) {
              url <- sprintf("/vsicurl/%sBC_800m_%s_%s_cog.tif", base_url, combos$period[i], combos$var[i])
              rrr <- rast(url); crs(rrr) <- "EPSG:4326"
              r_crop <- crop(rrr, vect(new_ws2 %>% st_transform(crs(rrr))))
              names(r_crop) <- paste0(combos$period[i], "_", combos$var[i])
              r_crop
            }))
            
            climateBC_pts <- terra::spatSample(vect(new_ws2 %>% st_transform(4326)), method="regular", size=1000)
            climateBC_pts_dat <- terra::extract(climateBC, climateBC_pts) %>% as_tibble() %>%
              pivot_longer(-ID) %>%
              mutate(
                period = case_when(
                  str_detect(name,"Normal_1961_1990") ~ "1961-1990",
                  str_detect(name,"Normal_1971_2000") ~ "1971-2000",
                  str_detect(name,"Normal_1981_2010") ~ "1981-2010",
                  str_detect(name,"Normal_1991_2020") ~ "1991-2020",
                  str_detect(name,"_2011-2040_")      ~ "2011-2040",
                  str_detect(name,"_2041-2070_")      ~ "2041-2070",
                  str_detect(name,"_2071-2100_")      ~ "2071-2100",
                  TRUE ~ "Error"),
                parameter = case_when(
                  str_detect(name,"MAP") ~ "Mean Annual Precipitation",
                  str_detect(name,"MAT") ~ "Mean Annual Temperature",
                  str_detect(name,"CMD") ~ "Cumulative Moisture Deficit",
                  str_detect(name,"PAS") ~ "Precipitation as Snow",
                  TRUE ~ "Error"),
                model = case_when(
                  str_detect(name,"Normal_")  ~ "Historic",
                  str_detect(name,"13GCMs_") ~ "13_GCMs",
                  TRUE ~ "Error"),
                ssp = case_when(
                  str_detect(name,"_ssp126_") ~ "SSP1-2.6",
                  str_detect(name,"_ssp245_") ~ "SSP2-4.5",
                  str_detect(name,"_ssp370_") ~ "SSP3-7.0",
                  str_detect(name,"_ssp585_") ~ "SSP5-8.5",
                  TRUE ~ "Historic"),
                year = case_when(
                  period=="1961-1990"~1975, period=="1971-2000"~1985,
                  period=="1981-2010"~1995, period=="1991-2020"~2005,
                  period=="2011-2040"~2025, period=="2041-2070"~2055,
                  period=="2071-2100"~2085, TRUE~0))
            
            make_clim_plot <- function(param_name, y_label) {
              ggplotly(
                climateBC_pts_dat %>%
                  filter(parameter==param_name, model %in% c("Historic","13_GCMs")) %>%
                  group_by(period,ssp,year) %>%
                  dplyr::summarize(
                    q25  = quantile(value, 0.25, na.rm=TRUE),
                    mean = mean(value, na.rm=TRUE),
                    q75  = quantile(value, 0.75, na.rm=TRUE)) %>%
                  ggplot() +
                  geom_rect(aes(xmin=year-14, xmax=year+15, ymin=q25, ymax=q75,
                                color=ssp, fill=ssp, group=period), alpha=0.25) +
                  geom_point(aes(x=year, y=mean, color=ssp), size=2) +
                  labs(x="Period mid-point", y=y_label, color="Scenario", fill="Scenario") +
                  theme_bw(),
                dynamicTicks=TRUE, width=800)
            }
            
            make_clim_gg <- function(param_name, y_label) {
              climateBC_pts_dat %>%
                filter(parameter == param_name, model %in% c("Historic","13_GCMs")) %>%
                group_by(period, ssp, year) %>%
                dplyr::summarize(
                  q25  = quantile(value, 0.25, na.rm=TRUE),
                  mean = mean(value, na.rm=TRUE),
                  q75  = quantile(value, 0.75, na.rm=TRUE)) %>%
                ggplot() +
                geom_rect(aes(xmin=year-14, xmax=year+15, ymin=q25, ymax=q75,
                              color=ssp, fill=ssp, group=period), alpha=0.25) +
                geom_point(aes(x=year, y=mean, color=ssp), size=2) +
                labs(x="Period mid-point", y=y_label, color="Scenario", fill="Scenario") +
                theme_bw()
            }
            
            clim_caption <- function(var_name = "t", var_desc = "t", units = "t") {
              caption(
                "<b>", var_name, " (", units, ")</b> — ", var_desc, " ",
                "Shaded rectangles span the 25th–75th percentile of values sampled across the watershed ",
                "for each climate normal period (historic) or projection window (future). ",
                "Points show the spatial mean. Each future window represents a 30-year ensemble mean from ",
                "13 CMIP6 General Circulation Models (GCMs) for the specified SSP scenario.<br>",
                "<b>SSP scenarios:</b> SSP1-2.6 = low emissions (Paris-aligned); SSP2-4.5 = intermediate; ",
                "SSP3-7.0 = high emissions; SSP5-8.5 = very high emissions (fossil-fuel intensive).<br>",
                "<b>Source:</b> ClimateBC v7.3 (Wang et al. 2016, <i>PLOS ONE</i> 11(12): e0156720). ",
                "800 m downscaled grids for BC; 13-GCM ensemble from CMIP6. ",
                "<a href='https://climatebc.ca/' target='_blank'>climatebc.ca</a>."
              )
            }
            
            gg_mat      <- make_clim_gg("Mean Annual Temperature",    "Mean Annual Temperature (\u00b0C)")
            gg_map_clim <- make_clim_gg("Mean Annual Precipitation",  "Mean Annual Precipitation (mm/yr)")
            gg_pas      <- make_clim_gg("Precipitation as Snow",      "Precipitation as Snow (mm/yr)")
            gg_cmd      <- make_clim_gg("Cumulative Moisture Deficit","Cumulative Moisture Deficit (mm)")
            
            output$plot_mat <- renderPlotly({ make_clim_plot("Mean Annual Temperature", "Mean Annual Temperature (°C)") })
            output$plot_map <- renderPlotly({ make_clim_plot("Mean Annual Precipitation","Mean Annual Precipitation (mm/yr)") })
            output$plot_pas <- renderPlotly({ make_clim_plot("Precipitation as Snow",   "Precipitation as Snow (mm/yr)") })
            output$plot_cmd <- renderPlotly({ make_clim_plot("Cumulative Moisture Deficit","Cumulative Moisture Deficit (mm)") })
            
            output$text_plot_mat <- renderUI({ clim_caption("Mean Annual Temperature","Watershed-averaged mean annual temperature (MAT).","°C") })
            output$text_plot_map <- renderUI({ clim_caption("Mean Annual Precipitation","Watershed-averaged mean annual precipitation (MAP). An increase in MAP generally increases total runoff, though the timing shift from snow to rain can reduce summer baseflow in nival watersheds.","mm/yr") })
            output$text_plot_pas <- renderUI({ clim_caption("Precipitation as Snow","Watershed-averaged precipitation falling as snow (PAS). PAS is a key driver of snowpack accumulation and spring freshet timing. Declining PAS increases the rain fraction and shifts runoff earlier in the water year.","mm/yr") })
            output$text_plot_cmd <- renderUI({ clim_caption("Cumulative Moisture Deficit","Watershed-averaged annual moisture deficit (CMD). CMD is the sum of monthly differences between potential evapotranspiration (PET) and precipitation when PET exceeds precipitation — a proxy for summer drought stress. Higher CMD values indicate greater soil moisture deficits and increased wildfire risk.","mm") })
            
          } # end ClimateBC
          
          # WATER ALLOCATIONS ────────────────────────────────────────────────────────────####
          
          if ("Water Allocations" %in% input$run_modules) {
            
            showTab(inputId = "module_tabs", target = "Water Allocations")
            incProgress(1, detail = "Getting water allocations...")
            
            wap <- bcdata::bcdc_query_geodata("water-approval-points") %>%
              filter(INTERSECTS(new_ws2)) %>%
              select(WATER_APPROVAL_ID, APPROVAL_TYPE, WORKS_DESCRIPTION, QUANTITY, QUANTITY_UNITS,
                     QTY_DIVERSION_MAX_RATE, QTY_UNITS_DIVERSION_MAX_RATE, APPROVAL_STATUS,
                     APPROVAL_START_DATE, APPROVAL_EXPIRY_DATE) %>% collect()
            if (nrow(wap) > 0)
              wap <- wap %>% filter(APPROVAL_STATUS=="Current") %>%
              st_transform(st_crs(new_ws2)) %>% st_intersection(new_ws2) %>% st_transform(4326) %>%
              mutate(lon=st_coordinates(.)[,1], lat=st_coordinates(.)[,2]) %>% st_drop_geometry()
            if (nrow(wap) == 0)
              wap <- data.frame(lon=-111,lat=55,type="test",QUANTITY="",QUANTITY_UNITS="",APPROVAL_STATUS="",APPROVAL_TYPE="") %>% filter(type!="test")
            
            wrl <- bcdata::bcdc_query_geodata("water-rights-licences-public") %>%
              filter(INTERSECTS(new_ws2)) %>%
              select(POD_NUMBER, POD_SUBTYPE, POD_DIVERSION_TYPE, POD_STATUS, LICENCE_NUMBER, LICENCE_STATUS,
                     LICENCE_STATUS_DATE, PRIORITY_DATE, PURPOSE_USE_CODE, PURPOSE_USE, QUANTITY, QUANTITY_UNITS,
                     QUANTITY_FLAG, QUANTITY_FLAG_DESCRIPTION, QTY_DIVERSION_MAX_RATE, QTY_UNITS_DIVERSION_MAX_RATE,
                     PRIMARY_LICENSEE_NAME) %>% collect()
            if (nrow(wrl) > 0)
              wrl <- wrl %>% filter(LICENCE_STATUS=="Current") %>%
              st_transform(st_crs(new_ws2)) %>% st_intersection(new_ws2) %>% st_transform(4326) %>%
              mutate(lon=st_coordinates(.)[,1], lat=st_coordinates(.)[,2]) %>% st_drop_geometry()
            if (nrow(wrl) == 0)
              wrl <- data.frame(lon=-111,lat=55,type="test",QUANTITY="",QUANTITY_UNITS="",LICENCE_STATUS="",POD_SUBTYPE="") %>% filter(type!="test")
            
            annualise <- function(df, qty_col, unit_col) {
              df %>% mutate(
                vol_units  = str_split_fixed(.data[[unit_col]], "/", 2)[,1],
                time_units = str_split_fixed(.data[[unit_col]], "/", 2)[,2],
                total_vol_year = case_when(
                  sub(" ","",time_units)=="sec"  ~ as.numeric(.data[[qty_col]]) * 60*60*24*365,
                  sub(" ","",time_units)=="day"  ~ as.numeric(.data[[qty_col]]) * 365,
                  sub(" ","",time_units)=="year" ~ as.numeric(.data[[qty_col]]),
                  TRUE ~ 0))
            }
            
            water_auth <- bind_rows(
              wap %>% annualise("QUANTITY","QUANTITY_UNITS") %>%
                group_by(TYPE=APPROVAL_TYPE, STATUS=APPROVAL_STATUS) %>%
                summarise(SUM=sum(total_vol_year,na.rm=TRUE),.groups="drop"),
              wrl %>% annualise("QUANTITY","QUANTITY_UNITS") %>%
                group_by(TYPE=POD_SUBTYPE, STATUS=LICENCE_STATUS) %>%
                summarise(SUM=sum(total_vol_year,na.rm=TRUE),.groups="drop"))
            
            gg_water_auth <- water_auth %>% ggplot() +
              geom_col(aes(TYPE, SUM)) + theme_bw() +
              scale_y_continuous(labels=function(x) format(x,big.mark=",",scientific=FALSE)) +
              labs(y="Annualised volume (m³/yr)", x="", title="Water allocations by type")
            output$plot_auth <- renderPlotly({ ggplotly(gg_water_auth, dynamicTicks=TRUE, width=800) })
            output$text_plot_auth <- renderUI({ caption(
              "<b>Water allocations</b> — total annualised volume of water allocated within the watershed, ",
              "shown by licence/approval type. Two datasets are combined:<br>",
              "<b>Water Rights Licences (WRL)</b> — long-term water rights granted under BC's Water Sustainability ",
              "Act (WSA). Licences are allocated under a priority ('first in time, first in right') system. ",
              "Purpose-of-use codes identify the intended use (irrigation, municipal, industrial, etc.).<br>",
              "<b>Water Approval Points (WAP)</b> — short-term or conditional approvals for water diversion, ",
              "often associated with specific infrastructure (e.g. fish hatcheries, small hydro intakes).<br>",
              "Licences with flow-rate units (m³/s) are annualised by multiplying by 31,536,000 s/yr. ",
              "This approach assumes licences are used to full allocation year-round, which will overestimate ",
              "actual withdrawals for seasonal or partial-use licences. Map points are shown on the main map.<br>",
              "<b>Source:</b> BC Water Licensing system — Water Rights Licences and Water Approvals, ",
              "BC Data Catalogue (OGL-BC 2.0). Data reflect current (active) licences only."
            )})
            
          } # end Water Allocations
          
          # LANDSLIDE ZONATION ───────────────────────────────────────────────────────────####
          
          if ("Landslide Zonation" %in% input$run_modules) {
            
            showTab(inputId = "module_tabs", target = "Landslide Zonation")
            incProgress(1, detail = "Calculating landslide zonation...")
            
            slp <- rast("/vsicurl/https://bcbasin.s3.ca-central-1.amazonaws.com/slp_perc_cog.tif",
                        win = ext(new_ws2 %>% st_transform(3005)))
            slp <- mask(slp, vect(new_ws2 %>% st_transform(crs(slp))))
            
            url_map <- sprintf("/vsicurl/https://bcbasin.s3.ca-central-1.amazonaws.com/climateBC_cog/BC_800m_Normal_1991_2020_MAP_cog.tif")
            rrr <- rast(url_map); crs(rrr) <- "EPSG:4326"
            map_raster <- crop(rrr, vect(new_ws2 %>% st_transform(crs(rrr)))) %>%
              terra::mask(vect(new_ws2 %>% st_transform(4326)))
            
            rcl_map <- matrix(c(0,2000,1, 2000,3000,2, 3000,Inf,3), ncol=3, byrow=TRUE)
            rlf <- classify(map_raster, rcl_map, include.lowest=TRUE)
            rlf <- project(rlf, slp, method="near") #resample(project(rlf, slp, method="near"), slp, method="near")
            levels(rlf) <- data.frame(value=1:3, label=c("Low (<2000 mm)","Moderate (2000–3000 mm)","High (>3000 mm)"))
            
            rcl_slp <- matrix(c(0,60,1, 60,Inf,2), ncol=3, byrow=TRUE)
            slp_cls <- classify(slp, rcl_slp, include.lowest=TRUE)
            levels(slp_cls) <- data.frame(value=1:2, label=c("≤60%",">60%"))
            
            ls_susc <- rlf + (slp_cls * 10)
            rcl_ls  <- matrix(c(11,1, 12,2, 13,3, 21,3, 22,4, 23,5), ncol=2, byrow=TRUE)
            ls_susc <- classify(ls_susc, rcl_ls)
            levels(ls_susc) <- data.frame(value=1:5, label=c("Low","Low–Moderate","Moderate","High","High"))
            
            ws_border <- geom_sf(data=new_ws2, fill=NA, linewidth=0.8, colour="black")
            rlf_cols  <- c("Low (<2000 mm)"="#fee090","Moderate (2000–3000 mm)"="#f46d43","High (>3000 mm)"="#d73027")
            slp_cols  <- c("≤60%"="#d9f0d3",">60%"="#1b7837")
            susc_cols <- c("Low"="#1a9641","Low–Moderate"="#a6d96a","Moderate"="orange","High"="#d73027")
            
            gg_rlf_map <- ggplot() + tidyterra::geom_spatraster(data=map_raster) +
              scale_fill_gradientn(colours=rev(hcl.colors(9,"Blues 3")),na.value=NA) + ws_border +
              labs(title="A) Mean Annual Precipitation (1991–2020)", fill="mm/yr") + theme_void()
            output$plot_rlf_map <- renderPlot({ print(gg_rlf_map) })
            
            gg_rlf_mapz <- ggplot() + tidyterra::geom_spatraster(data=rlf, aes(fill=label)) +
              scale_fill_manual(values=rlf_cols, limits=names(rlf_cols), na.value=NA, drop=FALSE) + ws_border +
              labs(title="B) Precipitation zone", fill=NULL) + theme_void()
            output$plot_rlf_mapz <- renderPlot({ print(gg_rlf_mapz) })
            
            gg_rlf_slp <- ggplot() + tidyterra::geom_spatraster(data=slp) +
              scale_fill_gradientn(colours=hcl.colors(9,"Viridis"), na.value=NA) + ws_border +
              labs(title="C) Slope (%)", fill="%") + theme_void()
            output$plot_rlf_slp <- renderPlot({ print(gg_rlf_slp) })
            
            gg_rlf_slpz <- ggplot() + tidyterra::geom_spatraster(data=slp_cls, aes(fill=label)) +
              scale_fill_manual(values=slp_cols, limits=names(slp_cols), na.value=NA, drop=FALSE) + ws_border +
              labs(title="D) Steep terrain (>60%)", fill=NULL) + theme_void()
            output$plot_rlf_slpz <- renderPlot({ print(gg_rlf_slpz) })
            
            gg_rlf_ls <- ggplot() + tidyterra::geom_spatraster(data=ls_susc, aes(fill=label)) +
              scale_fill_manual(values=susc_cols, limits=names(susc_cols), na.value=NA, drop=FALSE) + ws_border +
              labs(title="E) Landslide susceptibility (composite)", fill=NULL) + theme_void()
            output$plot_rlf_ls <- renderPlot({ print(gg_rlf_ls) })
            
            output$text_plot_rlf_map <- renderUI({ caption(
              "<b>Panels A &amp; B — Mean Annual Precipitation (MAP)</b> — continuous and classified precipitation ",
              "from the ClimateBC 1991–2020 normal period at 800 m resolution. Three MAP zones are used: ",
              "Low (&lt;2,000 mm/yr), Moderate (2,000–3,000 mm/yr), and High (&gt;3,000 mm/yr). Higher precipitation ",
              "increases pore-water pressure and soil saturation, both of which strongly influence shallow ",
              "landslide initiation (Jakob &amp; Hungr 2005)."
            )})
            output$text_plot_rlf_slp <- renderUI({ caption(
              "<b>Panels C &amp; D — Slope gradient</b> — continuous slope (%) and binary classification into ",
              "≤60% and &gt;60% slope classes. The 60% threshold (~31°) is widely used in BC forest practice ",
              "as a first-order indicator of terrain instability and potential for shallow translational or ",
              "rotational landslides (BC MFLNRORD Terrain Stability Mapping Standards)."
            )})
            output$text_plot_rlf <- renderUI({ caption(
              "<b>Panel E — Composite landslide susceptibility</b> — combines the MAP zone and slope class ",
              "into a 5-class susceptibility index using a simple look-up table rule:<br>",
              "Low MAP + gentle slope → Low; Moderate MAP + steep slope → High; High MAP + steep slope → High.<br>",
              "<b>⚠ Important limitations:</b> This is a simplified first-pass screening tool only. It does NOT ",
              "incorporate soil type, surficial geology, forest cover, aspect, slope curvature, proximity to ",
              "drainage, or historical landslide inventory data. It has NOT been validated against observed ",
              "landslide events and should NOT be used for site-level hazard assessment, land-use planning, ",
              "or engineering design. For formal terrain stability assessment in BC, refer to qualified ",
              "geoscience professionals and the Terrain Stability Mapping standards.<br>",
              "<b>Sources:</b> Slope — derived from Copernicus GLO-30 DEM; MAP — ClimateBC v7.3 (Wang et al. 2016). ",
              "Jakob, M. &amp; Hungr, O. (Eds.) (2005). <i>Debris-flow Hazards and Related Phenomena.</i> Springer."
            )})
            
          } # end Landslide Zonation
          
          # SNOW REMOTE SENSING ──────────────────────────────────────────────────────────####
          
          if ("Snow Remote Sensing" %in% input$run_modules) {
            
            showTab(inputId = "module_tabs", target = "Snow Remote Sensing")
            incProgress(1, detail = "Mapping snow probability...")
            
            snow <- rast(get_snow_vrt(), win = terra::ext(new_ws2_4326))
            names(snow) <- snow_days

            # Separate object so the 30 m `dem` (used by the DEM download) isn't replaced by the snow grid
            snow_dem <- project(dem, snow, method="bilinear")
            snow_dem <- terra::mask(snow_dem, vect(new_ws2_4326))
            snow     <- terra::mask(snow, vect(new_ws2_4326))

            elev_vals <- values(snow_dem, mat=FALSE)
            breaks    <- seq(0, 10, by=1)
            labels    <- paste0(head(breaks*10,-1), "-", tail(breaks*10,-1), "%")
            
            results <- map_dfr(names(snow), function(i) {
              prob_vals <- values(snow[[i]], mat=FALSE)
              valid     <- !is.na(prob_vals) & !is.na(elev_vals)
              if (sum(valid)==0) return(tibble())
              tibble(prob=prob_vals[valid], elev=elev_vals[valid]) %>%
                mutate(prob_bin=cut(prob, breaks=breaks, labels=labels, include.lowest=TRUE, right=TRUE)) %>%
                filter(!is.na(prob_bin)) %>%
                summarise(elev_p05=quantile(elev,0,na.rm=TRUE), n_pixels=n(), .by=prob_bin) %>%
                mutate(doy=i)
            }, .progress=TRUE)
            
            results <- results %>% mutate(prob_bin=factor(prob_bin, levels=labels))
            d <- as.data.frame(snow_dem$COP_GLO30_FOR_watershedBC_cog)$COP_GLO30_FOR_watershedBC_cog
            
            p_snow_elev <- results %>%
              mutate(prob_bin=ifelse(prob_bin %in% c("50-60%","60-70%","70-80%","80-90%","90-100%"),
                                     "50-100%","0-50%")) %>%
              group_by(prob_bin, doy) %>% summarize(elev_p05=min(elev_p05)) %>%
              full_join(data.frame(doy=names(snow), prob_bin="50-100%", elev_p05=max(d))) %>%
              filter(prob_bin=="50-100%") %>%
              mutate(date=as.Date("2000-01-01") + as.numeric(doy),
                     date=if_else(month(date)>=9, date-years(1), date)) %>%
              group_by(doy,prob_bin,date) %>% summarise(elev_p05=min(elev_p05)) %>%
              ggplot(aes(x=date, y=elev_p05)) +
              geom_ribbon(aes(ymin=elev_p05, ymax=max(d), fill=">50% snow"), color="black") +
              geom_ribbon(aes(ymin=min(d),   ymax=elev_p05, fill="<50% snow"), color="black") +
              scale_fill_manual(values=c("orange","steelblue")) +
              scale_x_date(date_labels="%b", date_breaks="1 month",
                           limits=as.Date(c("1999-09-01","2000-08-31")), expand=c(0,0)) +
              theme_classic() +
              labs(x="", y="Elevation (m a.s.l.)",
                   fill="Snow probability at this elevation",
                   title="Snow line elevation — median day-of-year, 2001–2023") +
              scale_y_continuous(n.breaks=7, expand=c(0,0)) +
              theme(legend.position="bottom", legend.title=element_blank())
            
            snow_season <- sum(snow > 2, na.rm=TRUE) * 5
            snow_season <- terra::mask(snow_season, vect(new_ws2_4326))
            brks     <- c(0,30,60,90,120,150,180,240,300,365)
            lbls     <- c("<30","30-60","60-90","90-120","120-150","150-180","180-240","240-300","300-365")
            rcl_dur  <- cbind(head(brks,-1), tail(brks,-1), seq_along(lbls))
            snow_class <- terra::classify(snow_season, rcl_dur, include.lowest=TRUE)
            levels(snow_class) <- data.frame(id=seq_along(lbls), label=lbls)
            cols <- setNames(hcl.colors(length(lbls),"Spectral"), lbls)
            
            p_snow_map <- ggplot() +
              tidyterra::geom_spatraster(data=snow_class) +
              scale_fill_manual(values=cols, name="Snow season\n(days)", limits=lbls, drop=TRUE, na.value=NA) +
              geom_sf(data=new_ws2_4326, fill=NA, color="black", linewidth=1) +
              theme_void() +
              labs(title="Snow season duration (days with >50% snow probability, 2001–2023 median)")
            
            output$plot_snowrs_plot  <- renderPlotly({ ggplotly(p_snow_elev) %>%
                layout(xaxis=list(tickformat="%b", dtick="M1")) })
            output$plot_snowrs_map   <- renderPlot({ p_snow_map })
            
            output$text_plot_snowrs_elev <- renderUI({ caption(
              "<b>Snow line elevation transect</b> — shows the elevation at which at least 50% of pixels have ",
              "snow present, for each 5-day interval through the water year (September–August). The blue area ",
              "above the line represents the snow-covered zone (&gt;50% probability); the orange area below ",
              "is the snow-free zone. The curve reveals the seasonal rise (melt-out) and fall (accumulation) ",
              "of the transient snow line, and the minimum snow line elevation in mid-winter."
            )})
            output$text_plot_snowrs_map <- renderUI({ caption(
              "<b>Snow season duration map</b> — spatial distribution of annual snow season length, defined as ",
              "the number of days per year when a pixel has &gt;50% probability of snow cover. Longer snow ",
              "seasons (blue/purple) indicate persistent snowpacks at high elevations; shorter seasons ",
              "(red/orange) indicate transitional or rain-dominated zones.<br>",
              "<b>Source:</b> MODIS Terra &amp; Aqua "
            )})
            
          } # end Snow Remote Sensing
          
          # SATELLITE IMAGERY ────────────────────────────────────────────────────────────####
          
          if ("Satellite Imagery" %in% input$run_modules) {
            
            showTab(inputId = "module_tabs", target = "Satellite Imagery")
            incProgress(1, detail = "Loading satellite imagery...")
            
            v     <- vect(new_ws2 %>% st_transform(4326))
            ext4326 <- terra::ext(new_ws2_4326)
            
            r1985 <- terra::rast("/vsicurl/https://bcbasin.s3.ca-central-1.amazonaws.com/1985_1990v3_COG_AV_JP_BIG.tif",
                                 win = ext4326)
            output$plot_landsat_1985 <- renderPlot({
              plot(r1985, main = "Landsat TM — 1985–1990 mosaic (true colour)", mar = 2)
              plot(v, add = TRUE, border = "red", lwd = 2)
            }, height = 800, width = 800)
            output$text_plot_landsat_1985 <- renderUI({ caption(
              "<b>Landsat 5 TM mosaic, 1985–1990</b> — cloud-free median composite of Landsat 5 Thematic Mapper ",
              "surface reflectance imagery (Bands 3-2-1 = RGB true colour). The 5-year window improves cloud ",
              "and shadow gap-filling in cloudy coastal and mountain environments. Spatial resolution: 30 m.<br>",
              "This image provides a baseline land-cover snapshot for assessing long-term change — particularly ",
              "forest cover, glacier extent, and land clearing — relative to the 2020 and 2023 composites.<br>",
              "<b>Source:</b> Landsat 5 TM Collection 2 Level-2 surface reflectance (USGS). Processed and ",
              "mosaicked by BC Ministry of Forests. Served as a JPEG-compressed Cloud-Optimised GeoTIFF ",
              "(COG) from AWS S3 (ca-central-1). Watershed boundary overlay in red."
            )})
            
            r2020 <- terra::rast("/vsicurl/https://bcbasin.s3.ca-central-1.amazonaws.com/2020_2023v3_COG_AV_JP_BIG.tif",
                                 win = ext4326)
            output$plot_landsat_2020 <- renderPlot({
              plot(r2020, main = "Landsat OLI — 2020–2023 mosaic (true colour)", mar = 2)
              plot(v, add = TRUE, border = "red", lwd = 2)
            }, height = 800, width = 800)
            output$text_plot_landsat_2020 <- renderUI({ caption(
              "<b>Landsat 8/9 OLI mosaic, 2020–2023</b> — cloud-free median composite of Landsat 8 and 9 ",
              "Operational Land Imager (OLI) surface reflectance imagery (Bands 4-3-2 = RGB true colour). ",
              "Spatial resolution: 30 m. Cross-compare with the 1985–1990 composite to identify deforestation, ",
              "wildfire scars, glacier retreat, lake changes, or land development over the ~35-year interval.<br>",
              "<b>Source:</b> Landsat 8/9 OLI Collection 2 Level-2 surface reflectance (USGS). Processed and ",
              "mosaicked by BC Ministry of Forests. Served as a COG from AWS S3 (ca-central-1)."
            )})
            
            r2023 <- terra::rast("/vsicurl/https://bcbasin.s3.ca-central-1.amazonaws.com/BC_2023v2_4326_v2_bigTiff_JPEG.tif",
                                 win = ext4326)
            output$plot_sentinel_2023 <- renderPlot({
              plot(r2023, main = "Sentinel-2 MSI — 2023 mosaic (true colour)", mar = 2)
              plot(v, add = TRUE, border = "red", lwd = 2)
            }, height = 800, width = 800)
            output$text_plot_sentinel_2023 <- renderUI({ caption(
              "<b>Sentinel-2 MSI mosaic, 2023</b> — cloud-free median composite of Sentinel-2 Level-2A ",
              "surface reflectance imagery (Bands B4-B3-B2 = RGB true colour). Spatial resolution: 10 m ",
              "(3× finer than Landsat), enabling detection of finer landscape features such as individual ",
              "cutblock units, riparian corridors, small water bodies, and built infrastructure.<br>",
              "<b>Download:</b> Use the 'Imagery — S2 2023' download button to retrieve the clipped GeoTIFF ",
              "for use in GIS software.<br>",
              "<b>Source:</b> Copernicus Sentinel-2 MSI (ESA). Level-2A atmospheric correction via sen2cor. ",
              "Mosaicked by BC Ministry of Forests. Served as a COG from AWS S3 (ca-central-1). ",
              "Contains modified Copernicus Sentinel data 2023."
            )})
            
          } # end Satellite Imagery
          
          # UPDATE LEAFLET MAP ───────────────────────────────────────────────────────────####
          
          incProgress(1, detail = "Update map")
          bbbb <- st_bbox(new_ws2 %>% st_transform(4326))
          
          # Update the existing map in place instead of re-rendering it: keeps the user's
          # basemap / lidar / landslide state and avoids resending every base layer.
          # (The watershed outline is already on the map from the selection step.)
          map_proxy <- leafletProxy("mymap") %>%
            clearGroup(RESULT_GROUPS) %>%
            removeControl("legend_points") %>% removeControl("legend_polygons")

          if ("Forest Disturbance" %in% input$run_modules) {
            dra_map <- map_simplify(dra)
            wf_map  <- map_simplify(my_wf %>% filter(clipped_area_m2>0))
            cb_map  <- map_simplify(my_cb %>% filter(clipped_area_m2>0))
            map_proxy %>%
              addPolylines(data=dra_map, group="Roads", color="black", weight=1,
                           label=map_label(dra_map$transport_line_surface_code_desc)) %>%
              addPolygons(data=wf_map, group="Fire",
                          fillColor="red", color="red", weight=2, fillOpacity=0.8, label=map_label(if (nrow(wf_map)) paste0("Fire year: ",wf_map$fire_year))) %>%
              addPolygons(data=cb_map, group="Cutblock",
                          fillColor="darkgreen", color="darkgreen", weight=1, fillOpacity=0.8, label=map_label(if (nrow(cb_map)) paste0("Harvest year: ",cb_map$harvest_year)))
          }

          if ("Water Allocations" %in% input$run_modules) {
            map_proxy %>%
              addCircles(data=wrl, lng=wrl$lon, lat=wrl$lat, color="red", fillColor="red", group="Water Rights",
                         label=paste0(wrl$POD_STATUS," — ",wrl$PRIMARY_LICENSEE_NAME),
                         popup=~paste("<h3>Water Licence:</h3>",wrl$PRIMARY_LICENSEE_NAME,"<br>",
                                      "<b>Type:</b>",wrl$POD_SUBTYPE,"<br><b>Use:</b>",wrl$PURPOSE_USE,"<br>",
                                      "<b>Quantity:</b>",wrl$QUANTITY,wrl$QUANTITY_UNITS)) %>%
              addCircles(data=wap, lng=wap$lon, lat=wap$lat, color="blue", fillColor="blue", group="Water Approvals",
                         label=paste0(wap$APPROVAL_STATUS," — ",wap$APPROVAL_TYPE),
                         popup=~paste("<h3>Water Approval:</h3>",wap$APPROVAL_TYPE,"<br>",
                                      "<b>Works:</b>",wap$WORKS_DESCRIPTION,"<br>",
                                      "<b>Quantity:</b>",wap$QUANTITY,wap$QUANTITY_UNITS))
          }

          if ("Streamflow and Freshwater" %in% input$run_modules) {
            random_hex_color <- function() paste0("#",paste(sample(c(0:9,letters[1:6]),6,replace=TRUE),collapse=""))
            sub_basins <- my_named[my_named$Location %in% c("Upstream","Watershed of Interest"),] %>%
              mutate(weight=1+(as.numeric(area_km2)/max(area_km2)*3)) %>%
              arrange(-area_km2) %>% rowwise() %>% mutate(random_color=random_hex_color())
            if (nrow(sub_basins) > 0) {                        # ← add this guard
              sub_map <- map_simplify(sub_basins)
              wl_map  <- map_simplify(my_wl %>% filter(clipped_area_m2>0))
              lk_map  <- map_simplify(my_lk %>% filter(clipped_area_m2>0))
              gl_map  <- map_simplify(my_gl %>% filter(clipped_area_m2>0))
              map_proxy %>%
                addPolygons(data=sub_map, group="Sub-basins",
                            label=map_label(sub_map$gnis_name), fillOpacity=0.1,
                            weight=sub_map$weight, color=sub_map$random_color) %>%
                addPolygons(data=wl_map, group="FWA Wetland",
                            fillColor="yellow",color="pink",weight=1,fillOpacity=0.3,label=map_label(wl_map$waterbody_type)) %>%
                addPolygons(data=lk_map, group="FWA Lake",
                            fillColor="steelblue",color="steelblue",weight=1,fillOpacity=1,label=map_label(lk_map$waterbody_type)) %>%
                addPolygons(data=gl_map, group="FWA Glacier",
                            fillColor="grey",color="grey",weight=1,fillOpacity=0.3,label=map_label(gl_map$waterbody_type)) %>%
                addPolygons(data=map_simplify(my_gl_1985 %>% filter(FEATURE_AREA_SQM>0)), group="Glacier 1985",
                            fillColor="brown",color="brown",weight=1,fillOpacity=0.3) %>%
                addPolygons(data=map_simplify(my_gl_2021 %>% filter(FEATURE_AREA_SQM>0)), group="Glacier 2021",
                            fillColor="blue",color="blue",weight=1,fillOpacity=0.3)
            }
          }

          map_proxy %>%
            addLayersControl(
              baseGroups    = BASE_GROUPS,
              overlayGroups = c(REF_OVERLAYS, RESULT_GROUPS),
              options = layersControlOptions(collapsed=TRUE)) %>%
            hideGroup(c("Roads","Glacier 1985","Glacier 2021")) %>%
            fitBounds(bbbb$xmin[[1]], bbbb$ymin[[1]], bbbb$xmax[[1]], bbbb$ymax[[1]]) %>%
            addLegend("bottomleft",  colors=c("blue","red"), layerId="legend_points",
                      labels=c("Water Approvals","Water Rights"), title="Points", opacity=1) %>%
            addLegend("bottomright", colors=c("yellow","steelblue","grey","brown","blue","red","darkgreen"), layerId="legend_polygons",
                      labels=c("FWA Wetland","FWA Lake","FWA Glacier","Glacier 1985","Glacier 2021","Wildfire","Cutblock"),
                      title="Polygons", opacity=1)

          # MAP FOR REPORT 
          
          # ── Static ggplot map for PDF export (never rendered in UI) ──────────────────
          gg_map_static <- ggplot() +
            geom_sf(data = new_ws2 %>% st_transform(4326),
                    fill = NA, colour = "black", linewidth = 0.8)
          
          if ("Forest Disturbance" %in% input$run_modules) {
            if (nrow(my_wf) > 0 && any(my_wf$clipped_area_m2 > 0))
              gg_map_static <- gg_map_static +
                geom_sf(data = my_wf %>% filter(clipped_area_m2 > 0) %>% st_transform(4326),
                        fill = "red", colour = "red", alpha = 0.6)
            if (nrow(my_cb) > 0 && any(my_cb$clipped_area_m2 > 0))
              gg_map_static <- gg_map_static +
                geom_sf(data = my_cb %>% filter(clipped_area_m2 > 0) %>% st_transform(4326),
                        fill = "darkgreen", colour = "darkgreen", alpha = 0.6)
          }
          
          if ("Streamflow and Freshwater" %in% input$run_modules) {
            if (nrow(my_lk) > 0 && any(my_lk$clipped_area_m2 > 0))
              gg_map_static <- gg_map_static +
                geom_sf(data = my_lk %>% filter(clipped_area_m2 > 0) %>% st_transform(4326),
                        fill = "steelblue", colour = "steelblue")
            if (nrow(my_gl) > 0 && any(my_gl$clipped_area_m2 > 0))
              gg_map_static <- gg_map_static +
                geom_sf(data = my_gl %>% filter(clipped_area_m2 > 0) %>% st_transform(4326),
                        fill = "grey80", colour = "grey60")
          }
          
          if ("Water Allocations" %in% input$run_modules) {
            if (nrow(wrl) > 0)
              gg_map_static <- gg_map_static +
                geom_point(data = wrl, aes(lon, lat), colour = "red",  size = 1.5, shape = 16)
            if (nrow(wap) > 0)
              gg_map_static <- gg_map_static +
                geom_point(data = wap, aes(lon, lat), colour = "blue", size = 1.5, shape = 16)
          }
          
          gg_map_static <- gg_map_static +
            annotation_scale(location = "bl") +
            annotation_north_arrow(location = "tr", style = north_arrow_minimal()) +
            theme_bw() +
            labs(title = paste0(new_ws2$gnis_name, " — ", round(new_ws2$area_km2, 1), " km²"),
                 subtitle = input$watershed_source,
                 x = "Longitude", y = "Latitude")
          
          # DOWNLOAD HANDLERS ────────────────────────────────────────────────────────────
          
          output$downloadWatershed  <- downloadHandler(
            filename = function() paste0(gsub(" ","-",new_ws2$gnis_name),"_",new_ws2$gnis_id,"_watershed.kml"),
            content  = function(file) st_write(new_ws2 %>% rename(Name=gnis_name,Description=area_km2) %>%
                                                 mutate(Description=paste0(round(Description,2)," km²")) %>%
                                                 select(Name,Description), file))
          output$downloadSubbasins  <- downloadHandler(
            filename = function() paste0(gsub(" ","-",new_ws2$gnis_name),"_",new_ws2$gnis_id,"_subbasins.kml"),
            content  = function(file) st_write(sub_basins %>% rename(Name=gnis_name,Description=area_km2) %>%
                                                 mutate(Description=paste0(round(Description,2)," km²")) %>%
                                                 select(Name,Description), file))
          output$downloadImagery1985 <- downloadHandler(
            filename = function() paste0(gsub(" ","-",new_ws2$gnis_name),"_",new_ws2$gnis_id,"_L5_1985.tif"),
            content  = function(file) terra::writeRaster(r1985, file))
          output$downloadImagery2020 <- downloadHandler(
            filename = function() paste0(gsub(" ","-",new_ws2$gnis_name),"_",new_ws2$gnis_id,"_L8_2020.tif"),
            content  = function(file) terra::writeRaster(r2020, file))
          output$downloadImagery2023 <- downloadHandler(
            filename = function() paste0(gsub(" ","-",new_ws2$gnis_name),"_",new_ws2$gnis_id,"_S2_2023.tif"),
            content  = function(file) terra::writeRaster(r2023, file))
          output$downloadFire <- downloadHandler(
            filename = function() paste0(gsub(" ", "-", new_ws2$gnis_name), "_", new_ws2$gnis_id, "_fire.gpkg"),
            content  = function(file) {
              dat <- my_wf %>% filter(clipped_area_m2 > 0)
              if (nrow(dat) > 0) st_write(dat %>% st_transform(4326), file, driver = "GPKG", delete_dsn = TRUE)
            })
          
          output$downloadCutblocks <- downloadHandler(
            filename = function() paste0(gsub(" ", "-", new_ws2$gnis_name), "_", new_ws2$gnis_id, "_cutblocks.gpkg"),
            content  = function(file) {
              dat <- my_cb %>% filter(clipped_area_m2 > 0)
              if (nrow(dat) > 0) st_write(dat %>% st_transform(4326), file, driver = "GPKG", delete_dsn = TRUE)
            })
          
          output$downloadRoads <- downloadHandler(
            filename = function() paste0(gsub(" ", "-", new_ws2$gnis_name), "_", new_ws2$gnis_id, "_roads.gpkg"),
            content  = function(file) {
              if (nrow(dra) > 0) st_write(dra %>% st_transform(4326), file, driver = "GPKG", delete_dsn = TRUE)
            })
          
          output$downloadLakes <- downloadHandler(
            filename = function() paste0(gsub(" ", "-", new_ws2$gnis_name), "_", new_ws2$gnis_id, "_lakes.gpkg"),
            content  = function(file) {
              dat <- my_lk %>% filter(clipped_area_m2 > 0)
              if (nrow(dat) > 0) st_write(dat %>% st_transform(4326), file, driver = "GPKG", delete_dsn = TRUE)
            })
          
          output$downloadWetlands <- downloadHandler(
            filename = function() paste0(gsub(" ", "-", new_ws2$gnis_name), "_", new_ws2$gnis_id, "_wetlands.gpkg"),
            content  = function(file) {
              dat <- my_wl %>% filter(clipped_area_m2 > 0)
              if (nrow(dat) > 0) st_write(dat %>% st_transform(4326), file, driver = "GPKG", delete_dsn = TRUE)
            })
          
          output$downloadGlaciers <- downloadHandler(
            filename = function() paste0(gsub(" ", "-", new_ws2$gnis_name), "_", new_ws2$gnis_id, "_glaciers.gpkg"),
            content  = function(file) {
              # Write FWA glaciers as default layer, add 1985/2021 as extra layers
              gl_fwa <- my_gl %>% filter(clipped_area_m2 > 0)
              if (nrow(gl_fwa) > 0) {
                st_write(gl_fwa %>% st_transform(4326), file,
                         layer = "glaciers_fwa", driver = "GPKG", delete_dsn = TRUE)
              }
              gl_85 <- my_gl_1985 %>% filter(FEATURE_AREA_SQM > 0)
              if (nrow(gl_85) > 0) {
                st_write(gl_85 %>% st_transform(4326) %>%
                           select(any_of(c("GLACIER_ID", "SOURCE_YEAR", "clipped_area_m2"))),
                         file, layer = "glaciers_1985", driver = "GPKG", append = TRUE)
              }
              gl_21 <- my_gl_2021 %>% filter(FEATURE_AREA_SQM > 0)
              if (nrow(gl_21) > 0) {
                st_write(gl_21 %>% st_transform(4326) %>%
                           select(any_of(c("GLACIER_ID", "SOURCE_YEAR", "clipped_area_m2"))),
                         file, layer = "glaciers_2021", driver = "GPKG", append = TRUE)
              }
            })
          
          output$downloadDEM <- downloadHandler(
            filename = function() paste0(gsub(" ", "-", new_ws2$gnis_name), "_", new_ws2$gnis_id, "_DEM_30m.tif"),
            content  = function(file) terra::writeRaster(dem, file, overwrite = TRUE))
          
          # ── Download ALL spatial data (.zip) ─────────────────────────────────
          output$downloadAllSpatial <- downloadHandler(
            filename = function() paste0(gsub(" ", "-", new_ws2$gnis_name), "_", new_ws2$gnis_id, "_all_spatial.zip"),
            content  = function(file) {
              withProgress(message = "Zipping...", max = 3, {
                
                tmpdir <- tempdir()
                outdir <- file.path(tmpdir, "spatial_export")
                if (dir.exists(outdir)) unlink(outdir, recursive = TRUE)
                dir.create(outdir)
                
                ws_prefix <- gsub(" ", "-", new_ws2$gnis_name)
                
                # Helper to write vector layers safely
                safe_write_vec <- function(dat, fname, filter_col = "clipped_area_m2") {
                  if (!is.null(dat) && nrow(dat) > 0) {
                    if (!is.null(filter_col) && filter_col %in% names(dat))
                      dat <- dat %>% filter(.data[[filter_col]] > 0)
                    if (nrow(dat) > 0)
                      st_write(dat %>% st_transform(4326),
                               file.path(outdir, fname), driver = "GPKG",
                               delete_dsn = TRUE, quiet = TRUE)
                  }
                }
                
                # ── Boundaries ──
                st_write(new_ws2 %>% rename(Name = gnis_name, Description = area_km2) %>%
                           mutate(Description = paste0(round(Description, 2), " km²")) %>%
                           select(Name, Description),
                         file.path(outdir, paste0(ws_prefix, "_watershed.kml")),
                         quiet = TRUE)
                
                if (exists("sub_basins") && nrow(sub_basins) > 0)
                  st_write(sub_basins %>% rename(Name = gnis_name, Description = area_km2) %>%
                             mutate(Description = paste0(round(Description, 2), " km²")) %>%
                             select(Name, Description),
                           file.path(outdir, paste0(ws_prefix, "_subbasins.kml")),
                           quiet = TRUE)
                
                # ── DEM ──
                if (exists("dem"))
                  terra::writeRaster(dem,
                                     file.path(outdir, paste0(ws_prefix, "_DEM_30m.tif")),
                                     overwrite = TRUE)
                
                # ── Disturbance (if Forest Disturbance module was run) ──
                if ("Forest Disturbance" %in% input$run_modules) {
                  if (exists("my_wf")) safe_write_vec(my_wf, paste0(ws_prefix, "_fire.gpkg"))
                  if (exists("my_cb")) safe_write_vec(my_cb, paste0(ws_prefix, "_cutblocks.gpkg"))
                  if (exists("dra") && nrow(dra) > 0)
                    st_write(dra %>% st_transform(4326),
                             file.path(outdir, paste0(ws_prefix, "_roads.gpkg")),
                             driver = "GPKG", delete_dsn = TRUE, quiet = TRUE)
                }
                
                # ── Water features (if Streamflow module was run) ──
                if ("Streamflow and Freshwater" %in% input$run_modules) {
                  if (exists("my_lk")) safe_write_vec(my_lk, paste0(ws_prefix, "_lakes.gpkg"))
                  if (exists("my_wl")) safe_write_vec(my_wl, paste0(ws_prefix, "_wetlands.gpkg"))
                  
                  # Glaciers — multi-layer gpkg
                  gl_path <- file.path(outdir, paste0(ws_prefix, "_glaciers.gpkg"))
                  gl_written <- FALSE
                  if (exists("my_gl") && nrow(my_gl %>% filter(clipped_area_m2 > 0)) > 0) {
                    st_write(my_gl %>% filter(clipped_area_m2 > 0) %>% st_transform(4326),
                             gl_path, layer = "glaciers_fwa", driver = "GPKG",
                             delete_dsn = TRUE, quiet = TRUE)
                    gl_written <- TRUE
                  }
                  if (exists("my_gl_1985") && nrow(my_gl_1985 %>% filter(FEATURE_AREA_SQM > 0)) > 0) {
                    st_write(my_gl_1985 %>% filter(FEATURE_AREA_SQM > 0) %>% st_transform(4326) %>%
                               select(any_of(c("GLACIER_ID", "SOURCE_YEAR", "clipped_area_m2"))),
                             gl_path, layer = "glaciers_1985", driver = "GPKG",
                             delete_dsn = !gl_written, append = gl_written, quiet = TRUE)
                    gl_written <- TRUE
                  }
                  if (exists("my_gl_2021") && nrow(my_gl_2021 %>% filter(FEATURE_AREA_SQM > 0)) > 0) {
                    st_write(my_gl_2021 %>% filter(FEATURE_AREA_SQM > 0) %>% st_transform(4326) %>%
                               select(any_of(c("GLACIER_ID", "SOURCE_YEAR", "clipped_area_m2"))),
                             gl_path, layer = "glaciers_2021", driver = "GPKG",
                             delete_dsn = !gl_written, append = gl_written, quiet = TRUE)
                  }
                }
                
                # ── Imagery (if Satellite Imagery module was run) ──
                if ("Satellite Imagery" %in% input$run_modules) {
                  if (exists("r1985")) terra::writeRaster(r1985,
                                                          file.path(outdir, paste0(ws_prefix, "_L5_1985.tif")), overwrite = TRUE)
                  if (exists("r2020")) terra::writeRaster(r2020,
                                                          file.path(outdir, paste0(ws_prefix, "_L8_2020.tif")), overwrite = TRUE)
                  if (exists("r2023")) terra::writeRaster(r2023,
                                                          file.path(outdir, paste0(ws_prefix, "_S2_2023.tif")), overwrite = TRUE)
                }
                
                # ── Zip it ──
                files_to_zip <- list.files(outdir, full.names = TRUE)
                zip(file, files = files_to_zip, flags = "-j")})
            })
          
          output$downloadPDF <- downloadHandler(
            filename = function() paste0(gsub(" ", "-", new_ws2$gnis_name), "_", new_ws2$gnis_id, "_", format(Sys.time(), "%Y%m%d_%H%M%S"), "_report.pdf"),
            contentType = "application/pdf",       # ← add this
            content = function(file) {
              
              # Helper: return object only if module was run, otherwise NULL
              mod <- function(module_name, obj) {
                if (module_name %in% input$run_modules) obj else NULL
              }
              
              render_report(my_params = list(
                
                # ── Watershed summary ──────────────────────────────────────────────────
                watershed_name     = new_ws2$gnis_name,
                watershed_source   = input$watershed_source,
                watershed_area_km2 = round(new_ws2$area_km2, 1),
                run_modules        = input$run_modules,
                # new_leaflet        = new_leaflet,
                gg_map_static = gg_map_static,
                
                # ── Streamflow & Freshwater ────────────────────────────────────────────
                gg_discharge = mod("Streamflow and Freshwater", gg_plot_discharge2()),
                gg_hyps      = mod("Streamflow and Freshwater", gg_hyps),
                gg_fwa       = mod("Streamflow and Freshwater", gg_my_fwa),
                table_named  = mod("Streamflow and Freshwater",
                                   my_named %>% st_drop_geometry() %>%
                                     mutate(area_km2 = round(area_km2, 1)) %>%
                                     rename(gnis_name = gnis_name, area_km2 = area_km2)),
                
                # ── Forest Disturbance ─────────────────────────────────────────────────
                gg_timeseries        = mod("Forest Disturbance", gg_timeseries),
                gg_timeseries_cumsum = mod("Forest Disturbance", gg_timeseries_cumsum),
                gg_eca               = mod("Forest Disturbance", gg_eca),
                gg_elevbins          = mod("Forest Disturbance", gg_elevbins),
                gg_dra               = mod("Forest Disturbance", gg_dra),
                
                # ── CEF Human Disturbance ─────────────────────────────────────────────
                bc_cef_2021_sum      = mod("CEF Human Disturbance - 2021", bc_cef_2021_sum),
                bc_cef_2021_sum_flag = mod("CEF Human Disturbance - 2021", bc_cef_2021_sum_flag),
                
                # ── Stream Profile ─────────────────────────────────────────────────────
                gg_profile = mod("Stream Profile", gg_profile),
                
                # ── Water Allocations ──────────────────────────────────────────────────
                water_auth    = mod("Water Allocations", water_auth),
                gg_water_auth = mod("Water Allocations", gg_water_auth),
                
                # ── ClimateBC ──────────────────────────────────────────────────────────
                gg_mat      = mod("ClimateBC", gg_mat),
                gg_map_clim = mod("ClimateBC", gg_map_clim),
                gg_pas      = mod("ClimateBC", gg_pas),
                gg_cmd      = mod("ClimateBC", gg_cmd),
                
                # ── Snow Remote Sensing ────────────────────────────────────────────────
                p_snow_map  = mod("Snow Remote Sensing", p_snow_map),
                p_snow_elev = mod("Snow Remote Sensing", p_snow_elev),
                
                # ── Landslide Zonation ─────────────────────────────────────────────────
                gg_rlf_map  = mod("Landslide Zonation", gg_rlf_map),
                gg_rlf_mapz = mod("Landslide Zonation", gg_rlf_mapz),
                gg_rlf_slp  = mod("Landslide Zonation", gg_rlf_slp),
                gg_rlf_slpz = mod("Landslide Zonation", gg_rlf_slpz),
                gg_rlf_ls   = mod("Landslide Zonation", gg_rlf_ls),
                
                # ── Satellite Imagery ──────────────────────────────────────────────────
                r1985 = mod("Satellite Imagery", r1985),
                r2020 = mod("Satellite Imagery", r2020),
                r2023 = mod("Satellite Imagery", r2023)
              ), pdf_path = file)
            }
          )
          
          # FINISH ───────────────────────────────────────────────────────────────────────
          
          time <- paste0(round(as.numeric(strsplit(toc()$callback_msg," ")[[1]][1])/60, 1), " min elapsed")
          output$ws_run <- renderText({ time })
          output$ws_selection_pred_time <- renderText({ "Processing complete! ✓" })
          updateTabsetPanel(session, "module_tabs", selected = "Overview")
          
          dbWriteTable(conn, "usage",
                       data.frame(date_time=as.character(session_start), session_token=session_token,
                                  gnis_name=new_ws()$gnis_name, gnis_id=new_ws()$gnis_id,
                                  processing_time=time, action="processing",
                                  area_km2=round(new_ws()$area_km2,1), basin_source=basin_source()),
                       append=TRUE)
          
          # ── Always show ──
          shinyjs::show("downloadAllSpatial")
          # shinyjs::show("downloadWatershed")
          # shinyjs::show("downloadSubbasins")
          # shinyjs::show("downloadDEM")
          shinyjs::show("downloadPDF")
          
          # ── Conditional on modules ──
          # if ("Forest Disturbance" %in% input$run_modules) {
          #   shinyjs::show("downloadFire")
          #   shinyjs::show("downloadCutblocks")
          #   shinyjs::show("downloadRoads")}
          # if ("Streamflow and Freshwater" %in% input$run_modules) {
          #   shinyjs::show("downloadLakes")
          #   shinyjs::show("downloadWetlands")
          #   shinyjs::show("downloadGlaciers")}
          # if ("Satellite Imagery" %in% input$run_modules) {
          #   shinyjs::show("downloadImagery1985")
          #   shinyjs::show("downloadImagery2020")
          #   shinyjs::show("downloadImagery2023")}
          
          
        }) # end withProgress
      }   # end area < 20000
    } else {
      output$ws_run <- renderText("")
      Sys.sleep(1)
      output$ws_run <- renderText("Please select a watershed first.")
    }
  }) # end observeEvent run_button
  
  # USAGE PLOTS ──────────────────────────────────────────────────────────────────
  
  usage_plots <- reactive(make_usage_plots())
  
  output$usage_visits <- renderPlot({ usage_plots()$visits })
  output$usage_recent <- renderPlot({ usage_plots()$recent })
  output$usage_top    <- renderPlot({ usage_plots()$top    })
  output$usage_total  <- renderPlot({ usage_plots()$total  })
  
}

shinyApp(ui, server, enableBookmarking = "url")