# ── Station networks menu ───────────────────────────────────────────────────
# A "Stations" menu on the map (below Snow) toggles WSC hydrometric stations plus ECCC,
# BC Wildfire Service, MOTI, NBCClim and RemoteLogger station locations. The non-WSC
# snapshot is built by functions_prep/station_networks.R. Each layer is hidden by default
# and its points are only sent to the browser the first time it is switched on
# (input$stn_load); the WSC points are already on the map (app_5_leaflet.R).

STN_CSV <- "data/stations/station_networks.csv"

# Layer name -> colour, in menu order (after WSC)
STATION_NETWORKS <- c(
  "ECCC Climate Active"       = "#6a51a3",
  "ECCC Climate Discontinued" = "#bcbddc",
  "BCWS Weather"              = "#d94801",
  "MOTI Weather"              = "#e6ab02",
  "NBCClim"                   = "#1b9e77",
  "RemoteLogger"              = "#e7298a"
)
STN_LONG <- c(ECCC = "Environment Canada climate station", BCWS = "BC Wildfire Service weather station",
              MOTI = "MOTI avalanche & weather station", NBCClim = "Northern BC climate research station",
              RemoteLogger = "RemoteLogger research station")
STN_LINK <- c(ECCC = "ECCC climate data", BCWS = "BCWS weather app", MOTI = "BC Data Catalogue",
              NBCClim = "NBCClim app", RemoteLogger = "RemoteLogger app")

stn_pts <- if (file.exists(STN_CSV)) read.csv(STN_CSV, stringsAsFactors = FALSE, encoding = "UTF-8") else NULL
if (!is.null(stn_pts)) {
  stn_pts <- stn_pts[stn_pts$group %in% names(STATION_NETWORKS), ]
  esc <- htmltools::htmlEscape
  stn_pts$label <- sprintf("%s (%s %s)", stn_pts$name, stn_pts$network, stn_pts$id)
  stn_pts$popup <- sprintf(
    "<b>%s</b><br>%s<br>ID: %s%s<br>%s<br><a href='%s' target='_blank' rel='noopener'>%s</a><br><span style='color:#888'>%.5f, %.5f · list fetched %s</span>",
    esc(stn_pts$name), STN_LONG[stn_pts$network], esc(stn_pts$id),
    ifelse(is.na(stn_pts$elev_m), "", sprintf("<br>Elevation: %s m", format(round(stn_pts$elev_m), big.mark = ","))),
    esc(stn_pts$detail), stn_pts$url, STN_LINK[stn_pts$network], stn_pts$lat, stn_pts$lon, stn_pts$fetched)
}
STATION_GROUPS <- if (is.null(stn_pts)) character() else intersect(names(STATION_NETWORKS), unique(stn_pts$group))

add_station_points <- function(map, group) {
  d <- stn_pts[stn_pts$group == group, ]
  col <- STATION_NETWORKS[[group]]
  addCircleMarkers(map, lng = d$lon, lat = d$lat, group = group, radius = 4, weight = 1,
                   color = "#333333", opacity = 0.8, fillColor = col, fillOpacity = 0.9,
                   label = d$label, popup = d$popup,
                   # a click opens the popup only, not the map click (delineation / snow values)
                   options = pathOptions(bubblingMouseEvents = FALSE))
}

# Everything the Stations menu lists: WSC (always present) then the other networks
STATION_MENU <- c("WSC Active" = "steelblue", "WSC Discontinued" = "grey", STATION_NETWORKS[STATION_GROUPS])

station_control <- function() {
  n <- table(stn_pts$group)
  n <- c("WSC Active" = sum(wsc_pp$status == "active"), "WSC Discontinued" = sum(wsc_pp$status == "discontinued"),
         setNames(as.integer(n), names(n)))
  rows <- lapply(names(STATION_MENU), function(g)
    tags$label(
      style = "display:flex; align-items:center; padding:1px 0; cursor:pointer; font-weight:normal; white-space:nowrap;",
      tags$input(type = "checkbox", class = "stn-layer-chk", `data-group` = g,
                 `data-lazy` = if (g %in% STATION_GROUPS) "true" else "false",
                 checked = if (g == "WSC Active") NA, style = "margin:0 5px 0 0;"),
      tags$span(style = sprintf("display:inline-block; width:9px; height:9px; border-radius:50%%; border:1px solid #333; margin-right:5px; background:%s;",
                                STATION_MENU[[g]])),
      g, tags$span(style = "color:#888; margin-left:4px;", format(n[[g]], big.mark = ","))))
  fetched <- if (!is.null(stn_pts)) max(stn_pts$fetched) else NA
  tags$div(
    id = "stn-control",
    style = "background:white; border-radius:4px; box-shadow:0 1px 5px rgba(0,0,0,0.4); font:12px/1.5 'Helvetica Neue',Arial,sans-serif; padding:6px 12px; max-width:260px;",
    tags$label(
      style = "display:flex; align-items:center; font-weight:bold; font-size:13px; cursor:pointer; margin:0;",
      tags$input(type = "checkbox", id = "stn-master-chk", style = "margin-right:6px;"),
      "Stations (beta)",
      tags$span(id = "stn-info", title = "About these layers (click)",
                style = "margin-left:8px; cursor:pointer; color:#0b6a88; font-weight:normal;", HTML("&#9432;")),
      tags$span(id = "stn-caret", title = "Show/hide networks",
                style = "margin-left:auto; padding-left:10px; cursor:pointer;", HTML("&#9662;"))),
    tags$div(id = "stn-list", style = "display:none; margin-top:4px;", rows),
    tags$div(
      id = "stn-help",
      style = "display:none; margin-top:6px; padding-top:6px; border-top:1px solid #ddd; white-space:normal;",
      tags$b("Station networks"), " (hover for the name, click for details and a link):", tags$br(),
      tags$b("WSC"), " Water Survey of Canada hydrometric stations. ",
      tags$b("ECCC"), " Environment Canada climate stations (active = data in the last year). ",
      tags$b("BCWS"), " BC Wildfire Service weather stations. ",
      tags$b("MOTI"), " Ministry of Transportation avalanche & weather stations. ",
      tags$b("NBCClim"), " Northern BC climate research stations. ",
      tags$b("RemoteLogger"), " research hydrometric stations (active and offline).",
      if (!is.na(fetched)) tags$div(style = "color:#888; margin-top:4px;", paste("Non-WSC lists fetched", fetched))
    )
  )
}

# Menu control + lazy-load hook. Groups are switched directly on the map (not through
# the layers control), so the WSC and network layers are left out of REF_OVERLAYS.
add_station_networks <- function(map) {
  map %>%
    hideGroup(STATION_GROUPS) %>%
    addControl(station_control(), position = "topleft", className = "stn-panel") %>%
    htmlwidgets::onRender("
    function(el, x) {
      var map = this, ctrl = document.getElementById('stn-control');
      if (!ctrl) return;
      var master = document.getElementById('stn-master-chk'), list = document.getElementById('stn-list'),
          caret = document.getElementById('stn-caret'), info = document.getElementById('stn-info'),
          help = document.getElementById('stn-help'), cbs = ctrl.querySelectorAll('.stn-layer-chk'),
          collapsed = false, requested = {};
      L.DomEvent.disableClickPropagation(ctrl);
      L.DomEvent.disableScrollPropagation(ctrl);
      function group(name) { return map.layerManager.getLayerGroup(name, true); }
      function apply() {
        caret.innerHTML = collapsed ? '&#9656;' : '&#9662;';
        caret.style.visibility = master.checked ? 'visible' : 'hidden';
        list.style.display = master.checked && !collapsed ? 'block' : 'none';
        cbs.forEach(function(cb) {
          var name = cb.getAttribute('data-group'), g = group(name), on = master.checked && cb.checked;
          if (on && cb.getAttribute('data-lazy') === 'true' && !requested[name] && window.Shiny) {
            requested[name] = true;
            Shiny.setInputValue('stn_load', name, { priority: 'event' });
          }
          if (on && !map.hasLayer(g)) map.addLayer(g);
          if (!on && map.hasLayer(g)) map.removeLayer(g);
        });
      }
      master.checked = false;
      apply();
      master.addEventListener('change', apply);
      cbs.forEach(function(cb) { cb.addEventListener('change', apply); });
      caret.addEventListener('click', function(e) {
        e.preventDefault(); e.stopPropagation();        // don't toggle the Stations checkbox
        collapsed = !collapsed; apply();
      });
      info.addEventListener('click', function(e) {
        e.preventDefault(); e.stopPropagation();
        help.style.display = (help.style.display === 'none') ? 'block' : 'none';
      });
    }")
}

station_networks_server <- function(input, output, session, map_id = "mymap") {
  observeEvent(input$stn_load, {
    req(input$stn_load %in% STATION_GROUPS)
    leafletProxy(map_id) %>% add_station_points(input$stn_load)
  })
}
