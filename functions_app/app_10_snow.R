# ── Snow metrics on the map ─────────────────────────────────────────────────
# Gridded mean snow onset (SD-on), melt-out (SD-off) and duration (SD-dur) from
# snow_metrics_cog.tif (~460 m; bands: on, off, dur, observation count; on/off are days
# after Sep 1). A "Snow" panel on the map shows one metric as a coloured overlay
# (pre-rendered PNGs in www/snow/, see functions_prep/snow_metrics_overlays.R).
# While it is on, a map click shows the values at that point instead of starting a
# delineation. The elevation profile tool (app_9) adds the metrics along its line.
# Method: Bevington et al. (2019), The Cryosphere 13, 2693–2712, doi:10.5194/tc-13-2693-2019

SNOW_COG     <- "https://bcbasin.s3.ca-central-1.amazonaws.com/snow_metrics_cog.tif"
SNOW_DEM     <- "https://bcbasin.s3.ca-central-1.amazonaws.com/COP_GLO30_FOR_watershedBC_cog.tif"
SNOW_DIR     <- "www/snow"
SNOW_ORIGIN  <- as.Date("2001-09-01")   # day 0 of the water year (non-leap)

SNOW_METRICS <- list(
  on  = list(band = 1, label = "SD-on",  long = "Snow onset",    unit = "date", range = c(0, 150),
             pal = grDevices::hcl.colors(100, "viridis")),
  off = list(band = 2, label = "SD-off", long = "Snow melt-out", unit = "date", range = c(120, 365),
             pal = grDevices::hcl.colors(100, "plasma")),
  dur = list(band = 3, label = "SD-dur", long = "Snow duration", unit = "days", range = c(0, 365),
             pal = rev(grDevices::hcl.colors(100, "YlGnBu")))
)

# Values -> hex colours, clamped to the metric's range; NA -> transparent
snow_colours <- function(s, v) {
  n   <- length(s$pal)
  idx <- pmin(n, pmax(1, round((v - s$range[1]) / diff(s$range) * (n - 1)) + 1))
  out <- s$pal[idx]
  out[is.na(v)] <- "#00000000"
  out
}

snow_date <- function(d) ifelse(is.na(d), NA, format(SNOW_ORIGIN + round(d), "%b %d"))

# Legend ticks: month starts for dates, quarter-years for durations
snow_ticks <- function(s) {
  if (s$unit == "date") {
    m <- seq(SNOW_ORIGIN, by = "month", length.out = 13)
    d <- as.numeric(m - SNOW_ORIGIN)
    keep <- d >= s$range[1] & d <= s$range[2]
    lab <- format(m[keep], "%b"); d <- d[keep]
  } else {
    d <- c(0, 90, 180, 270, 365); d <- d[d >= s$range[1] & d <= s$range[2]]; lab <- d
  }
  lapply(seq_along(d), function(i) list(p = (d[i] - s$range[1]) / diff(s$range), label = as.character(lab[i])))
}

snow_ready <- file.exists(file.path(SNOW_DIR, "bounds.json"))

snow_control <- function() {
  cfg <- list(
    bounds  = jsonlite::read_json(file.path(SNOW_DIR, "bounds.json")),
    metrics = lapply(setNames(names(SNOW_METRICS), names(SNOW_METRICS)), function(k) {
      s <- SNOW_METRICS[[k]]
      png <- file.path(SNOW_DIR, paste0("snow_", k, ".png"))
      list(url   = sprintf("snow/snow_%s.png?v=%s", k, as.integer(file.mtime(png))),
           title = paste0(s$long, if (s$unit == "date") " (mean date)" else " (mean days)"),
           stops = s$pal[round(seq(1, length(s$pal), length.out = 12))],
           ticks = snow_ticks(s))
    }))
  radio <- function(k, checked = FALSE)
    tags$label(style = "display:block; padding:1px 0; cursor:pointer; font-weight:normal; white-space:nowrap;",
               tags$input(type = "radio", name = "snow-metric", value = k, checked = if (checked) NA,
                          style = "margin-right:5px;"),
               tags$b(SNOW_METRICS[[k]]$label), " ", SNOW_METRICS[[k]]$long)
  tags$div(
    id = "snow-control", `data-cfg` = jsonlite::toJSON(cfg, auto_unbox = TRUE),
    style = "background:white; border-radius:4px; box-shadow:0 1px 5px rgba(0,0,0,0.4); font:12px/1.5 'Helvetica Neue',Arial,sans-serif; padding:6px 12px; max-width:240px;",
    tags$label(
      style = "display:flex; align-items:center; font-weight:bold; font-size:13px; cursor:pointer; margin:0;",
      tags$input(type = "checkbox", id = "snow-master-chk", style = "margin-right:6px;"),
      "Snow",
      tags$span(id = "snow-info", title = "About these layers (click)",
                style = "margin-left:8px; cursor:pointer; color:#0b6a88; font-weight:normal;", HTML("&#9432;")),
      tags$span(id = "snow-caret", title = "Show/hide options",
                style = "margin-left:auto; padding-left:10px; cursor:pointer;", HTML("&#9662;"))),
    tags$div(
      id = "snow-body", style = "display:none; margin-top:4px;",
      radio("on"), radio("off"), radio("dur", checked = TRUE),
      tags$div(id = "snow-legend", style = "margin:6px 0 2px;"),
      tags$label(style = "display:flex; align-items:center; gap:6px; font-weight:normal; margin:4px 0 0;",
                 "Opacity", tags$input(type = "range", id = "snow-opacity", min = 0, max = 100, value = 75,
                                       style = "flex:1; margin:0;")),
      tags$label(style = "display:block; font-weight:normal; cursor:pointer; margin:4px 0 0;",
                 title = "While on, clicking the map shows snow values instead of delineating a watershed",
                 tags$input(type = "checkbox", id = "snow-inspect", checked = NA, style = "margin-right:5px;"),
                 "Click map for values")
    ),
    tags$div(
      id = "snow-help",
      style = "display:none; margin-top:6px; padding-top:6px; border-top:1px solid #ddd; white-space:normal;",
      tags$b("Snow metrics"), " (~500 m grid): mean snow onset (SD-on), melt-out (SD-off) and ",
      "snow-covered duration (SD-dur). Dates count days after Sep 1.", tags$br(),
      "While Snow is on, a map click shows the values at that point (untick ",
      tags$i("Click map for values"), " to delineate watersheds again). The elevation profile ",
      "tool also plots the snow metrics along its line.",
      tags$div(
        style = "margin-top:6px; font-size:11px; color:#444;",
        tags$b("Reference: "),
        "Bevington, A. R., Gleason, H. E., Foord, V. N., Floyd, W. C., and Griesbauer, H. P. (2019). ",
        "Regional influence of ocean–atmosphere teleconnections on the timing and duration of ",
        "MODIS-derived snow cover in British Columbia, Canada. ", tags$i("The Cryosphere"), ", 13, 2693–2712. ",
        tags$a(href = "https://doi.org/10.5194/tc-13-2693-2019", target = "_blank", rel = "noopener",
               "doi:10.5194/tc-13-2693-2019")
      )
    )
  )
}

add_snow_tool <- function(map) {
  if (!snow_ready) return(map)
  map %>%
    addControl(snow_control(), position = "topleft", className = "snow-panel") %>%
    htmlwidgets::onRender("
    function(el, x) {
      var map = this, ctrl = document.getElementById('snow-control');
      if (!ctrl) return;
      var cfg = JSON.parse(ctrl.getAttribute('data-cfg')), b = cfg.bounds;
      var bounds = [[b.south, b.west], [b.north, b.east]];
      var master = document.getElementById('snow-master-chk'), body = document.getElementById('snow-body'),
          caret = document.getElementById('snow-caret'), info = document.getElementById('snow-info'),
          help = document.getElementById('snow-help'), opac = document.getElementById('snow-opacity'),
          inspect = document.getElementById('snow-inspect'), legend = document.getElementById('snow-legend'),
          radios = ctrl.querySelectorAll('input[name=snow-metric]');
      var layer = null, collapsed = false;
      L.DomEvent.disableClickPropagation(ctrl);
      L.DomEvent.disableScrollPropagation(ctrl);
      // Above Sentinel-2 (300) and lidar (350), below vector overlays and points (400)
      if (!map.getPane('snowPane')) { map.createPane('snowPane'); map.getPane('snowPane').style.zIndex = 360; }

      function metric() { var k = 'dur'; radios.forEach(function(r) { if (r.checked) k = r.value; }); return k; }
      function drawLegend(k) {
        var m = cfg.metrics[k];
        legend.innerHTML = '<div style=\"font-weight:bold;\">' + m.title + '</div>' +
          '<div style=\"height:10px; border:1px solid #999; background:linear-gradient(to right,' + m.stops.join(',') + ');\"></div>' +
          '<div style=\"position:relative; height:14px; font-size:10px; color:#444;\">' +
          m.ticks.map(function(t) {
            return '<span style=\"position:absolute; left:' + (t.p * 100) + '%; transform:translateX(' +
                   (t.p < 0.05 ? '0' : t.p > 0.95 ? '-100%' : '-50%') + ');\">' + t.label + '</span>';
          }).join('') + '</div>';
      }
      function sendMode() { Shiny.setInputValue('snow_mode', master.checked && inspect.checked); }
      function show() {
        if (layer) { map.removeLayer(layer); layer = null; }
        caret.innerHTML = collapsed ? '&#9656;' : '&#9662;';
        caret.style.visibility = master.checked ? 'visible' : 'hidden';
        body.style.display = master.checked && !collapsed ? 'block' : 'none';
        if (master.checked) {
          var k = metric();
          layer = L.imageOverlay(cfg.metrics[k].url, bounds,
                                 { pane: 'snowPane', opacity: opac.value / 100, interactive: false }).addTo(map);
          drawLegend(k);
        }
        sendMode();
      }
      master.checked = false;
      show();
      master.addEventListener('change', show);
      radios.forEach(function(r) { r.addEventListener('change', show); });
      opac.addEventListener('input', function() { if (layer) layer.setOpacity(this.value / 100); });
      inspect.addEventListener('change', sendMode);
      caret.addEventListener('click', function(e) {
        e.preventDefault(); e.stopPropagation();        // don't toggle the Snow checkbox
        collapsed = !collapsed; show();
      });
      info.addEventListener('click', function(e) {
        e.preventDefault(); e.stopPropagation();
        help.style.display = (help.style.display === 'none') ? 'block' : 'none';
      });
      // Ignored while the elevation profile tool is drawing its line
      map.on('click', function(e) {
        if (master.checked && inspect.checked && !map._profileDrawing)
          Shiny.setInputValue('snow_click', { lat: e.latlng.lat, lng: e.latlng.lng }, { priority: 'event' });
      });
    }")
}

# ── Reading values ──────────────────────────────────────────────────────────
.snow_cache <- new.env()
snow_rast <- function() {
  if (is.null(.snow_cache$r)) .snow_cache$r <- terra::rast(paste0("/vsicurl/", SNOW_COG))
  .snow_cache$r
}

# Snow bands at points (matrix of lon/lat): data.frame with on, off, dur, obs
snow_values <- function(xy) {
  v <- terra::extract(snow_rast(), xy)
  setNames(as.data.frame(v)[, 1:4], c("on", "off", "dur", "obs"))
}

# GLO-30 elevation at a clicked point (one tile request)
snow_elev <- function(xy) {
  z <- terra::extract(terra::rast(paste0("/vsicurl/", SNOW_DEM)), xy)[[1]]
  z[!is.na(z) & z > 9000] <- NA                                   # UInt16 nodata/fill
  as.numeric(z)
}

# Snow metrics at profile sample points (data.frame with pt, lon, lat); NULL when the
# line is outside the grid or the overlays haven't been built
snow_along <- function(pts) {
  if (!snow_ready || !nrow(pts)) return(NULL)
  v <- tryCatch(snow_values(cbind(pts$lon, pts$lat)),
                error = function(e) { message("Snow along profile failed: ", e$message); NULL })
  if (is.null(v) || all(is.na(v$dur))) return(NULL)
  cbind(pts, v)
}

snow_popup <- function(lat, lng, v, z) {
  row <- function(k, val) sprintf("<tr><td style='padding-right:8px;'><b>%s</b></td><td>%s</td></tr>", k, val)
  body <- if (all(is.na(unlist(v[1, c("on", "off", "dur")])))) "<i>No snow data at this location</i>" else
    paste0("<table>",
           row("SD-on",  sprintf("%s <span style='color:#888'>(day %.0f)</span>", snow_date(v$on),  v$on)),
           row("SD-off", sprintf("%s <span style='color:#888'>(day %.0f)</span>", snow_date(v$off), v$off)),
           row("SD-dur", sprintf("%.0f days", v$dur)),
           row("Observations", sprintf("%.0f", v$obs)),
           row("Elevation", if (is.na(z)) "–" else sprintf("%s m", format(round(z), big.mark = ","))),
           "</table>")
  paste0("<div style='font-size:12px;'><b>Snow metrics</b> <span style='color:#888'>(days after Sep 1)</span><br>",
         body, "<div style='color:#888; margin-top:3px;'>", sprintf("%.5f, %.5f", lat, lng), "</div></div>")
}

# ── Server ──────────────────────────────────────────────────────────────────
snow_server <- function(input, output, session, map_id = "mymap") {
  if (!snow_ready) return(invisible())
  observeEvent(input$snow_click, {
    p <- input$snow_click
    xy <- cbind(p$lng, p$lat)
    res <- tryCatch(list(v = snow_values(xy), z = snow_elev(xy)),
                    error = function(e) { message("Snow click failed: ", e$message); NULL })
    if (is.null(res)) return(showNotification("Couldn't read snow values here.", type = "warning"))
    leafletProxy(map_id) %>% clearPopups() %>%
      addPopups(p$lng, p$lat, snow_popup(p$lat, p$lng, res$v, res$z))
  })
}
