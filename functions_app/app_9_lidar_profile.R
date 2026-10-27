# ── Lidar elevation profile ─────────────────────────────────────────────────
# A map button lets the user draw a line; elevations along it are read from every
# LidarBC DEM survey that covers it (STAC catalogue) plus MRDEM-30 (DTM) and
# Copernicus GLO-30 (DSM) for context, and plotted below the map.

PROFILE_STAC    <- "https://images.a11s.one/search?collections=stac-elevation-bc"
PROFILE_MRDEM   <- "https://canelevation-dem.s3.ca-central-1.amazonaws.com/mrdem-30/mrdem-30-dtm.tif"
PROFILE_GLO30   <- "https://copernicus-dem-30m.s3.amazonaws.com"
PROFILE_MAX_PTS <- 1000   # samples per line (1 m spacing until the line is longer than this)
PROFILE_MAX_KM  <- 20     # longer lines touch too many lidar tiles to read interactively

# DEM tiles crossing the line: lidar surveys from STAC (following its paging), then the
# national/global 30 m COGs. GLO-30 is split into 1-degree tiles named by their SW corner.
profile_tiles <- function(line_ll) {
  bb  <- as.numeric(st_bbox(line_ll))
  url <- paste0(PROFILE_STAC, "&limit=200&bbox=", paste(bb, collapse = ","))
  feats <- list()
  while (!is.null(url)) {
    res   <- jsonlite::fromJSON(url, simplifyVector = FALSE)
    feats <- c(feats, res$features)
    nxt   <- Filter(function(l) identical(l$rel, "next"), res$links)
    url   <- if (length(nxt)) nxt[[1]]$href else NULL
  }
  lidar <- bind_rows(lapply(feats, function(f) {
    if (is.null(f$assets$dem$href)) return(NULL)
    data.frame(source = paste("Lidar", substr(f$properties$datetime %||% "", 1, 4)),
               href = f$assets$dem$href,
               xmin = f$bbox[[1]], ymin = f$bbox[[2]], xmax = f$bbox[[3]], ymax = f$bbox[[4]])
  }))
  # bbox search also returns tiles beside a diagonal line: keep the ones it crosses
  if (nrow(lidar)) {
    boxes <- st_sfc(lapply(seq_len(nrow(lidar)), function(i)
      st_as_sfc(st_bbox(unlist(lidar[i, c("xmin", "ymin", "xmax", "ymax")]), crs = 4326))[[1]]), crs = 4326)
    lidar <- lidar[lengths(st_intersects(boxes, line_ll)) > 0, ]
  }
  g  <- expand.grid(lat = seq(floor(bb[2]), floor(bb[4])), lon = seq(floor(bb[1]), floor(bb[3])))
  nm <- sprintf("Copernicus_DSM_COG_10_%s%02d_00_%s%03d_00_DEM",
                ifelse(g$lat < 0, "S", "N"), abs(g$lat), ifelse(g$lon < 0, "W", "E"), abs(g$lon))
  bind_rows(lidar,
            data.frame(source = "MRDEM-30 (DTM)", href = PROFILE_MRDEM,
                       xmin = -180, ymin = -90, xmax = 180, ymax = 90),
            data.frame(source = "GLO-30 (DSM)", href = sprintf("%s/%s/%s.tif", PROFILE_GLO30, nm, nm),
                       xmin = g$lon, ymin = g$lat, xmax = g$lon + 1, ymax = g$lat + 1))
}

# Elevation profile along a WGS84 linestring: one row per source and sample point
lidar_profile <- function(line_ll, progress = function(i, n) NULL) {
  line_m <- st_transform(line_ll, 3005)
  len    <- as.numeric(st_length(line_m))
  n      <- max(2, min(PROFILE_MAX_PTS, floor(len) + 1))
  spacing <- len / (n - 1)
  pts    <- st_transform(st_cast(st_line_sample(line_m, sample = seq(0, 1, length.out = n)), "POINT"), 4326)
  xy     <- st_coordinates(pts)
  tiles  <- profile_tiles(line_ll)

  z <- bind_rows(lapply(seq_len(nrow(tiles)), function(i) {
    progress(i, nrow(tiles))
    t  <- tiles[i, ]
    in_tile <- which(xy[, 1] >= t$xmin & xy[, 1] <= t$xmax & xy[, 2] >= t$ymin & xy[, 2] <= t$ymax)
    if (!length(in_tile)) return(NULL)
    r <- try(terra::rast(paste0("/vsicurl/", t$href)), silent = TRUE)
    if (inherits(r, "try-error")) return(NULL)
    v <- terra::project(terra::vect(pts[in_tile]), terra::crs(r))
    # Read small windows that follow the line (~256 px of line each): point-by-point reads
    # are a request per point (~20 s on MRDEM), and one window around a diagonal line
    # through a 1 m mapsheet (no overviews) pulls in most of the sheet
    px    <- terra::res(r)[1]
    px_m  <- if (terra::is.lonlat(r)) px * 111000 else px      # GLO-30 is in degrees
    chunk <- (seq_along(in_tile) - 1) %/% max(1, floor(256 * px_m / spacing))
    z <- unlist(lapply(split(seq_along(in_tile), chunk), function(k) {
      win <- terra::intersect(terra::ext(v[k]) + 2 * px, terra::ext(r))
      if (is.null(win)) return(rep(NA_real_, length(k)))
      terra::extract(terra::crop(r, win), v[k], ID = FALSE)[[1]]
    }))
    data.frame(source = t$source, pt = in_tile, z = z)
  }))
  if (!nrow(z)) return(NULL)
  z %>%
    filter(!is.na(z), z > -500) %>%
    distinct(source, pt, .keep_all = TRUE) %>%          # overlapping tiles of one survey
    mutate(dist_m = (pt - 1) * len / (n - 1), lon = xy[pt, 1], lat = xy[pt, 2]) %>%
    arrange(source, pt)
}

# Map button + line drawing, all in the browser: click to add points, click the last point
# again to finish, Esc to cancel. While drawing, input$profile_mode is TRUE so map clicks
# don't open the delineation dialog; the finished line arrives as input$profile_line.
add_profile_tool <- function(map) {
  htmlwidgets::onRender(map, "
    function(el, x) {
      var map = this, drawing = false, pts = [], line = null, guide = null;
      var Btn = L.Control.extend({
        options: { position: 'topleft' },
        onAdd: function() {
          var d = L.DomUtil.create('div', 'leaflet-bar');
          d.innerHTML = '<a href=\"#\" role=\"button\" title=\"Elevation profile: click along a line, click the last point again to finish\" ' +
                        'style=\"font-size:15px;\"><i class=\"fa fa-chart-area\"></i></a>';
          L.DomEvent.disableClickPropagation(d);
          L.DomEvent.on(d.firstChild, 'click', function(e) { L.DomEvent.preventDefault(e); drawing ? stop() : start(); });
          return d;
        }
      });
      var btn  = new Btn().addTo(map).getContainer().firstChild;
      var hint = L.DomUtil.create('div', '', map.getContainer());
      hint.style.cssText = 'position:absolute;bottom:24px;left:50%;transform:translateX(-50%);z-index:1000;display:none;' +
        'background:rgba(255,255,255,0.9);font:12px sans-serif;padding:3px 10px;border-radius:4px;pointer-events:none;';

      function clear() {
        if (line)  { map.removeLayer(line);  line = null; }
        if (guide) { map.removeLayer(guide); guide = null; }
        pts = [];
      }
      function setMode(on, delay) {
        drawing = on;
        btn.style.background = on ? '#cde4f0' : '';
        map.getContainer().style.cursor = on ? 'crosshair' : '';
        on ? map.doubleClickZoom.disable() : map.doubleClickZoom.enable();
        hint.style.display = on ? 'block' : 'none';
        hint.textContent = 'Click to add points \\u00b7 click the last point to finish \\u00b7 Esc to cancel';
        // Turning off: wait until the map click that finished the line has reached the server
        setTimeout(function() { Shiny.setInputValue('profile_mode', drawing); }, delay || 0);
      }
      function start() { clear(); setMode(true); }
      function stop()  { clear(); setMode(false); }
      function finish() {
        setMode(false, 500);
        if (guide) { map.removeLayer(guide); guide = null; }
        line.setStyle({ dashArray: null });
        Shiny.setInputValue('profile_line', { lng: pts.map(function(p) { return p.lng; }),
                                              lat: pts.map(function(p) { return p.lat; }) }, { priority: 'event' });
      }

      map.on('click', function(e) {
        if (!drawing) return;
        var last = pts[pts.length - 1];
        if (last && map.latLngToContainerPoint(last).distanceTo(e.containerPoint) < 10) {
          if (pts.length > 1) finish();
          return;
        }
        pts.push(e.latlng);
        if (!line) line = L.polyline(pts, { color: '#d7301f', weight: 3, dashArray: '6 4' }).addTo(map);
        else line.setLatLngs(pts);
      });
      map.on('mousemove', function(e) {
        if (!drawing || !pts.length) return;
        var seg = [pts[pts.length - 1], e.latlng];
        if (!guide) guide = L.polyline(seg, { color: '#d7301f', weight: 2, opacity: 0.6, dashArray: '2 4', interactive: false }).addTo(map);
        else guide.setLatLngs(seg);
      });
      document.addEventListener('keydown', function(e) { if (drawing && e.key === 'Escape') stop(); });
      // Shiny throws if the handler doesn't take exactly one argument or is registered twice
      // (the map can render more than once); either aborts the render hooks after this one
      // (landslides), so register once and point it at the current map's clear()
      window._profileClear = clear;
      if (!window._profileHandler) {
        window._profileHandler = true;
        Shiny.addCustomMessageHandler('profile_clear', function(msg) { if (window._profileClear) window._profileClear(); });
      }
    }")
}

# Panel below the map; empty until a line has been drawn
profile_panel_ui <- uiOutput("profile_panel")

lidar_profile_server <- function(input, output, session, map_id = "mymap") {
  prof <- reactiveVal(NULL)

  observeEvent(input$profile_line, {
    xy <- cbind(unlist(input$profile_line$lng), unlist(input$profile_line$lat))
    ln <- st_sfc(st_linestring(xy), crs = 4326)
    km <- as.numeric(st_length(st_transform(ln, 3005))) / 1000
    if (km > PROFILE_MAX_KM) {
      showNotification(sprintf("That line is %.0f km; profiles are limited to %d km.", km, PROFILE_MAX_KM),
                       type = "warning")
      session$sendCustomMessage("profile_clear", TRUE)
      return()
    }
    z <- withProgress(message = "Reading elevations...", value = 0,
                      tryCatch(lidar_profile(ln, function(i, n) setProgress(i / n, detail = sprintf("tile %d of %d", i, n))),
                               error = function(e) { message("Profile failed: ", e$message); NULL }))
    if (is.null(z)) {
      showNotification("Couldn't read elevations along this line.", type = "warning")
      session$sendCustomMessage("profile_clear", TRUE)
    }
    prof(z)
  })

  observeEvent(input$profile_close, {
    prof(NULL)
    session$sendCustomMessage("profile_clear", TRUE)
    leafletProxy(map_id) %>% clearGroup("profile_hover")
  })

  output$profile_panel <- renderUI({
    z <- prof(); req(z)
    n_lidar <- length(unique(grep("^Lidar", z$source, value = TRUE)))
    tags$div(
      style = "border:1px solid #ddd; border-radius:4px; padding:6px 10px; margin-bottom:6px;",
      tags$div(
        style = "display:flex; align-items:center; gap:14px; font-size:12px;",
        tags$b(style = "font-size:14px;", "Elevation profile"),
        sprintf("%s m · %s", format(round(max(z$dist_m)), big.mark = ","),
                if (n_lidar) paste(n_lidar, if (n_lidar == 1) "lidar survey" else "lidar surveys")
                else "no lidar coverage, 30 m DEMs only"),
        downloadLink("profile_csv", tagList(icon("download"), "CSV")),
        actionLink("profile_close", icon("xmark"), title = "Close profile", style = "margin-left:auto; color:#666;")
      ),
      plotlyOutput("profile_plot", height = "300px")
    )
  })

  output$profile_plot <- renderPlotly({
    z <- prof(); req(z)
    srcs  <- unique(z$source)
    lidar <- sort(grep("^Lidar", srcs, value = TRUE))
    cols  <- setNames(c(viridisLite::viridis(length(lidar), end = 0.9), "#888888", "#bbbbbb"),
                      c(lidar, "MRDEM-30 (DTM)", "GLO-30 (DSM)"))
    p <- plot_ly(source = "profile")
    for (s in c(lidar, intersect(c("MRDEM-30 (DTM)", "GLO-30 (DSM)"), srcs))) {
      d <- z[z$source == s, ]
      p <- add_trace(p, data = d, x = ~dist_m, y = ~z, customdata = ~pt, name = s,
                     type = "scatter", mode = "lines", hovertemplate = "%{y:.1f} m",
                     line = list(color = cols[[s]], width = if (s %in% lidar) 1.5 else 1,
                                 dash = if (s %in% lidar) "solid" else "dot"))
    }
    p %>%
      layout(xaxis = list(title = "Distance (m)"), yaxis = list(title = "Elevation (m)"),
             hovermode = "x unified", margin = list(t = 10),
             legend = list(orientation = "h", y = -0.25)) %>%
      config(displaylogo = FALSE) %>%
      event_register("plotly_hover") %>%
      event_register("plotly_unhover")
  })

  # Hover along the profile → marker at that spot on the map. suppressWarnings: plotly
  # warns "not registered" while the plot doesn't exist yet (before the first line)
  observeEvent(suppressWarnings(event_data("plotly_hover", source = "profile")), {
    h <- event_data("plotly_hover", source = "profile"); z <- prof(); req(h, z)
    row <- z[z$pt == h$customdata[1], ][1, ]
    leafletProxy(map_id) %>% clearGroup("profile_hover") %>%
      addCircleMarkers(row$lon, row$lat, radius = 6, color = "white", weight = 2,
                       fillColor = "#d7301f", fillOpacity = 1, group = "profile_hover")
  })
  observeEvent(suppressWarnings(event_data("plotly_unhover", source = "profile")), {
    leafletProxy(map_id) %>% clearGroup("profile_hover")
  })

  output$profile_csv <- downloadHandler(
    filename = function() "elevation_profile.csv",
    content  = function(file) {
      z <- prof()
      write.csv(z[, c("source", "dist_m", "lon", "lat", "z")], file, row.names = FALSE)
    }
  )
}
