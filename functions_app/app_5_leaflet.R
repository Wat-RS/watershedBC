res <- 300

# ── coast_ifw is NOT loaded here — see lazy-load observer in server ────────────
# Previously this did:
#   coast_ifw <- st_read(conn, query = "SELECT * FROM dcwbm_bsn_typed_ifw_1991_2020_100m")
# which pulled ALL IFW polygons at startup (~thousands of complex coastal polygons).
# Now deferred to first toggle of the "Coast - IFW" overlay group.

wsc_pp_ac <- wsc_pp %>% filter(status == "active")       %>% mutate(id = stationnum)
wsc_pp_dc <- wsc_pp %>% filter(status == "discontinued") %>% mutate(id = stationnum)

# initial_map <- leaflet() %>%
#   addProviderTiles(providers$Esri.WorldImagery, group = "WorldImagery") %>%
#   addProviderTiles(providers$Esri.WorldTopoMap, group = "WorldTopoMap") %>%
#   addWMSTiles("http://maps.gov.bc.ca/arcserver/rest/services/province/roads_wm/MapServer/tile/{z}/{y}/{x}",
#               layers = "GRB_BSK",
#               options = WMSTileOptions(format = "image/png", transparent = TRUE),
#               group = "BC Basemap") %>%
#   addMeasure(primaryLengthUnit = "kilometers",
#              secondaryLengthUnit = "meters",
#              primaryAreaUnit = "hectares",
#              secondaryAreaUnit = "sqmeters",
#              position = "topleft") %>%
#   addWMSTiles("https://storage.googleapis.com/earthenginepartners-hansen/tiles/gfc_v1.4/first_543/{z}/{x}/{y}.jpg",
#               layers = "GRB_BSK", options = WMSTileOptions(format = "ima ge/png", transparent = TRUE), group = "Landsat 2000") %>%
#   addWMSTiles("https://storage.googleapis.com/earthenginepartners-hansen/tiles/gfc_v1.9/last_543/{z}/{x}/{y}.jpg",
#               layers = "GRB_BSK", options = WMSTileOptions(format = "image/png", transparent = TRUE), group = "Landsat 2021") %>%
#   addCircleMarkers(data = wsc_pp_ac, lng = wsc_pp_ac$lon, lat = wsc_pp_ac$lat,
#                    color = "steelblue", radius = 3, group = "WSC Active",
#                    label = paste0(wsc_pp_ac$name, " - ", wsc_pp_ac$stationnum, " [active]")) %>%
#   addCircleMarkers(data = wsc_pp_dc, lng = wsc_pp_dc$lon, lat = wsc_pp_dc$lat,
#                    color = "grey", radius = 3, group = "WSC Discontinued",
#                    label = paste0(wsc_pp_dc$name, " - ", wsc_pp_dc$stationnum, " [discontinued]")) %>%
#   addLayersControl(baseGroups    = c("BC Basemap", "WorldImagery", "WorldTopoMap", "Landsat 2000", "Landsat 2021"),
#                    overlayGroups = c("WSC Active", "WSC Discontinued"),
#                    options       = layersControlOptions(collapsed = FALSE)) %>%
#   hideGroup(c("WSC Active", "WSC Discontinued")) %>%
#   addMouseCoordinates()

library(leaflet)
library(xml2)
library(htmltools)
library(htmlwidgets)

# Lightweight stand-in for leafem::addMouseCoordinates() — leafem pulls in raster/sp (~2s startup)
addMouseCoordinates <- function(map) {
  htmlwidgets::onRender(map, "
    function(el, x) {
      var map = this;
      var box = L.DomUtil.create('div');
      box.style.cssText = 'position:absolute;top:0;left:50%;transform:translateX(-50%);z-index:1000;' +
        'background:rgba(255,255,255,0.8);font:11px monospace;padding:1px 6px;pointer-events:none;';
      map.getContainer().appendChild(box);
      map.on('mousemove', function(e) {
        box.textContent = 'lon: ' + e.latlng.lng.toFixed(5) + ' | lat: ' + e.latlng.lat.toFixed(5) +
          ' | zoom: ' + map.getZoom();
      });
      map.on('mouseout', function() { box.textContent = ''; });
    }")
}

# ── WMTS Config ─────────────────────────────────────────────────────────────
GEOSERVER_BASE <- "https://geoserver.watershedbc.ca/geoserver"
WMTS_CAPS_URL  <- paste0(GEOSERVER_BASE, "/gwc/service/wmts?request=GetCapabilities")

# ── Parse WMTS GetCapabilities ──────────────────────────────────────────────
parse_wmts <- function(url) {
  tryCatch({
    doc    <- read_xml(url)
    ns     <- xml_ns(doc)
    layers <- xml_find_all(doc, ".//d1:Layer", ns)
    lapply(layers, function(lyr) {
      id    <- xml_text(xml_find_first(lyr, ".//ows:Identifier", ns))
      title <- xml_text(xml_find_first(lyr, ".//ows:Title", ns))
      tms   <- xml_text(xml_find_all(lyr, ".//d1:TileMatrixSetLink/d1:TileMatrixSet", ns))
      fmts  <- xml_text(xml_find_all(lyr, ".//d1:Format", ns))
      list(id = id, title = title, tms = tms, formats = fmts)
    })
  }, error = function(e) { message("WMTS parse failed: ", e$message); list() })
}

build_tile_url <- function(layer_id, tms = "EPSG:900913", fmt = "image/png") {
  paste0(
    GEOSERVER_BASE, "/gwc/service/wmts",
    "?Service=WMTS&Request=GetTile&Version=1.0.0",
    "&Layer=", layer_id, "&Style=",
    "&TileMatrixSet=", tms, "&TileMatrix=", tms, ":{z}",
    "&TileCol={x}&TileRow={y}&Format=", fmt
  )
}

pick_tms <- function(ids) {
  for (p in c("EPSG:900913", "EPSG:3857", "GoogleMapsCompatible"))
    if (p %in% ids) return(p)
  ids[1]
}

pick_fmt <- function(fmts) {
  if ("image/png"  %in% fmts) return("image/png")
  if ("image/jpeg" %in% fmts) return("image/jpeg")
  fmts[1]
}

# ── Load WMTS layers at startup ─────────────────────────────────────────────
wmts_layers <- parse_wmts(WMTS_CAPS_URL)

# Sort by year ascending (extract year from underscore-delimited titles)
wmts_years <- vapply(wmts_layers, function(lyr) {
  parts <- unlist(strsplit(lyr$title, "_"))
  yr <- grep("^(19|20)\\d{2}$", parts, value = TRUE)
  if (length(yr)) as.integer(yr[1]) else 0L}, integer(1))
wmts_layers <- wmts_layers[order(wmts_years)]

# Sentinel-2 annual mosaics (BC_<year>_S2_20m_mosaic) drive the satellite timelapse;
# everything else on the GeoServer is a lidar DTM
is_s2       <- vapply(wmts_layers, function(lyr) grepl("_S2_", lyr$title), logical(1))
s2_layers   <- wmts_layers[is_s2]
wmts_layers <- wmts_layers[!is_s2]
s2_years    <- as.integer(sub(".*_((19|20)\\d{2})_.*", "\\1", vapply(s2_layers, `[[`, "", "title")))
s2_urls     <- vapply(s2_layers, function(lyr) build_tile_url(lyr$id, pick_tms(lyr$tms), pick_fmt(lyr$formats)), "")

# ── CDN scripts — add to your UI via tags$head(map_head_tags) ──────────────
map_head_tags <- tagList(
  tags$script(src = "https://cdn.jsdelivr.net/npm/html2canvas@1.4.1/dist/html2canvas.min.js"),
  tags$script(src = "https://cdn.jsdelivr.net/npm/jspdf@2.5.1/dist/jspdf.umd.min.js")
)

# ── Build LiDAR control HTML (no script tag — JS goes in onRender) ─────────
lidar_checkboxes <- lapply(wmts_layers, function(lyr) {
  url <- build_tile_url(lyr$id, pick_tms(lyr$tms), pick_fmt(lyr$formats))
  # Reformat: dem_hs_cog_2025_5m → DTM 2025 5m
  parts <- unlist(strsplit(lyr$title, "_"))
  yr  <- grep("^(19|20)\\d{2}$", parts, value = TRUE)
  res <- grep("^\\d+m$", parts, value = TRUE)
  is_mosaic <- length(yr) == 0
  label <- paste("DTM", paste(c(if (is_mosaic) "all years" else yr, res), collapse = " "))
  tags$label(
    style = "display:block; padding:2px 0; cursor:pointer; font-weight:normal; white-space:nowrap;",
    tags$input(
      type = "checkbox",
      class = "lidar-layer-chk",
      `data-url` = url,
      `data-mosaic` = if (is_mosaic) "true" else "false",
      style = "margin-right:5px;"
    ),
    label
  )
})

lidar_control <- tags$div(
  id = "lidar-control",
  style = "background:white; border-radius:4px; box-shadow:0 1px 5px rgba(0,0,0,0.4); font:12px/1.5 'Helvetica Neue',Arial,sans-serif; padding:6px 12px;",
  tags$label(
    style = "display:flex; align-items:center; font-weight:bold; font-size:13px; cursor:pointer; margin:0;",
    tags$input(type = "checkbox", id = "lidar-master-chk", style = "margin-right:6px;"),
    "Lidar",
    tags$span(id = "lidar-info", title = "Use these lidar layers in QGIS or ArcGIS Pro (click)",
              style = "margin-left:8px; cursor:pointer; color:#0b6a88; font-weight:normal;", HTML("&#9432;")),
    tags$span(id = "lidar-caret", title = "Show/hide years",
              style = "margin-left:auto; padding-left:10px; cursor:pointer;", HTML("&#9662;")),
    ),
  tags$div(
    id = "lidar-years",
    style = "display:none; margin-top:4px;",
    lidar_checkboxes
  ),
  tags$div(
    id = "lidar-help",
    style = "display:none; margin-top:6px; padding-top:6px; border-top:1px solid #ddd; max-width:260px; white-space:normal;",
    tags$b("Use these layers in your own GIS"), tags$br(),
    "Add a WMTS connection with this address:",
    tags$input(type = "text", readonly = NA, onclick = "this.select();",
               value = paste0(GEOSERVER_BASE, "/gwc/service/wmts?request=GetCapabilities"),
               style = "width:100%; font-size:11px; margin:4px 0;"),
    tags$b("QGIS:"), " Layer > Add Layer > Add WMS/WMTS Layer > New, paste the address, Connect.", tags$br(),
    tags$b("ArcGIS Pro:"), " Insert > Connections > Server > New WMTS Server, paste the address."
  )
  
)

# ── Toolbar to place below leafletOutput("mymap") in your UI ───────────────
# Satellite timelapse controls: hidden until "Satellite timelapse" is checked. Plain HTML
# (not Shiny inputs) — playback runs entirely in the browser, see onRender below.
s2_panel <- tags$div(
  id = "s2-panel",
  `data-years` = jsonlite::toJSON(s2_years), `data-urls` = jsonlite::toJSON(s2_urls),
  style = "display:none; align-items:center; flex-wrap:wrap; gap:6px 14px; padding:6px 10px; margin-bottom:6px;
           background:#f7f7f7; border:1px solid #ddd; border-radius:4px; font-size:12px;",
  tags$button(id = "s2-play", class = "btn btn-default btn-sm", title = "Play / pause", icon("play")),
  tags$input(id = "s2-year", type = "range", min = 0, max = length(s2_years) - 1, step = 1,
             value = length(s2_years) - 1, style = "flex:1; min-width:160px; margin:0;"),
  tags$b(id = "s2-label", style = "font-size:15px; min-width:38px;", max(s2_years)),
  tags$label(style = "font-weight:normal; margin:0;", "Speed ",
             tags$select(id = "s2-fps", lapply(1:6, function(f) tags$option(value = f, paste(f, "fps"))))),
  tags$label(style = "font-weight:normal; margin:0;",
             tags$input(type = "checkbox", id = "s2-loop", checked = NA, style = "margin-right:4px;"), "Loop"),
  tags$button(id = "s2-export", class = "btn btn-default btn-sm", icon("film"), "Export video")
)

map_toolbar <- tagList(
  tags$div(
    style = "display:flex; align-items:center; gap:8px; padding:6px 0;",
    tags$button(id = "map_fullscreen", class = "btn btn-default",
                style = "font-size:12px; padding:4px 12px;",
                icon("expand"), "Full Screen"),
    tags$button(id = "map_pdf", class = "btn btn-default",
                style = "font-size:12px; padding:4px 12px;",
                icon("file-pdf"), "Export Map PDF"),
    if (length(s2_years))
      tags$label(style = "font-weight:normal; font-size:12px; margin:0 0 0 6px; cursor:pointer;",
                 tags$input(type = "checkbox", id = "s2-toggle", style = "margin-right:5px;"),
                 sprintf("Satellite timelapse (Sentinel-2, %d–%d)", min(s2_years), max(s2_years)))
  ),
  if (length(s2_years)) s2_panel,
  tags$style(".s2-layer { transition: opacity 0.2s; } .s2-capturing .s2-layer { transition: none; }")
)

# ── Build the map ───────────────────────────────────────────────────────────
initial_map <- leaflet() %>%
  addProviderTiles(providers$Esri.WorldImagery, group = "WorldImagery") %>%
  addProviderTiles(providers$Esri.WorldTopoMap, group = "WorldTopoMap") %>%
  addTiles(
    urlTemplate = "http://maps.gov.bc.ca/arcserver/rest/services/province/roads_wm/MapServer/tile/{z}/{y}/{x}",
    group = "BC Basemap"
  ) %>%
  addMeasure(
    primaryLengthUnit   = "kilometers",
    secondaryLengthUnit = "meters",
    primaryAreaUnit     = "hectares",
    secondaryAreaUnit   = "sqmeters",
    position            = "topleft"
  ) %>%
  addCircleMarkers(
    data = wsc_pp_ac, lng = wsc_pp_ac$lon, lat = wsc_pp_ac$lat,
    color = "steelblue", radius = 3, group = "WSC Active",
    label = paste0(wsc_pp_ac$name, " - ", wsc_pp_ac$stationnum, " [active]")
  ) %>%
  addCircleMarkers(
    data = wsc_pp_dc, lng = wsc_pp_dc$lon, lat = wsc_pp_dc$lat,
    color = "grey", radius = 3, group = "WSC Discontinued",
    label = paste0(wsc_pp_dc$name, " - ", wsc_pp_dc$stationnum, " [discontinued]")
  ) %>%
  addMouseCoordinates() %>%
  addScaleBar(position = "bottomleft", options = scaleBarOptions(imperial = FALSE)) %>%
  addLayersControl(
    baseGroups    = c("BC Basemap", "WorldImagery", "WorldTopoMap"),
    overlayGroups = c("WSC Active", "WSC Discontinued"),
    options       = layersControlOptions(collapsed = TRUE)   # forced open on desktop via wrsl.css
  ) %>%
  hideGroup(c("WSC Active", "WSC Discontinued")) %>%
  addControl(lidar_control, position = "topleft", className = "lidar-panel") %>%
  # ── onRender: JS runs AFTER the map is fully ready, with direct map access ──
  onRender("
    function(el, x) {
      var map = this;
var _lidarLayers = {};

      // Dedicated pane: above base tiles (200), below overlay/points (400)
      if (!map.getPane('lidarPane')) {
        map.createPane('lidarPane');
        map.getPane('lidarPane').style.zIndex = 350;
      }

      var ctrl = document.getElementById('lidar-control');
      if (ctrl) {
        L.DomEvent.disableClickPropagation(ctrl);
        L.DomEvent.disableScrollPropagation(ctrl);
      }

      function addLidar(cb) {
        var url = cb.getAttribute('data-url');
        if (_lidarLayers[url]) return;
        var tl = L.tileLayer(url, { maxZoom: 18, tileSize: 256, pane: 'lidarPane' });
        tl.addTo(map);
        _lidarLayers[url] = tl;
      }
      function removeLidar(cb) {
        var url = cb.getAttribute('data-url');
        if (_lidarLayers[url]) { map.removeLayer(_lidarLayers[url]); delete _lidarLayers[url]; }
      }

      var cbs = document.querySelectorAll('.lidar-layer-chk');
      cbs.forEach(function(cb) {
        // cb.checked = true;                       // all years on by default
        cb.checked = cb.getAttribute('data-mosaic') === 'true';   // only mosaic on by default
        cb.addEventListener('change', function() {
          this.checked ? addLidar(this) : removeLidar(this);
        });
      });

      // Master 'Lidar' toggle: off by default, controls the whole panel
      var master = document.getElementById('lidar-master-chk');
      var yearsBox = document.getElementById('lidar-years');
      var lidarCaret = document.getElementById('lidar-caret');
      var yearsCollapsed = true;                       // year list hidden until the caret is clicked
      function applyMaster() {
        lidarCaret.innerHTML = yearsCollapsed ? '&#9656;' : '&#9662;';
        lidarCaret.style.visibility = master.checked ? 'visible' : 'hidden';
        if (master.checked) {
          yearsBox.style.display = yearsCollapsed ? 'none' : 'block';
          cbs.forEach(function(cb){ if (cb.checked) addLidar(cb); });
        } else {
          yearsBox.style.display = 'none';
          cbs.forEach(function(cb){ removeLidar(cb); });   // hide layers, keep checkbox state
        }
      }
      master.checked = false;
      applyMaster();
      master.addEventListener('change', applyMaster);
      lidarCaret.addEventListener('click', function(e) {
        e.preventDefault(); e.stopPropagation();        // don't toggle the Lidar checkbox
        yearsCollapsed = !yearsCollapsed;
        applyMaster();
      });
      var lidarInfo = document.getElementById('lidar-info');
      var lidarHelp = document.getElementById('lidar-help');
      lidarInfo.addEventListener('click', function(e) {
        e.preventDefault(); e.stopPropagation();        // don't toggle the Lidar checkbox
        lidarHelp.style.display = (lidarHelp.style.display === 'none') ? 'block' : 'none';
      });
      // ── Full Screen ──────────────────────────────────────────────────────
      var fsBtn = document.getElementById('map_fullscreen');
      if (fsBtn) {
        fsBtn.addEventListener('click', function(e) {
          e.preventDefault();
          var target = el.querySelector('.leaflet-container') || el;
          if (!document.fullscreenElement) {
            target.requestFullscreen().catch(function(err) {
              alert('Fullscreen failed: ' + err.message);
            });
          } else {
            document.exitFullscreen();
          }
        });
      }
      document.addEventListener('fullscreenchange', function() {
        setTimeout(function() { map.invalidateSize(); }, 200);
      });

      // ── Export PDF ────────────────────────────────────────────────────────
      var pdfBtn = document.getElementById('map_pdf');
      if (pdfBtn) {
        pdfBtn.addEventListener('click', function() {
          pdfBtn.disabled = true;
          pdfBtn.textContent = 'Exporting...';
          var mapEl = el.querySelector('.leaflet-container') || el;
          html2canvas(mapEl, { useCORS: true, allowTaint: true, logging: false })
            .then(function(canvas) {
              var imgData = canvas.toDataURL('image/jpeg', 0.95);
              var w = canvas.width, h = canvas.height;
              var orient = w > h ? 'landscape' : 'portrait';
              var pdf = new jspdf.jsPDF({ orientation: orient, unit: 'px', format: [w, h] });
              pdf.addImage(imgData, 'JPEG', 0, 0, w, h);
              var b = map.getBounds();
              var stamp = 'Bounds: ' + b.getSouth().toFixed(4) + ', ' + b.getWest().toFixed(4) +
                          ' to ' + b.getNorth().toFixed(4) + ', ' + b.getEast().toFixed(4);
              pdf.setFontSize(10);
              pdf.setTextColor(180);
              pdf.text(stamp, 10, h - 10);
              pdf.save('map_view.pdf');
            })
            .catch(function(err) {
              alert('PDF export failed — this may be due to cross-origin tile restrictions.');
            })
            .finally(function() {
              pdfBtn.disabled = false;
              pdfBtn.innerHTML = '<i class=\"fa fa-file-pdf\"></i> Export PDF';
            });
        });
      }

      // ── Satellite timelapse ───────────────────────────────────────────────
      // All years are added at once (opacity 0) so their tiles preload; showing a year
      // fades it in on top, then hides the rest underneath.
      var s2chk = document.getElementById('s2-toggle'), s2panel = document.getElementById('s2-panel');
      if (s2chk && s2panel) {
        // Above base tiles (200), below lidar (350)
        if (!map.getPane('s2Pane')) {
          map.createPane('s2Pane');
          map.getPane('s2Pane').style.zIndex = 300;
        }
        var s2years  = JSON.parse(s2panel.getAttribute('data-years')),
            s2urls   = JSON.parse(s2panel.getAttribute('data-urls')),
            s2slider = document.getElementById('s2-year'),
            s2label  = document.getElementById('s2-label'),
            s2play   = document.getElementById('s2-play'),
            s2fps    = document.getElementById('s2-fps'),
            s2loop   = document.getElementById('s2-loop'),
            s2export = document.getElementById('s2-export'),
            s2tiles  = [], s2cur = s2years.length - 1, s2top = 0, s2timer = null, s2fade = null;

        function s2show(i) {
          s2cur = i; s2slider.value = i; s2label.textContent = s2years[i];
          s2tiles[i].setZIndex(++s2top).setOpacity(1);
          clearTimeout(s2fade);
          s2fade = setTimeout(function() {
            s2tiles.forEach(function(t, j) { if (j !== s2cur) t.setOpacity(0); });
          }, Math.min(250, 1000 / s2fps.value));
        }
        function s2step() {
          if (s2cur + 1 < s2years.length) s2show(s2cur + 1);
          else if (s2loop.checked) s2show(0);
          else s2stop();
        }
        function s2stop() {
          clearInterval(s2timer); s2timer = null;
          s2play.innerHTML = '<i class=\"fa fa-play\"></i>';
        }
        function s2start() {
          if (s2cur === s2years.length - 1) s2show(0);   // at the end: replay from the start
          clearInterval(s2timer);
          s2timer = setInterval(s2step, 1000 / s2fps.value);
          s2play.innerHTML = '<i class=\"fa fa-pause\"></i>';
        }

        s2chk.addEventListener('change', function() {
          s2panel.style.display = this.checked ? 'flex' : 'none';
          if (this.checked) {
            if (!s2tiles.length) s2tiles = s2urls.map(function(u) {
              return L.tileLayer(u, { pane: 's2Pane', className: 's2-layer', opacity: 0, maxZoom: 18 });
            });
            s2tiles.forEach(function(t) { t.addTo(map); });
            s2show(s2cur);
          } else {
            s2stop();
            s2tiles.forEach(function(t) { map.removeLayer(t); });
          }
        });
        s2play.addEventListener('click', function() { s2timer ? s2stop() : s2start(); });
        s2slider.addEventListener('input', function() { s2stop(); s2show(+this.value); });
        s2fps.addEventListener('change', function() {
          if (s2timer) { clearInterval(s2timer); s2timer = setInterval(s2step, 1000 / s2fps.value); }
        });

        // Export: screenshot each year once its tiles have loaded, then replay the frames
        // into a canvas recorded at the chosen speed (MP4 where supported, otherwise WebM)
        function s2loaded(t) {
          return new Promise(function(resolve) {
            if (!t.isLoading || !t.isLoading()) return resolve();
            t.once('load', resolve);
            setTimeout(resolve, 8000);                    // don't hang on a missing tile
          });
        }
        s2export.addEventListener('click', function() {
          var mime = ['video/mp4', 'video/webm;codecs=vp9', 'video/webm'].filter(function(m) {
            return window.MediaRecorder && MediaRecorder.isTypeSupported(m);
          })[0];
          if (!mime) { alert('Video export is not supported in this browser.'); return; }
          s2stop();
          var mapEl = el.querySelector('.leaflet-container') || el, frames = [], start = s2cur,
              fps = +s2fps.value, btnHtml = s2export.innerHTML,
              ctr = map.getCenter(), zm = map.getZoom();
          s2export.disabled = s2play.disabled = s2slider.disabled = true;
          mapEl.classList.add('s2-capturing');
          function done() {
            mapEl.classList.remove('s2-capturing');
            s2show(start);
            s2export.disabled = s2play.disabled = s2slider.disabled = false;
            s2export.innerHTML = btnHtml;
          }
          function grab(i) {
            if (i === s2years.length) return encode();
            s2export.textContent = 'Capturing ' + s2years[i] + '...';
            s2show(i);
            clearTimeout(s2fade);
            s2tiles.forEach(function(t, j) { t.setOpacity(j === i ? 1 : 0); });
            s2loaded(s2tiles[i])
              .then(function() { return html2canvas(mapEl, { useCORS: true, logging: false }); })
              .then(function(c) { frames.push(c); grab(i + 1); })
              .catch(function() { alert('Video export failed while capturing the map.'); done(); });
          }
          function encode() {
            s2export.textContent = 'Encoding...';
            var out = document.createElement('canvas'), ctx = out.getContext('2d'), chunks = [];
            out.width = frames[0].width & ~1; out.height = frames[0].height & ~1;   // H.264 needs even sizes
            function draw(k) {
              ctx.drawImage(frames[k], 0, 0);
              var fs = Math.round(out.height / 14);
              ctx.font = 'bold ' + fs + 'px sans-serif';
              ctx.lineWidth = fs / 8; ctx.strokeStyle = 'rgba(0,0,0,0.7)'; ctx.fillStyle = '#fff';
              ctx.strokeText(s2years[k], fs / 2, out.height - fs / 2);
              ctx.fillText(s2years[k], fs / 2, out.height - fs / 2);
            }
            // Record at 30 fps, redrawing each year for its whole duration, so the encoder gets
            // repeated frames to sharpen the still image; the default bitrate is far too low
            var FR = 30, hold = Math.max(1, Math.round(FR / fps));
            var rec = new MediaRecorder(out.captureStream(FR), { mimeType: mime, videoBitsPerSecond: 25e6 });
            rec.ondataavailable = function(e) { if (e.data.size) chunks.push(e.data); };
            rec.onstop = function() {
              var a = document.createElement('a');
              a.href = URL.createObjectURL(new Blob(chunks, { type: mime }));
              a.download = 'satellite_timelapse_' + s2years[0] + '-' + s2years[s2years.length - 1] +
                           '_lat' + ctr.lat.toFixed(4) + '_lon' + ctr.lng.toFixed(4) + '_z' + zm +
                           (mime.indexOf('mp4') >= 0 ? '.mp4' : '.webm');
              a.click();
              setTimeout(function() { URL.revokeObjectURL(a.href); }, 1000);
              done();
            };
            var n = 0;
            draw(0); rec.start();
            var tm = setInterval(function() {
              var k = Math.floor(++n / hold);
              if (k === frames.length) { clearInterval(tm); rec.stop(); } else draw(k);
            }, 1000 / FR);
          }
          grab(0);
        });
      }
    }
  ")