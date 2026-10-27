# Builds mosaic_asc_colour_calibration.csv: tile colour -> deformation value (m/yr) for the
# NRCan land-deformation WMS (layer mosaic-asc). The service only renders a fixed colour ramp
# (no SLD, no WCS, and the published legend doesn't match the tiles), so the map recovers
# values from colours with this lookup in order to mask small values in the browser.
# Method: render one large GetMap image over BC, pick one pixel for every distinct colour
# (sampled, plus all colours in both ramp tails), and read its true value with GetFeatureInfo.
# Rerun if NRCan changes the layer's styling.

library(png); library(curl); library(jsonlite)

WMS  <- "https://datacube.services.geo.ca/wrapper/ogc/land-deformation?service=WMS&version=1.3.0&layers=mosaic-asc&styles=&crs=EPSG:3857"
BBOX <- "-14500000,6200000,-12700000,8000000"; N <- 1024

tmp <- tempfile(fileext = ".png")
curl_download(sprintf("%s&request=GetMap&format=image/png&transparent=true&width=%d&height=%d&bbox=%s", WMS, N, N, BBOX), tmp)
img <- readPNG(tmp)
rgb <- round(img[,,1:3] * 255)
idx <- which(img[,,4] > 0, arr.ind = TRUE)
key <- paste(rgb[,,1][idx], rgb[,,2][idx], rgb[,,3][idx], sep = ",")

set.seed(1)
one  <- unlist(tapply(seq_along(key), key, function(i) i[sample.int(length(i), 1)]))
m    <- do.call(rbind, lapply(strsplit(key[one], ","), as.numeric))
tail <- one[(m[,1] == 0 & m[,2] < 110) | (m[,1] == 255 & m[,2] < 100)]   # sparse ramp ends
pick <- unique(c(sample(one, min(450, length(one))), tail))

vals <- rep(NA_real_, length(pick)); pool <- new_pool(host_con = 8)
for (k in seq_along(pick)) local({
  kk <- k
  url <- sprintf("%s&request=GetFeatureInfo&query_layers=mosaic-asc&info_format=application/json&format=image/png&width=%d&height=%d&bbox=%s&i=%d&j=%d",
                 WMS, N, N, BBOX, idx[pick[kk], 2] - 1, idx[pick[kk], 1] - 1)
  curl_fetch_multi(url, pool = pool, done = function(x) {
    v <- tryCatch(fromJSON(rawToChar(x$content))$features$properties[[1]], error = function(e) NULL)
    if (length(v)) vals[kk] <<- v[1]
  })
})
multi_run(pool = pool)

cal <- aggregate(v ~ key, data.frame(key = key[pick], v = vals), mean)
m   <- do.call(rbind, lapply(strsplit(cal$key, ","), as.numeric))
out <- data.frame(r = m[,1], g = m[,2], b = m[,3], v = signif(cal$v, 5))
write.csv(out[order(out$v), ], "data/land_deformation/mosaic_asc_colour_calibration.csv", row.names = FALSE)
