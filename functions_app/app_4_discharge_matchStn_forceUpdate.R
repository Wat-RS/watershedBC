

pred_Q_prepWS <-
  function(w = new_ws2,
           my_wetlands = my_wl,
           my_glaciers = my_gl,
           my_lakes = my_lk) {
    w$wetland_sum_perc <-
      100 * ((sum(my_wetlands$clipped_area_m2) / (1000 * 1000)) / w$area_km2)
    w$lake_sum_perc <-
      100 * ((sum(my_lakes$clipped_area_m2) / (1000 * 1000)) / w$area_km2)
    w$glacier_sum_perc <-
      100 * ((sum(my_glaciers$clipped_area_m2) / (1000 * 1000)) / w$area_km2)

    # Regular grid, not random: elevation quartiles from a few random points shift by
    # tens of metres between runs, which moves the station match score noticeably
    pts <-
      terra::spatSample(vect(w %>% st_transform(4326)), method = "regular", size = 300)

    # TERRAIN
    dem_pts <- elevatr::get_aws_points(pts %>% st_as_sf())[[1]]

    # CLIMATE BC
    # Read the two layers into memory first: extracting a few hundred points straight
    # from the remote file silently returns all NA
    climateBC <-
      terra::toMemory(terra::rast(
        "/vsicurl/https://bcbasin.s3.ca-central-1.amazonaws.com/climateBC.tif",
        win = terra::ext(vect(w %>% st_transform(4326)))
      )[[c("Normal_1991_2020_MAT_cog.tif", "Normal_1991_2020_MAP_cog.tif")]])

    w$MAT <-
      terra::extract(climateBC["Normal_1991_2020_MAT_cog.tif"], pts) %>% pull(Normal_1991_2020_MAT_cog.tif) %>% mean(na.rm = T)
    w$MAP <-
      terra::extract(climateBC["Normal_1991_2020_MAP_cog.tif"], pts) %>% pull(Normal_1991_2020_MAP_cog.tif) %>% mean(na.rm = T)

    # STATISTICS
    w$dem_p025 <-
      dem_pts %>% pull(elevation) %>% quantile(probs = 0.25, na.rm = T)
    w$dem_p050 <-
      dem_pts %>% pull(elevation) %>% quantile(probs = 0.50, na.rm = T)
    w$dem_p075 <-
      dem_pts %>% pull(elevation) %>% quantile(probs = 0.75, na.rm = T)

    return(w)
  }

# Score every reference station in the watershed's flow-regime model and return
# them best-first: score = random-forest class probability (sums to 1 across the
# model's stations), dist_km = watershed outlet to gauge. Ties (mostly zero
# scores) are ordered by distance.
# Each station is its own class with a single training row, so even a perfect
# analogue only scores ~0.2. ceiling = the score a station gets for its own
# attributes, and match = score / ceiling puts the score on a 0-100% scale.
pred_Q_rankRef <-
  function(w = new_ws2_forRF()) {
    regime <- if (w$regulated == "regulated") "Regulated Flow" else "Natural Flow"
    model  <- readRDS(if (regime == "Regulated Flow")
      "functions_model_app/rf_wsc_pred_station_reg_20240206_LL_A_PROB.RDS" else
      "functions_model_app/rf_wsc_pred_station_nat_20240206_LL_A_PROB.RDS")

    requireNamespace("ranger", quietly = TRUE)  # needed for predict() on ranger models
    set.seed(500)

    pool <- stn_training %>% filter(regulated == regime)
    vars <- c("wetland_sum_perc", "lake_sum_perc", "glacier_sum_perc", "area_km2", "MAT", "MAP",
              "dem_p025", "dem_p050", "dem_p075", "LONGITUDE", "LATITUDE")
    pool_x <- pool %>% rename(MAT = mat, MAP = map, LONGITUDE = x_3005, LATITUDE = y_3005) %>% select(all_of(vars))

    # Row 1 is the watershed, the rest are the training stations themselves
    prob <- predict(model, bind_rows(w %>% st_drop_geometry() %>% select(all_of(vars)), pool_x))$predictions
    pool$ceiling <- prob[cbind(1 + seq_len(nrow(pool)), match(pool$station_cluster, colnames(prob)))]

    data.frame(station_cluster = colnames(prob), score = prob[1, ]) %>%
      inner_join(pool, by = "station_cluster") %>%
      mutate(match   = pmin(score / ceiling, 1),
             dist_km = sqrt((x_3005 - w$LONGITUDE)^2 + (y_3005 - w$LATITUDE)^2) / 1000) %>%
      arrange(-score, dist_km) %>%
      mutate(rank = row_number())
  }

pred_Q_findRef <-
  function(w = new_ws2_forRF(),
           force_station = NULL) {
    if (is.null(force_station)) force_station <- pred_Q_rankRef(w)$station_number[1]
    force_station
  }

# Reference-station explanation helpers ####

# Same markup as caption() in app.R, which isn't visible from this file
ref_caption <- function(...) HTML(paste0('<div class="caption-box">', ..., '</div>'))

fmt_score <- function(p) ifelse(p > 0 & p < 0.005, "<1%", paste0(round(100 * p), "%"))

# Dropdown choices: the regime's stations best-first, then the other regime's (unscored)
ref_choices <- function(rank_df) {
  others <- stn_training %>% filter(!station_number %in% rank_df$station_number)
  setNames(
    list(
      setNames(rank_df$station_number,
               paste0("#", rank_df$rank, "  ", rank_df$station_number, " - ", rank_df$name,
                      "  |  match ", fmt_score(rank_df$match), "  |  ", round(rank_df$dist_km), " km")),
      setNames(others$station_number, others$total_name)),
    c(paste0("Ranked: ", rank_df$regulated[1], " model (", nrow(rank_df), " stations)"),
      "Other stations (not scored for this watershed)"))
}

# Descriptors shown in the comparison: watershed column, station column, label, units
ref_vars <- tibble::tribble(
  ~w_col,             ~s_col,             ~label,                  ~unit,
  "area_km2",         "area_km2",         "drainage area",         "km2",
  "MAP",              "map",              "mean annual precip.",   "mm",
  "MAT",              "mat",              "mean annual temp.",     "C",
  "dem_p050",         "dem_p050",         "median elevation",      "m",
  "lake_sum_perc",    "lake_sum_perc",    "lake cover",            "%",
  "glacier_sum_perc", "glacier_sum_perc", "glacier cover",         "%",
  "wetland_sum_perc", "wetland_sum_perc", "wetland cover",         "%")

# Plain-language difference, station relative to watershed
# (round() + 0 turns -0 into 0, so nothing prints as "-0")
ref_diff_txt <- function(var, wv, sv) {
  switch(var,
    area_km2 = if (sv >= wv) paste0(signif(sv / wv, 2), "x larger") else paste0(signif(wv / sv, 2), "x smaller"),
    MAP      = sprintf("%+.0f%%", round(100 * (sv - wv) / wv) + 0),
    MAT      = sprintf("%+.1f °C", round(sv - wv, 1) + 0),
    dem_p050 = sprintf("%+.0f m", round(sv - wv) + 0),
    sprintf("%+.1f pts", round(sv - wv, 1) + 0))
}

ref_explain_html <- function(rank_df, w, sel) {
  w   <- st_drop_geometry(w)
  top <- rank_df[1, ]
  s   <- rank_df %>% filter(station_number == sel)
  regime_txt <- tolower(rank_df$regulated[1])
  n <- nrow(rank_df)

  if (nrow(s) == 0) {  # station from the other flow regime: not scored
    s <- stn_training %>% filter(station_number == sel)
    return(ref_caption(
      "<b>You selected ", sel, " - ", s$name[1], "</b> (", tolower(s$regulated[1]), " station). ",
      "This watershed is classed as ", w$regulated, ", so only the ", n, " ", regime_txt,
      " stations were scored and this station has no match score. ",
      "The model's top match is <b>", top$station_number, " - ", top$name, "</b> (match ", fmt_score(top$match), ")."))
  }

  # Similarity per descriptor: absolute difference in units of the training spread
  # (log scale for area, which spans orders of magnitude)
  pool <- stn_training %>% filter(regulated == rank_df$regulated[1])
  z <- sapply(seq_len(nrow(ref_vars)), function(i) {
    wv <- w[[ref_vars$w_col[i]]]; sv <- s[[ref_vars$s_col[i]]]; pv <- pool[[ref_vars$s_col[i]]]
    if (ref_vars$w_col[i] == "area_km2") abs(log10(sv) - log10(wv)) / sd(log10(pv), na.rm = TRUE)
    else abs(sv - wv) / sd(pv, na.rm = TRUE)
  })
  desc <- sapply(seq_len(nrow(ref_vars)), function(i)
    paste0(ref_vars$label[i], " (", ref_diff_txt(ref_vars$w_col[i], w[[ref_vars$w_col[i]]], s[[ref_vars$s_col[i]]]), ")"))
  # Cover fractions that are ~0 in both basins say nothing about the match, so leave them out
  both_absent <- grepl("_perc$", ref_vars$w_col) &
    sapply(seq_len(nrow(ref_vars)), function(i) max(w[[ref_vars$w_col[i]]], s[[ref_vars$s_col[i]]]) < 0.1)
  ord <- order(z)[!both_absent[order(z)]]

  head_txt <- if (s$rank == 1) {
    paste0("<b>Why ", s$station_number, " - ", s$name, "?</b> It is the model's top-ranked station (match ",
           fmt_score(s$match), ") of the ", n, " ", regime_txt, " reference stations ",
           "(this watershed is classed as ", w$regulated, "). ")
  } else {
    paste0("<b>You selected ", s$station_number, " - ", s$name, "</b>: rank #", s$rank, " of ", n,
           ", match ", fmt_score(s$match), ". The model's top match is <b>", top$station_number,
           " - ", top$name, "</b> (match ", fmt_score(top$match), "). ")
  }
  ref_caption(
    head_txt,
    "Compared with this watershed, the station's basin is most similar in ",
    paste(desc[head(ord, 3)], collapse = ", "), "; and least similar in ",
    paste(desc[head(rev(ord), 2)], collapse = " and "), ". ",
    "The gauge is ", round(s$dist_km), " km from the watershed outlet.",
    "<br><i>Stations are ranked by a random forest trained on area, location, climate (ClimateBC MAT, MAP), ",
    "elevation quartiles, and lake, glacier and wetland cover. Its probability (model prob.) is split across all ",
    n, " stations, so even a perfect analogue only reaches about 20%. Match rescales it: 100% means this ",
    "watershed scores as highly as the station's own basin does.</i>")
}

# Match quality for the selected station. Each issue is a mismatch large enough to
# make the scaled estimate doubtful; 1 issue = weak match, 2+ = no good reference.
# Area matters most because discharge is scaled from the reference by area.
ref_quality <- function(rank_df, w, sel) {
  w <- st_drop_geometry(w)
  s <- rank_df %>% filter(station_number == sel)
  scored <- nrow(s) == 1
  if (!scored) s <- stn_training %>% filter(station_number == sel) %>% slice(1)
  area_ratio <- max(s$area_km2 / w$area_km2, w$area_km2 / s$area_km2)
  map_diff   <- (s$map - w$MAP) / w$MAP
  elev_diff  <- s$dem_p050 - w$dem_p050
  issues <- c(
    if (!scored) paste0("it is a ", tolower(s$regulated), " station but this watershed is ", w$regulated),
    if (scored && s$match < 0.25) paste0("its model match is low (", fmt_score(s$match), ")"),
    if (area_ratio > 10) paste0("its drainage area is ", round(area_ratio), "x ",
                                if (s$area_km2 > w$area_km2) "larger" else "smaller"),
    if (abs(map_diff) > 0.3) sprintf("its mean annual precipitation differs by %+.0f%%", 100 * map_diff),
    if (abs(elev_diff) > 300) sprintf("its median elevation differs by %+.0f m", elev_diff))
  list(level = c("good", "weak", "poor")[min(length(issues), 2) + 1], issues = issues,
       is_top = scored && s$rank == 1)
}

ref_warning_html <- function(q, sel) {
  if (q$level == "good") return(NULL)
  col  <- if (q$level == "poor") c("#fdecea", "#b71c1c") else c("#fff4e5", "#e65100")
  head <- if (q$level == "poor") {
    if (q$is_top) "⚠ No good reference station for this watershed" else "⚠ Poor reference station selected"
  } else "⚠ Weak reference station match"
  HTML(paste0(
    '<div style="background:', col[1], ';border-left:5px solid ', col[2], ';padding:8px 12px;margin:6px 0;">',
    '<b style="color:', col[2], '">', head, '</b><br>',
    if (q$is_top) "Even the best-matching station (" else "The selected station (", sel, ") differs from this watershed: ",
    paste(q$issues, collapse = "; "), ". ",
    if (q$level == "poor") "Treat the discharge estimate below as indicative only. " else "Interpret the estimate with caution. ",
    if (q$is_top) "No other station scores higher; check the alternatives in the dropdown and table." else
      "Consider the model's top-ranked station instead.",
    '</div>'))
}

# Comparison table: this watershed, the top 5 candidates, and the selection if outside the top 5
ref_compare_table <- function(rank_df, w, sel) {
  w <- st_drop_geometry(w)
  rows <- rank_df %>% filter(rank <= 5 | station_number == sel)
  if (!sel %in% rank_df$station_number)
    rows <- bind_rows(rows, stn_training %>% filter(station_number == sel) %>%
                        mutate(dist_km = sqrt((x_3005 - w$LONGITUDE)^2 + (y_3005 - w$LATITUDE)^2) / 1000))
  bind_rows(
    tibble(Rank = "", Station = "This watershed", Name = "", Match = "", `Model prob.` = "", `Distance (km)` = "",
           `Area (km²)` = w$area_km2, `MAP (mm)` = w$MAP, `MAT (°C)` = w$MAT,
           `Median elev. (m)` = w$dem_p050, `Lake %` = w$lake_sum_perc,
           `Glacier %` = w$glacier_sum_perc, `Wetland %` = w$wetland_sum_perc),
    tibble(Rank = paste0(ifelse(rows$station_number == sel, "► ", ""),
                         ifelse(is.na(rows$rank), "–", paste0("#", rows$rank))),
           Station = rows$station_number, Name = rows$name,
           Match = ifelse(is.na(rows$match), "not scored", fmt_score(rows$match)),
           `Model prob.` = ifelse(is.na(rows$score), "", fmt_score(rows$score)),
           `Distance (km)` = as.character(round(rows$dist_km)),
           `Area (km²)` = rows$area_km2, `MAP (mm)` = rows$map, `MAT (°C)` = rows$mat,
           `Median elev. (m)` = rows$dem_p050, `Lake %` = rows$lake_sum_perc,
           `Glacier %` = rows$glacier_sum_perc, `Wetland %` = rows$wetland_sum_perc)
  ) %>%
    mutate(`Area (km²)` = format(round(`Area (km²)`), big.mark = ",", trim = TRUE),
           `MAP (mm)` = sprintf("%.0f", `MAP (mm)`), `MAT (°C)` = sprintf("%.1f", round(`MAT (°C)`, 1) + 0),
           `Median elev. (m)` = sprintf("%.0f", `Median elev. (m)`),
           across(c(`Lake %`, `Glacier %`, `Wetland %`), ~ sprintf("%.1f", .x)))
}

# Daily day-of-year values averaged into weeks (1-52, day 365/366 joins week 52) or
# months; `day` is the day of year, `date` the "Jan-01" style label
q_period <- function(day, date, step) {
  switch(step,
    Weekly  = pmin(ceiling(day / 7), 52),
    Monthly = match(substr(date, 1, 3), month.abb),
    day)
}

pred_Q_rf <- function(w = new_ws2_forRF(),
                      force_station = ref_stn,
                      wsc_STATION_NUMBER = NULL,
                      plot = T,
                      step = "Daily") {

    ref_wsc_daily <-
      st_read(
        conn,
        query = paste0(
          "SELECT * FROM daily_wsc_clean_20240205 WHERE station_number = '",
          force_station,
          "'"
        )
      )

    ref_wsc_area <-
      st_read(
        conn,
        query = paste0(
          "SELECT area FROM wsc_drainagebasin_clean_3005 WHERE stationnum = '",
          force_station,
          "'"
        )
      )

    ref_mad_perkm2 <-
      mean(ref_wsc_daily$mean, na.rm = T) / (ref_wsc_area$area / (1000 * 1000))

    target_mad <- ref_mad_perkm2 * w$area_km2

    target_mad_ts <- ref_wsc_daily %>%
      mutate(
        `LT Mean` = mean_mad_perc * target_mad,
        `LT 95th` = p95_mad_perc * target_mad,
        `LT 5th` = p5_mad_perc * target_mad
      ) %>%
      mutate(period = q_period(dayof_year, date, step)) %>%
      group_by(period) %>%
      summarise(across(c(`LT Mean`, `LT 95th`, `LT 5th`), ~ mean(.x, na.rm = TRUE)), .groups = "drop")

    x_lab   <- c(Daily = "Day", Weekly = "Week", Monthly = "Month")[[step]]
    x_scale <- if (step == "Monthly")
      scale_x_continuous(expand = c(0, 0), breaks = 1:12, labels = month.abb) else
      scale_x_continuous(expand = c(0, 0))

    if (plot != T) return(data.frame(MAD = target_mad))

    # Observed long-term median for a WSC basin, averaged to the same step
    wsc_lt <- if (!is.null(wsc_STATION_NUMBER))
      st_read(conn, query = paste0(
        "SELECT * FROM wsc_pp_fasstr_calc_daily_stats WHERE STATION_NUMBER = '", wsc_STATION_NUMBER, "'"))
    has_obs <- !is.null(wsc_lt) && nrow(wsc_lt) > 0
    obs_lbl <- paste0(wsc_STATION_NUMBER, " [WSC Actual]")

    # Named so the observed series gets its own colour without shifting the others
    cols <- c(`LT 5th` = "blue", `LT 95th` = "#D53E4F", `LT Mean` = "black", MAD = "green")
    ltys <- c(`LT 5th` = 1, `LT 95th` = 1, `LT Mean` = 1, MAD = 2)
    if (has_obs) { cols[obs_lbl] <- "darkorange"; ltys[obs_lbl] <- 1 }

    pppp <- target_mad_ts %>%
      pivot_longer(cols = -period) %>%
      ggplot() +
      geom_line(aes(
        period, value, color = name, linetype = name
      )) +
      geom_hline(aes(
        yintercept = target_mad,
        linetype = "MAD",
        color = "MAD"
      )) +
      annotate("text",
        x = min(target_mad_ts$period) + diff(range(target_mad_ts$period)) * 0.02,
        y = target_mad,
        hjust = 0, vjust = -0.5,
        label = paste0("MAD: ", signif(target_mad, 4))
      ) +
      scale_linetype_manual(values = ltys) +
      scale_color_manual(values = cols) +
      x_scale +
      scale_y_continuous(expand = c(0, 0)) +
      labs(
        x = x_lab,
        y = paste0("Estimated Mean ", step, " Q (cms)"),
        color = "Estimate Percentiles",
        linetype = "Estimate Percentiles",
        title = paste0(
          "Model v3 [WSC reference station: ",
          force_station,
          ", ",
          w$regulated,
          "]"
        )
      ) +
      theme_bw()

    if (!has_obs) return(pppp)

    wsc_lt <- wsc_lt %>%
      mutate(period = q_period(yday, date, step)) %>%
      group_by(period) %>%
      summarise(median = mean(median, na.rm = TRUE), .groups = "drop")
    pppp +
      geom_line(data = wsc_lt, aes(period, median, color = obs_lbl, linetype = obs_lbl), linewidth = 1)
}
