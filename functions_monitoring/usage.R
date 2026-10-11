# Usage plots are built on demand (when the Usage tab is opened), not at app startup,
# so the full usage table is only read when someone looks at it.
make_usage_plots <- function() {


d <- dbReadTable(conn, "usage")

(df <- d %>%
    # as_tibble() %>%
    select(session_token, date_time, gnis_name, processing_time, action, area_km2) %>%
    filter(processing_time != "") %>%
    filter(!is.na(processing_time)) %>%
    as_tibble() %>%
    # select(processing_time) %>%
    mutate(time_type = str_split_fixed(processing_time," ",3)[,2],
           processing_time =  str_split_fixed(processing_time," ",3)[,1]) %>%
    mutate(processing_time = case_when(time_type == "minutes" ~ as.numeric(processing_time),
                            time_type == "sec" ~ as.numeric(processing_time)/60,
                            TRUE ~ -9999)) %>%
    mutate(date_time = as_datetime(date_time) %>% with_tz("America/Vancouver")) %>%
  mutate(date = as.Date.character(as.character(date_time), format = "%Y-%m-%d")) %>%
  mutate(row = row_number()) %>%
  arrange(rev(date_time)))

# df %>%
#   group_by(date, action) %>%
#   filter(action != "start") %>%
#   summarise(n = n()) %>%
#   ggplot(aes(date, n)) +
#   geom_col(aes(fill = action)) +
#   theme_bw() +
#   scale_x_date(date_breaks = "5 day", expand = c(0,0)) +
#   theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
#   facet_wrap(.~action, scales = "free_y", ncol = 1)

plot_usage_recent <- df %>% as_tibble() %>%
  filter(action == "processing") %>%
  arrange(rev(date_time)) %>%
  # filter(date > Sys.Date()-3) %>%
  filter(row_number() < 30) %>%
  mutate(gnis_name = paste0(gnis_name, " ", format(date_time, "(%Y-%m-%d %H:%M)") )) %>%
  mutate(gnis_name = fct_reorder(gnis_name, date_time)) %>%
  ggplot(aes(date_time, gnis_name)) +
  geom_point(aes(fill = session_token, size = time),
             shape = 21,
             # size = 3,
             show.legend = F) +
  geom_vline(xintercept = now(), linetype = 2) +
  theme_bw() +
  labs(x = "Date/Time of Report",
       y = "") +
  theme(aspect.ratio = 1.4)

# df %>% ggplot(aes(area_km2, time)) +
#   geom_point() +
#   geom_smooth(method = "lm") +
#   theme_bw() + theme(aspect.ratio = 1) +
#   scale_x_continuous(n.breaks = 10) +
#   scale_y_continuous(n.breaks = 10) +
#   labs(x = bquote(Area~(km^2)),
#        y = "Minutes to Process") +
#   facet_wrap(.~action, scales = "free")
#
# summary(lm(time~area_km2, data = df))
#
#
# df %>%
#   filter(gnis_name == "Bowron River") %>%
#   filter(action == "processing") %>%
#   ggplot() +
#   geom_point(aes(date_time, time))
#
#
# df %>%
#   filter(action == "processing") %>%
#   ggplot() +
#   geom_point(aes(date_time, time, size = area_km2))
#
#
#
#   refresh <- function() {
# }


plot_usage_top <- df %>%
  filter(action == "processing") %>%
  group_by(gnis_name) %>%
  summarize(n = n()) %>%
  arrange(-n) %>%
  filter(n > 1) %>%
  filter(row_number() < 50) %>%
  mutate(gnis_name = fct_reorder(gnis_name, n)) %>%
  ggplot() +
  geom_col(aes(gnis_name, n)) +
  coord_flip() +
  theme_bw() +
  scale_y_continuous(expand = c(0,0)) +
    labs(x = "Top 50 watersheds",
       y = "Total number of reports (to date)") +
  theme(aspect.ratio = 1.4)

# Visits per day: one visit = one session, dated by its first row. Before Oct 2026
# only sessions that selected a watershed were logged; since then every app open is.
visits_daily <- d %>%
  as_tibble() %>%
  mutate(date_time = as_datetime(date_time) %>% with_tz("America/Vancouver")) %>%
  filter(!is.na(date_time)) %>%
  group_by(session_token) %>%
  summarize(date = as.Date(min(date_time), tz = "America/Vancouver")) %>%
  count(date, name = "visits") %>%
  complete(date = seq(min(date), max(date), by = "day"), fill = list(visits = 0)) %>%
  mutate(visits_7d = as.numeric(stats::filter(visits, rep(1/7, 7), sides = 1)))

plot_usage_visits <- visits_daily %>%
  ggplot(aes(date)) +
  geom_col(aes(y = visits), fill = "#3d90a8", alpha = 0.45, width = 1) +
  geom_line(aes(y = visits_7d), colour = "#084b61", linewidth = 0.8, na.rm = TRUE) +
  geom_vline(xintercept = as.Date("2026-10-11"), linetype = 2, colour = "grey50") +
  annotate("text", x = as.Date("2026-10-11"), y = Inf, vjust = 1.5, hjust = 1.05, size = 3,
           colour = "grey35", label = "all app opens\nlogged from here") +
  scale_x_date(date_breaks = "3 months", date_labels = "%b %Y", expand = c(0, 0)) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
  theme_bw() +
  theme(panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  labs(x = "", y = "Visits per day",
       title = paste0(format(sum(visits_daily$visits), big.mark = ","), " visits since ",
                      format(min(visits_daily$date), "%b %Y")),
       subtitle = "Bars: daily visits · line: 7-day average")

plot_usage_total <- df %>%
  filter(action == "processing") %>%
  group_by(date, action) %>%
  summarize(n = n()) %>%
  ggplot() +
    geom_line(aes(date, cumsum(n))) +
    theme_bw() +
    labs(x = "",
         y = "Cumulative reports") +
    theme(aspect.ratio = 1.4)




list(visits = plot_usage_visits, recent = plot_usage_recent,
     top = plot_usage_top, total = plot_usage_total)
}
