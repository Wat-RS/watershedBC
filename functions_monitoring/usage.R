
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



