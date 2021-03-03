### Load packages, define setup and graphics theme ----
source("02_code/00_packages_setup_theme.R") 

### Impact ----

## Deaths ----

# Calculate total number of deaths
tot_num_deaths <- 
  mex_deaths %>% 
  summarise(last_qt = sum(num_deaths)) %>% 
  pull()

# Daily confirmed COVID-19 deaths in Mexico ----
mex_deaths %>% 
  # Remove deaths registered before March 18th, 2020, which is the official date for the first death
  filter(fecha_def > as_date("2020-03-17")) %>% 
  mutate(seven_days_mov_avg = rollmean(num_deaths, k = 7, align = 'right', fill = NA),
         seven_days_mov_avg = ifelse(Sys.Date() - fecha_def < 16, NA, seven_days_mov_avg)) %>% 
  ggplot(aes(x = fecha_def, y = num_deaths)) +
  geom_col(fill = "grey30", color = "grey30") +
  geom_vline(xintercept = as_date("2021-01-01"), color = "white") +
  geom_line(aes(y = seven_days_mov_avg), color = "salmon", size = 2) +
  annotate(geom = "text", x = as_date("2020-12-18"), y = 80, label = "2020", fontface = "bold",  color = "white", size = 8, family = "Raleway Medium", hjust = 0.5) +
  annotate(geom = "text", x = as_date("2021-01-15"), y = 80, label = "2021", fontface = "bold",  color = "white", size = 8, family = "Raleway Medium", hjust = 0.5) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b", expand = c(0, 0)) +
  scale_y_continuous(breaks = seq(0, 1500, 100), 
                     limits = c(0, 1300),
                     expand = c(0, 0), 
                     labels = comma) +
  labs(title = "Daily confirmed COVID-19 deaths in Mexico",
       subtitle = "The **<span style='color:#fa8072;'>red</span>** line shows the seven-day moving average. It stops 15 days before the last day in the database due to<br>the backlog for this period.",
       x = NULL,
       y = NULL,
       caption = str_c("\n@segasi / Data: Mexico's Health Ministry, last updated on ", Sys.Date() - 1)) +
  theme_p +
  theme(plot.title = element_text(size = 37),
        plot.subtitle = element_markdown(size = 22),
        plot.caption = element_text(size = 18),
        axis.text = element_text(size = 20),
        axis.ticks.y = element_blank()) +
  ggsave("03_vis/impact/deaths/01_a_mexico_daily_deaths.png", dpi = 200, width = 16, height = 9)

# Daily confirmed COVID-19 deaths in Mexico, w/first legend ----
mex_deaths %>% 
  # Remove deaths registered before March 18th, 2020, which is the official date for the first death
  filter(fecha_def > as_date("2020-03-17")) %>% 
  mutate(seven_days_mov_avg = rollmean(num_deaths, k = 7, align = 'right', fill = NA),
         seven_days_mov_avg = ifelse(Sys.Date() - fecha_def < 16, NA, seven_days_mov_avg)) %>% 
  ggplot(aes(x = fecha_def, y = num_deaths)) +
  geom_col(fill = "grey30", color = "grey30") +
  geom_text(aes(x = as_date("2020-07-01"), y = 1100, label = str_c(comma(tot_num_deaths), " total deaths")), family = "Raleway Medium", check_overlap = T, size = 16, color = "grey20") +
  geom_vline(xintercept = as_date("2021-01-01"), color = "white") +
  geom_line(aes(y = seven_days_mov_avg), color = "salmon", size = 2) +
  annotate(geom = "text", x = as_date("2020-12-18"), y = 80, label = "2020", fontface = "bold",  color = "white", size = 8, family = "Raleway Medium", hjust = 0.5) +
  annotate(geom = "text", x = as_date("2021-01-15"), y = 80, label = "2021", fontface = "bold",  color = "white", size = 8, family = "Raleway Medium", hjust = 0.5) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b", expand = c(0, 0)) +
  scale_y_continuous(breaks = seq(0, 1500, 100), 
                     limits = c(0, 1300),
                     expand = c(0, 0), 
                     labels = comma) +
  labs(title = "Daily confirmed COVID-19 deaths in Mexico",
       subtitle = "The **<span style='color:#fa8072;'>red</span>** line shows the seven-day moving average. It stops 15 days before the last day in the database due to<br>the backlog for this period.",
       x = NULL,
       y = NULL,
       caption = str_c("\n@segasi / Data: Mexico's Health Ministry, last updated on ", Sys.Date() - 1)) +
  theme_p +
  theme(plot.title = element_text(size = 37),
        plot.subtitle = element_markdown(size = 22),
        plot.caption = element_text(size = 18),
        axis.text = element_text(size = 20),
        axis.ticks.y = element_blank()) +
  ggsave("03_vis/impact/deaths/01_b_mexico_daily_deaths.png", dpi = 200, width = 16, height = 9)


# Daily confirmed COVID-19 deaths in Mexico, w/both legends ----
mex_deaths %>% 
  # Remove deaths registered before March 18th, 2020, which is the official date for the first death
  filter(fecha_def > as_date("2020-03-17")) %>% 
  mutate(seven_days_mov_avg = rollmean(num_deaths, k = 7, align = 'right', fill = NA),
         seven_days_mov_avg = ifelse(Sys.Date() - fecha_def < 16, NA, seven_days_mov_avg)) %>% 
  ggplot(aes(x = fecha_def, y = num_deaths)) +
  geom_col(fill = "grey30", color = "grey30") +
  geom_text(aes(x = as_date("2020-07-01"), y = 1100, label = str_c(comma(tot_num_deaths), " total deaths")), family = "Raleway Medium", check_overlap = T, size = 16, color = "grey20") +
  annotate(geom = "text", x = as_date("2020-07-01"), y = 950, label = "Third highest for any country", family = "Raleway Medium", check_overlap = T, size = 12, color = "grey40") +
  geom_vline(xintercept = as_date("2021-01-01"), color = "white") +
  geom_line(aes(y = seven_days_mov_avg), color = "salmon", size = 2) +
  annotate(geom = "text", x = as_date("2020-12-18"), y = 80, label = "2020", fontface = "bold",  color = "white", size = 8, family = "Raleway Medium", hjust = 0.5) +
  annotate(geom = "text", x = as_date("2021-01-15"), y = 80, label = "2021", fontface = "bold",  color = "white", size = 8, family = "Raleway Medium", hjust = 0.5) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b", expand = c(0, 0)) +
  scale_y_continuous(breaks = seq(0, 1500, 100), 
                     limits = c(0, 1300),
                     expand = c(0, 0), 
                     labels = comma) +
  labs(title = "Daily confirmed COVID-19 deaths in Mexico",
       subtitle = "The **<span style='color:#fa8072;'>red</span>** line shows the seven-day moving average. It stops 15 days before the last day in the database due to<br>the backlog for this period.",
       x = NULL,
       y = NULL,
       caption = str_c("\n@segasi / Data: Mexico's Health Ministry, last updated on ", Sys.Date() - 1)) +
  theme_p +
  theme(plot.title = element_text(size = 37),
        plot.subtitle = element_markdown(size = 22),
        plot.caption = element_text(size = 18),
        axis.text = element_text(size = 20),
        axis.ticks.y = element_blank()) +
  ggsave("03_vis/impact/deaths/01_c_mexico_daily_deaths.png", dpi = 200, width = 16, height = 9)


# Daily confirmed COVID-19 deaths in Mexico, highlighting last quarter and first legend ----

# Calculate deaths between Dec. 2020 and Feb. 2021
num_deaths_last_qt <- 
  mex_deaths %>% 
  filter(fecha_def >= as_date("2020-12-01") & fecha_def <= as_date("2021-03-01")) %>% 
  summarise(last_qt = sum(num_deaths)) %>% 
  pull()

num_deaths_last_qt_comma <- 
  mex_deaths %>% 
  filter(fecha_def >= as_date("2020-12-01") & fecha_def <= as_date("2021-03-01")) %>% 
  summarise(last_qt = comma(sum(num_deaths))) %>% 
  pull()

mex_deaths %>% 
  # Remove deaths registered before March 18th, 2020, which is the official date for the first death
  filter(fecha_def > as_date("2020-03-17")) %>% 
  mutate(color_bars = ifelse(fecha_def >= as_date("2020-12-01") & fecha_def <= as_date("2021-03-01"), "A", "B")) %>% 
  ggplot(aes(x = fecha_def, y = num_deaths, fill = color_bars, color = color_bars )) +
  geom_col() +
  geom_vline(xintercept = as_date("2021-01-01"), color = "white") +
  geom_curve(x = as_date("2020-12-15"), y = 1080, xend = as_date("2021-01-03"), yend = 970, arrow = arrow(length = unit(0.03, "npc")), curvature = -0.2, size = 1, color = "grey40") +
  annotate(geom = "text", x = as_date("2020-12-18"), y = 80, label = "2020", fontface = "bold",  color = "white", size = 8, family = "Raleway Medium", hjust = 0.5) +
  annotate(geom = "text", x = as_date("2021-01-15"), y = 80, label = "2021", fontface = "bold",  color = "white", size = 8, family = "Raleway Medium", hjust = 0.5) +
  annotate(geom = "richtext", x = as_date("2020-10-15"), y = 1100, label = str_c("Dec. '20-Feb '21: <span style='color:#fa8072;'>", num_deaths_last_qt_comma, "</span>"), family = "Raleway Medium", size = 12, color = "grey20", label.color = "transparent") +
  scale_x_date(date_breaks = "1 month", date_labels = "%b", expand = c(0, 0)) +
  scale_y_continuous(breaks = seq(0, 1500, 100), 
                     limits = c(0, 1300),
                     expand = c(0, 0), 
                     labels = comma) +
  scale_fill_manual(values = c("grey70", "grey30")) +
  scale_color_manual(values = c("grey70", "grey30")) +
  labs(title = "Daily confirmed COVID-19 deaths in Mexico",
       # subtitle = "The red line shows the seven-day moving average. It stops 15 days before the last day in the database due to<br>the data backlog for this period.",
       x = NULL,
       y = NULL,
       caption = str_c("\n@segasi / Data: Mexico's Health Ministry, last updated on ", Sys.Date() - 1)) +
  theme_p +
  theme(plot.title = element_text(size = 37),
        plot.subtitle = element_markdown(size = 22, color = "transparent"),
        plot.caption = element_text(size = 18),
        legend.position = "none",
        axis.text = element_text(size = 20),
        axis.ticks.y = element_blank()) +
  ggsave("03_vis/impact/deaths/02_a_mexico_daily_deaths_highlight_dec_to_feb.png", dpi = 200, width = 16, height = 9)


# Daily confirmed COVID-19 deaths in Mexico, highlighting last quarter and two legends ----

mex_deaths %>% 
  # Remove deaths registered before March 18th, 2020, which is the official date for the first death
  filter(fecha_def > as_date("2020-03-17")) %>% 
  mutate(color_bars = ifelse(fecha_def >= as_date("2020-12-01") & fecha_def <= as_date("2021-03-01"), "A", "B")) %>% 
  ggplot(aes(x = fecha_def, y = num_deaths, fill = color_bars, color = color_bars )) +
  geom_col() +
  geom_vline(xintercept = as_date("2021-01-01"), color = "white") +
  geom_curve(x = as_date("2020-12-15"), y = 1080, xend = as_date("2021-01-03"), yend = 970, arrow = arrow(length = unit(0.03, "npc")), curvature = -0.2, size = 1, color = "grey40") +
  annotate(geom = "text", x = as_date("2020-12-18"), y = 80, label = "2020", fontface = "bold",  color = "white", size = 8, family = "Raleway Medium", hjust = 0.5) +
  annotate(geom = "text", x = as_date("2021-01-15"), y = 80, label = "2021", fontface = "bold",  color = "white", size = 8, family = "Raleway Medium", hjust = 0.5) +
  annotate(geom = "richtext", x = as_date("2020-10-15"), y = 1100, label = str_c("Dec. '20-Feb '21: <span style='color:#fa8072;'>", num_deaths_last_qt_comma, "</span>"), family = "Raleway Medium", size = 12, color = "grey20", label.color = "transparent") +
  annotate(geom = "richtext", x = as_date("2020-08-17"), y = 980, label = str_c("Represents <span style='color:#fa8072;'>", percent(num_deaths_last_qt/tot_num_deaths), "</span> of Mexico's total"), family = "Raleway Medium", size = 8, color = "grey40", hjust = 0, label.color = "transparent") +
  scale_x_date(date_breaks = "1 month", date_labels = "%b", expand = c(0, 0)) +
  scale_y_continuous(breaks = seq(0, 1500, 100), 
                     limits = c(0, 1300),
                     expand = c(0, 0), 
                     labels = comma) +
  scale_fill_manual(values = c("grey70", "grey30")) +
  scale_color_manual(values = c("grey70", "grey30")) +
  labs(title = "Daily confirmed COVID-19 deaths in Mexico",
       # subtitle = "The red line shows the seven-day moving average. It stops 15 days before the last day in the database due to<br>the data backlog for this period.",
       x = NULL,
       y = NULL,
       caption = str_c("\n@segasi / Data: Mexico's Health Ministry, last updated on ", Sys.Date() - 1)) +
  theme_p +
  theme(plot.title = element_text(size = 37),
        plot.subtitle = element_markdown(size = 22, color = "transparent"),
        plot.caption = element_text(size = 18),
        legend.position = "none",
        axis.text = element_text(size = 20),
        axis.ticks.y = element_blank()) +
  ggsave("03_vis/impact/deaths/02_b_mexico_daily_deaths_highlight_dec_to_feb.png", dpi = 200, width = 16, height = 9)


# Daily confirmed COVID-19 deaths in Mexico, highlighting last quarter and all legends ----

mex_deaths %>% 
  # Remove deaths registered before March 18th, 2020, which is the official date for the first death
  filter(fecha_def > as_date("2020-03-17")) %>% 
  mutate(color_bars = ifelse(fecha_def >= as_date("2020-12-01") & fecha_def <= as_date("2021-03-01"), "A", "B")) %>% 
  ggplot(aes(x = fecha_def, y = num_deaths, fill = color_bars, color = color_bars )) +
  geom_col() +
  geom_vline(xintercept = as_date("2021-01-01"), color = "white") +
  geom_curve(x = as_date("2020-12-15"), y = 1080, xend = as_date("2021-01-03"), yend = 970, arrow = arrow(length = unit(0.03, "npc")), curvature = -0.2, size = 1, color = "grey40") +
  annotate(geom = "text", x = as_date("2020-12-18"), y = 80, label = "2020", fontface = "bold",  color = "white", size = 8, family = "Raleway Medium", hjust = 0.5) +
  annotate(geom = "text", x = as_date("2021-01-15"), y = 80, label = "2021", fontface = "bold",  color = "white", size = 8, family = "Raleway Medium", hjust = 0.5) +
  annotate(geom = "richtext", x = as_date("2020-10-15"), y = 1100, label = str_c("Dec. '20-Feb '21: <span style='color:#fa8072;'>", num_deaths_last_qt_comma, "</span>"), family = "Raleway Medium", size = 12, color = "grey20", label.color = "transparent") +
  annotate(geom = "richtext", x = as_date("2020-08-17"), y = 980, label = str_c("Represents <span style='color:#fa8072;'>", percent(num_deaths_last_qt/tot_num_deaths), "</span> of Mexico's total"), family = "Raleway Medium", size = 8, color = "grey40", hjust = 0, label.color = "transparent") +
  annotate(geom = "richtext", x = as_date("2020-08-17"), y = 800, label = "Only <span style='color:#fa8072;'>seven</span> countries accumulate<br>more deaths during the <span style='color:#fa8072;'>entire<br>pandemic</span> than Mexico in the last<br><span style='color:#fa8072;'>three months</span>", family = "Raleway Medium", size = 8, color = "grey40", hjust = 0, lineheight = 1, label.color = "transparent", fill = "transparent") +
  scale_x_date(date_breaks = "1 month", date_labels = "%b", expand = c(0, 0)) +
  scale_y_continuous(breaks = seq(0, 1500, 100), 
                     limits = c(0, 1300),
                     expand = c(0, 0), 
                     labels = comma) +
  scale_fill_manual(values = c("grey70", "grey30")) +
  scale_color_manual(values = c("grey70", "grey30")) +
  labs(title = "Daily confirmed COVID-19 deaths in Mexico",
       # subtitle = "The red line shows the seven-day moving average. It stops 15 days before the last day in the database due to<br>the data backlog for this period.",
       x = NULL,
       y = NULL,
       caption = str_c("\n@segasi / Data: Mexico's Health Ministry, last updated on ", Sys.Date() - 1)) +
  theme_p +
  theme(plot.title = element_text(size = 37),
        plot.subtitle = element_markdown(size = 22, color = "transparent"),
        plot.caption = element_text(size = 18),
        legend.position = "none",
        axis.text = element_text(size = 20),
        axis.ticks.y = element_blank()) +
  ggsave("03_vis/impact/deaths/02_c_mexico_daily_deaths_highlight_dec_to_feb.png", dpi = 200, width = 16, height = 9)

# Top-15 countries with the highest number of deaths per million people ----
owid_covid %>% 
  filter(!str_detect(iso_code, "OWID") ,
         population > 1e6) %>% 
  # distinct(location) %>% print(n= Inf)
  group_by(location) %>% 
  filter(date == max(date)) %>%
  ungroup() %>%  
  top_n(n = 15, wt = total_deaths_per_million) %>% 
  mutate(label_big = comma(round(total_deaths_per_million, 1)),
         mex_color = ifelse(location == "Mexico", "A", "B")) %>% 
  ggplot(aes(x = total_deaths_per_million, 
             y = fct_reorder(str_wrap(location, width = 15), total_deaths_per_million),
             fill = mex_color)) +
  geom_col() +
  geom_text(aes(label = label_big), color = "white", fontface = "bold", family = "Raleway Medium", hjust = 1.15, size = 8) +
  scale_x_continuous(expand = c(0, 0),
                     limits = c(-10, 2050)) +
  scale_fill_manual(values = c("salmon", "grey30")) +
  labs(title = "Top-15 countries with the <span style='color:#fa8072;'>highest</span> number of deaths<br>per million people",
       subtitle = "The graph only considers countries or territories with more than 1M people",
       x = NULL, 
       y = NULL,
       caption = str_c("\n@segasi / Data: Our World in Data, last updated on ", Sys.Date())) +
  theme_p +
  theme(plot.title = element_markdown(size = 37),
        plot.caption = element_text(size = 22),
        legend.position = "none",
        panel.grid.major = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_text(size = 22)) +
  ggsave("03_vis/impact/deaths/03_a_top_15_deaths_per_million.png", width = 14, height = 15, dpi = 200)


# Top-15 countries with the highest number of deaths per million people, with legend ----
owid_covid %>% 
  filter(!str_detect(iso_code, "OWID") ,
         population > 1e6) %>% 
  group_by(location) %>% 
  filter(date == max(date)) %>%
  ungroup() %>%  
  top_n(n = 15, wt = total_deaths_per_million) %>% 
  mutate(label_big = comma(round(total_deaths_per_million, 1)),
         mex_color = ifelse(location == "Mexico", "A", "B")) %>% 
  ggplot(aes(x = total_deaths_per_million, 
             y = fct_reorder(str_wrap(location, width = 15), total_deaths_per_million),
             fill = mex_color)) +
  geom_col() +
  geom_segment(aes(x = 2000, y = 4.5, xend = 2000, yend = 15.3),
               arrow = arrow(length = unit(0.5, "cm"))) +
  geom_text(aes(label = label_big), color = "white", fontface = "bold", family = "Raleway Medium", hjust = 1.15, size = 8) +
  annotate(geom = "richtext", label = "Only 11 out of 155<br>countries have<br><span style='color:#fa8072;'>more</span> deaths per<br>1M than Mexico", x = 2000, y = 3.5, family = "Raleway Medium", color = "grey30", size = 8, lineheight = 0.9, hjust = 1, label.colour = "transparent") +
  scale_x_continuous(expand = c(0, 0),
                     limits = c(-10, 2050)) +
  scale_fill_manual(values = c("salmon", "grey30")) +
  labs(title = "Top-15 countries with the <span style='color:#fa8072;'>highest</span> number of deaths<br>per million people",
       subtitle = "The graph only considers countries or territories with more than 1M people",
       x = NULL, 
       y = NULL,
       caption = str_c("\n@segasi / Data: Our World in Data, last updated on ", Sys.Date())) +
  theme_p +
  theme(plot.title = element_markdown(size = 37),
        plot.caption = element_text(size = 22),
        legend.position = "none",
        panel.grid.major = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_text(size = 22)) +
  ggsave("03_vis/impact/deaths/03_b_top_15_deaths_per_million_with_legend.png", width = 14, height = 15, dpi = 200)



## Excess mortality ----

# Mexico's 2020 official number of COVID-19 deaths and excess deaths ----
mex_ex_mor %>% 
  gather(key = "variable",
         value = "number") %>% 
  mutate(variable = ifelse(variable == "covid_deaths", "COVID-19 deaths\n (as of Dec. 31, 2020)", "Excess deaths\n(Mar 29-Dec 31, 2020)"),
         variable = fct_relevel(variable, "COVID-19 deaths\n (as of Dec. 31, 2020)", "Excess deaths\n(Mar 29-Dec 31, 2020)")) %>% 
  ggplot(aes(x = variable, 
             y = number)) +
  geom_col() +
  geom_text(aes(label = comma(number)), color = "white", fontface = "bold", family = "Raleway Medium", vjust = 1.5, size = 9) +
  scale_y_continuous(expand = c(0, 0)) +
  labs(title = "Mexico's 2020 official number of COVID-19\ndeaths and excess deaths",
       x = NULL, 
       y = NULL,
       caption = str_c("\n@segasi / Data: Mexico's Health Ministry, last updated on ", Sys.Date())) +
  theme_p +
  theme(panel.grid.major = element_blank(),
        plot.caption = element_text(size = 20),
        axis.text.x = element_text(size = 22),
        axis.text.y = element_blank()) +
  ggsave("03_vis/impact/excess_mortality/01_a_mexico_covid_deaths_excess_mortality.png", width = 11, height = 8, dpi = 200)


# Mexico's 2020 official number of COVID-19 deaths and excess deaths with legend ----
mex_ex_mor %>% 
  gather(key = "variable",
         value = "number") %>% 
  mutate(variable = ifelse(variable == "covid_deaths", "COVID-19 deaths\n (as of Dec. 31, 2020)", "Excess deaths\n(Mar 29-Dec 31, 2020)"),
         variable = fct_relevel(variable, "COVID-19 deaths\n (as of Dec. 31, 2020)", "Excess deaths\n(Mar 29-Dec 31, 2020)")) %>% 
  ggplot(aes(x = variable, 
             y = number)) +
  geom_col() +
  geom_text(aes(label = comma(number)), color = "white", fontface = "bold", family = "Raleway Medium", vjust = 1.5, size = 9) +
  scale_y_continuous(expand = c(0, 0)) +
  annotate(geom = "richtext", x = 1, y = 2.5e5, label = "Mexico's excces deaths<br>is  <span style='color:#fa8072;'>2.58 larger</span> than the<br>official number of<br>COVID-19 deaths", family = "Raleway Medium", size = 8, color = "grey40", label.color = "transparent", fill = "transparent") +
  labs(title = "Mexico's 2020 official number of COVID-19\ndeaths and excess deaths",
       x = NULL, 
       y = NULL,
       caption = str_c("\n@segasi / Data: Mexico's Health Ministry, last updated on ", Sys.Date())) +
  theme_p +
  theme(panel.grid.major = element_blank(),
        plot.caption = element_text(size = 20),
        axis.text.x = element_text(size = 22),
        axis.text.y = element_blank()) +
  ggsave("03_vis/impact/excess_mortality/01_b_mexico_covid_deaths_excess_mortality.png", width = 11, height = 8, dpi = 200)

# Top-15 countries with the highest excess deaths per 100k people ----
eco_ex_mor %>% 
  top_n(n = 15, wt = excess_deaths_per_100k) %>%  
  mutate(mex_color = ifelse(region == "Mexico", "A", "B")) %>% 
  ggplot(aes(x = excess_deaths_per_100k, 
             y = fct_reorder(str_wrap(region, width = 15), excess_deaths_per_100k),
             fill = mex_color)) +
  geom_col() +
  geom_text(aes(label = excess_deaths_per_100k), color = "white", fontface = "bold", family = "Raleway Medium", hjust = 1.15, size = 9) +
  geom_text(aes(label = period), x = 320, color = "grey50", fontface = "bold", family = "Raleway Medium", hjust = 0.5, size = 8) +
  geom_hline(yintercept = seq(1.5, 15.5, 1), size = 0.1, color = "grey70") +
  annotate(geom = "text", label = "I'm here to make some room", x = 320, y = 16, size = 10, color = "transparent") +
  annotate(geom = "text", label = "Time period", x = 320, y = 15.8, size = 8, fontface = "bold", family = "Raleway Medium",) +
  scale_x_continuous(expand = c(0, 0),
                     limits = c(-1, 370)) +
  scale_fill_manual(values = c("salmon", "grey30")) +
  labs(title = "Top-15 countries with the <span style='color:#fa8072;'>highest</span> excess deaths<br>per 100k people",
       x = NULL, 
       y = NULL,
       caption = str_c("\n@segasi / Data: The Economist and Mexico's Health Ministry, last updated on ", Sys.Date())) +
  theme_p +
  theme(plot.title = element_markdown(size = 39),
        plot.caption = element_text(size = 22),
        legend.position = "none",
        panel.grid.major = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_text(size = 22)) +
  ggsave("03_vis/impact/excess_mortality/02_top_15_excess_mortality_per_100k.png", width = 14, height = 15, dpi = 200)




## Economy ----

# GDP % change, 2020 vs. 2019 ----
wb %>% 
  filter(!is.na(unit),
         unit != "Euro area") %>%
  ggplot(aes(x = yr, y = change_gdp/100, group = unit)) +
  geom_line(color = "grey40", alpha = 0.7, size = 1) +
  geom_point(color = "grey40", alpha = 0.7, size = 4) +
  scale_x_continuous(breaks = c(2019, 2020)) +
  scale_y_continuous(labels = percent_format(accuracy = 1),
                     limits = c(-0.22, 0.24)) +
  labs(title = "GDP % change, 2020 vs. 2019",
       x = "",
       y = "",
       fill = NULL,
       caption = "\n@segasi / Data: World Bank, Global Economic Prospects, January 2021") +
  theme_p +
  theme(plot.title = element_text(size = 42),
        plot.caption = element_text(size = 24),
        axis.text = element_text(size = 30),
        legend.text = element_text(size = 20)) +
  ggsave("03_vis/impact/economy/1_a_gdp_change_2020.png", dpi = 200, width = 16, height = 12)



# GDP % change, 2020 vs. 2019, Mexico highlighted ----
wb %>% 
  filter(!is.na(unit),
         unit != "Euro area") %>%
  mutate(mex_color = ifelse(unit == "Mexico", "Z", "A")) %>% 
  arrange(mex_color) %>% 
  ggplot(aes(x = yr, y = change_gdp/100, group = unit, color = mex_color, alpha = mex_color, size = mex_color)) +
  geom_line() +
  geom_point(size = 4) +
  annotate(geom = "text", label = "Mexico", x = 2019.875, y = -12/100, color = "#1E6847", family = "Raleway Medium", size = 12) +
  geom_curve(x = 2019.94, y = -12/100, xend = 2019.996, yend = -9.6/100, arrow = arrow(length = unit(0.03, "npc")), curvature = 0.2, size = 1, color = "grey30") +
  scale_x_continuous(breaks = c(2019, 2020)) +
  scale_y_continuous(labels = percent_format(accuracy = 1),
                     limits = c(-0.22, 0.24)) +
  scale_color_manual(values = c("grey80", "#1E6847")) +
  scale_alpha_manual(values = c(0.2, 1)) +
  scale_size_manual(values = c(0.3, 2)) +
  labs(title = "GDP % change, 2020 vs. 2019",
       x = "",
       y = "",
       fill = NULL,
       caption = "\n@segasi / Data: World Bank, Global Economic Prospects, January 2021") +
  theme_p +
  theme(plot.title = element_text(size = 42),
        plot.caption = element_text(size = 24),
        axis.text = element_text(size = 30),
        legend.text = element_text(size = 20),
        legend.position = "none") +
  ggsave("03_vis/impact/economy/1_b_gdp_change_2020.png", dpi = 200, width = 16, height = 12)


# GDP % change, 2020 vs. 2019, Mexico highlighted + figures ----
wb %>% 
  filter(!is.na(unit),
         unit != "Euro area") %>%
  mutate(mex_color = ifelse(unit == "Mexico", "Z", "A")) %>% 
  arrange(mex_color) %>% 
  ggplot(aes(x = yr, y = change_gdp/100, group = unit, color = mex_color, alpha = mex_color, size = mex_color)) +
  geom_line() +
  geom_point(size = 4) +
  annotate(geom = "text", label = "Mexico", x = 2019.875, y = -12/100, color = "#1E6847", family = "Raleway Medium", size = 12) +
  annotate(geom = "richtext", label = "WB's estimation: <span style='color:#fa8072;'>-9%</span>", x = 2019.94, y = -15/100, color = "grey40", family = "Raleway Medium", size = 10, hjust = 1, label.color = "transparent", fill = "transparent") +
  annotate(geom = "richtext", label = "INEGI: <span style='color:#fa8072;'>-8.5%</span>", x = 2019.94, y = -17.5/100, color = "grey40", family = "Raleway Medium", size = 10, hjust = 1, label.color = "transparent", fill = "transparent") +
  geom_curve(x = 2019.94, y = -12/100, xend = 2019.996, yend = -9.6/100, arrow = arrow(length = unit(0.03, "npc")), curvature = 0.2, size = 1, color = "grey30") +
  scale_x_continuous(breaks = c(2019, 2020)) +
  scale_y_continuous(labels = percent_format(accuracy = 1),
                     limits = c(-0.22, 0.24)) +
  scale_color_manual(values = c("grey80", "#1E6847")) +
  scale_alpha_manual(values = c(0.2, 1)) +
  scale_size_manual(values = c(0.3, 2)) +
  labs(title = "GDP % change, 2020 vs. 2019",
       x = "",
       y = "",
       fill = NULL,
       caption = "\n@segasi / Data: World Bank, Global Economic Prospects, January 2021") +
  theme_p +
  theme(plot.title = element_text(size = 42),
        plot.caption = element_text(size = 24),
        axis.text = element_text(size = 30),
        legend.text = element_text(size = 20),
        legend.position = "none") +
  ggsave("03_vis/impact/economy/1_c_gdp_change_2020.png", dpi = 200, width = 16, height = 12)



## Domestic and gender violence ----

# Investigations related to domestic violence ----
snsp %>% 
  filter(crime  == "Domestic violence") %>% 
  mutate(bar_color = ifelse(yr == 2020, "A", "B")) %>% 
  ggplot(aes(x = yr, y = number, fill = bar_color)) +
  geom_col() +
  geom_text(aes(label = comma(number)), color = "white", fontface = "bold", family = "Raleway Medium", vjust = 1.5, size = 10) + 
  scale_y_continuous(expand = c(0, 0)) +
  scale_fill_manual(values = c("salmon", "#1E6847")) +
  labs(title = "Criminal investigations related to <span style='color:#fa8072;'>domestic violence</span>",
       x = NULL,
       y = NULL,
       caption = "\n@segasi / Data: Mexico's SNSP.") +
  theme_p +
  theme(plot.title = element_markdown(size = 40),
        plot.caption = element_text(size = 22), 
        legend.position = "none",
        axis.text = element_text(size = 25),
        axis.text.y = element_blank(),
        panel.grid.major = element_blank()) +
  ggsave("03_vis/impact/domestic_gender_violence/01_investigtions_domestic_violence.png", width = 16, height = 9, dpi = 200)


# Investigations related to gender violence different than domestic violence ----
snsp %>% 
  filter(crime  == "Other expressions of gender violence") %>% 
  mutate(bar_color = ifelse(yr == 2020, "A", "B")) %>% 
  ggplot(aes(x = yr, y = number, fill = bar_color)) +
  geom_col() +
  geom_text(aes(label = comma(number)), color = "white", fontface = "bold", family = "Raleway Medium", vjust = 1.5, size = 10) + 
  scale_y_continuous(expand = c(0, 0)) +
  scale_fill_manual(values = c("salmon", "#1E6847")) +
  labs(title = "Criminal investigations related to <span style='color:#fa8072;'>gender violence</span><br>different than <span style='color:#fa8072;'>domestic violence</span>",
       x = NULL,
       y = NULL,
       caption = "\n@segasi / Data: Mexico's SNSP.") +
  theme_p +
  theme(plot.title = element_markdown(size = 40, lineheight = 1.2),
        plot.caption = element_text(size = 22),
        legend.position = "none",
        axis.text = element_text(size = 25),
        axis.text.y = element_blank(),
        panel.grid.major = element_blank()) +
  ggsave("03_vis/impact/domestic_gender_violence/02_investigations_gender_violence_different_than_domestic_violence.png", width = 16, height = 9, dpi = 200)

# Emergency calls related to violence against women ----
snsp %>% 
  filter(crime  == "Emergency calls related to violence against women") %>% 
  mutate(bar_color = ifelse(yr == 2020, "A", "B")) %>% 
  ggplot(aes(x = yr, y = number, fill = bar_color)) +
  geom_col() +
  geom_text(aes(label = comma(number)), color = "white", fontface = "bold", family = "Raleway Medium", vjust = 1.5, size = 10) + 
  scale_y_continuous(expand = c(0, 0)) +
  scale_fill_manual(values = c("salmon", "#1E6847")) +
  labs(title = "9-1-1 emergency calls related to <span style='color:#fa8072;'>violence</span> against <span style='color:#fa8072;'>women</span>",
       x = NULL,
       y = NULL,
       caption = "\n@segasi / Data: Mexico's SNSP.") +
  theme_p +
  theme(plot.title = element_markdown(size = 38, lineheight = 1.2),
        plot.caption = element_text(size = 22),
        legend.position = "none",
        axis.text = element_text(size = 25),
        axis.text.y = element_blank(),
        panel.grid.major = element_blank()) +
  ggsave("03_vis/impact/domestic_gender_violence/03_emergency_calls_violence_against_woman.png", width = 16, height = 9, dpi = 200)



# Emergency calls related to sexual harassment ----
snsp %>% 
  filter(crime  == "Emergency calls related to sexual harasment") %>% 
  mutate(bar_color = ifelse(yr == 2020, "A", "B")) %>% 
  ggplot(aes(x = yr, y = number, fill = bar_color)) +
  geom_col() +
  geom_text(aes(label = comma(number)), color = "white", fontface = "bold", family = "Raleway Medium", vjust = 1.5, size = 10) + 
  scale_y_continuous(expand = c(0, 0)) +
  scale_fill_manual(values = c("salmon", "#1E6847")) +
  labs(title = "9-1-1 emergency calls related to <span style='color:#fa8072;'>sexual harassment</span>",
       x = NULL,
       y = NULL,
       caption = "\n@segasi / Data: Mexico's SNSP.") +
  theme_p +
  theme(plot.title = element_markdown(size = 38, lineheight = 1.2),
        plot.caption = element_text(size = 22),
        legend.position = "none",
        axis.text = element_text(size = 25),
        axis.text.y = element_blank(),
        panel.grid.major = element_blank()) +
  ggsave("03_vis/impact/domestic_gender_violence/04_emergency_calls_sexual_harrasment.png", width = 16, height = 9, dpi = 200)


## Education ----

# Status of educational institutions in Latin America, 2020-2021, with shaded areas ----
school %>% 
  filter(country %in% c("Argentina", "Bolivia", "Brazil", "Chile", "Colombia", "Costa Rica", "Cuba", "Dom. Rep.", "Ecuador", "El Salvador", "Guatemala", "Haiti", "Honduras", "Paraguay", "Peru", "Mexico", "Nicaragua", "Panama", "Uruguay", "Venezuela"),
         date < "2021-03-01") %>%
  ggplot(aes(x = date, y = fct_rev(country), fill = status)) +
  geom_tile() +
  geom_vline(xintercept = seq.Date(from = as_date("2020-03-01"), to = as_date("2021-03-01"), by = "1 month"), color = "white") +
  scale_x_date(expand = c(0, 0), date_breaks = "1 month", date_labels = "%b") +
  scale_fill_manual(values = c("#66bd63", "#a6d96a", "salmon", "grey80")) +
  labs(title = "Status of educational institutions in Latin America, 2020-2021",
       subtitle = "The graph only considers countries with more than 1M people",
       x = "",
       y = "",
       fill = NULL,
       caption = str_c("\n@segasi / Data: UNESCO, last updated on ", Sys.Date())) +
  theme_p +
  theme(plot.title = element_text(size = 36),
        # plot.subtitle = element_markdown(size = 22),
        plot.caption = element_text(size = 18),
        axis.text = element_text(size = 20),
        axis.text.x = element_text(hjust = -0.7),
        legend.text = element_text(size = 20)) +
  ggsave("03_vis/impact/education/1_a_status_educational_institution.png", dpi = 200, width = 16, height = 12)

# Status of educational institutions in Latin America, 2020-2021, with shaded areas ----
school %>% 
  filter(country %in% c("Argentina", "Bolivia", "Brazil", "Chile", "Colombia", "Costa Rica", "Cuba", "Dom. Rep.", "Ecuador", "El Salvador", "Guatemala", "Haiti", "Honduras", "Paraguay", "Peru", "Mexico", "Nicaragua", "Panama", "Uruguay", "Venezuela"),
         date < "2021-03-01") %>% 
  ggplot(aes(x = date, y = fct_rev(country), fill = status)) +
  geom_tile() +
  geom_vline(xintercept = seq.Date(from = as_date("2020-03-01"), to = as_date("2021-03-01"), by = "1 month"), color = "white") +
  annotate(geom = "rect", xmin = as_date("2020-02-15"), xmax = as_date("2021-03-01"), ymin = 1.5, ymax = 4.5, fill = "white", alpha = 0.75) +
  annotate(geom = "rect", xmin = as_date("2020-02-15"), xmax = as_date("2021-03-01"), ymin = 5.5, ymax = 6.5, fill = "white", alpha = 0.75) +
  annotate(geom = "rect", xmin = as_date("2020-02-15"), xmax = as_date("2021-03-01"), ymin = 7.5, ymax = 10.5, fill = "white", alpha = 0.75) +
  annotate(geom = "rect", xmin = as_date("2020-02-15"), xmax = as_date("2021-03-01"), ymin = 11.5, ymax = 12.5, fill = "white", alpha = 0.75) +
  annotate(geom = "rect", xmin = as_date("2020-02-15"), xmax = as_date("2021-03-01"), ymin = 13.5, ymax = 17.5, fill = "white", alpha = 0.75) +
  annotate(geom = "rect", xmin = as_date("2020-02-15"), xmax = as_date("2021-03-01"), ymin = 18.5, ymax = 20.5, fill = "white", alpha = 0.75) +
  annotate(geom = "richtext", x = as_date("2021-02-20"), y = 8.9, label = "Only in <span style='color:#fa8072;'>six</span> of <span style='color:#fa8072;'>20</span> Latin American countries schools<br>have not opened -even partially- since April 2020.<br><br><span style='color:#fa8072;'>Mexico</span> is one of them.", family = "Raleway Medium", size = 8, color = "grey40", fill = "transparent", label.color = "transparent", hjust = 1, lineheight = 0.8) + 
  geom_curve(x = as_date("2020-11-19"), y = 7.9, xend = as_date("2020-11-05"), yend = 6.9, arrow = arrow(length = unit(0.02, "npc")), curvature = 0.2, size = 1, color = "grey40") +
  scale_x_date(expand = c(0, 0), date_breaks = "1 month", date_labels = "%b") +
  scale_fill_manual(values = c("#66bd63", "#a6d96a", "salmon", "grey80")) +
  labs(title = "Status of educational institutions in Latin America, 2020-2021",
       subtitle = "The graph only considers countries with more than 1M people",
       x = "",
       y = "",
       fill = NULL,
       caption = str_c("\n@segasi / Data: UNESCO, last updated on ", Sys.Date())) +
  theme_p +
  theme(plot.title = element_text(size = 36),
        # plot.subtitle = element_markdown(size = 22),
        plot.caption = element_text(size = 18),
        axis.text = element_text(size = 20),
        axis.text.x = element_text(hjust = -0.7),
        legend.text = element_text(size = 20)) +
  ggsave("03_vis/impact/education/1_b_status_educational_institution.png", dpi = 200, width = 16, height = 12)



### Tests ----

# Bottom-20 countries with the lowest total number of tests per thousand people ----
owid_covid %>% 
  filter(!is.na(total_tests_per_thousand), # Keep only rows with values for total_tests_per_thousand
         !str_detect(iso_code, "OWID")) %>% # Remove regional or world data
  group_by(location) %>% 
  filter(date == max(date), # Keep latest value available for each country
         date > Sys.Date() - 15) %>% # Remove countries if latest value is older than 15 days from day when chart was created
  ungroup() %>% 
  arrange(total_tests_per_thousand) %>% 
  select(date, location, total_tests_per_thousand)  %>% 
  slice_head(n = 20) %>% 
  mutate(mex_color = ifelse(location == "Mexico", "A", "B")) %>% 
  ggplot(aes(x = total_tests_per_thousand, 
             y = fct_reorder(location, total_tests_per_thousand),
             fill = mex_color)) +
  geom_col() +
  geom_text(aes(label = round(total_tests_per_thousand, 1)), color = "white", fontface = "bold", family = "Raleway Medium", hjust = 1.2, size = 7) + 
  scale_x_continuous(expand = c(0, 0),
                     limits = c(-0.1, 45)) +
  scale_fill_manual(values = c("salmon", "#1E6847")) +
  labs(title = "Bottom-20 countries with the <span style='color:#fa8072;'>lowest</span> total number of<br>tests per thousand people",
       x = NULL, 
       y = NULL,
       caption = str_c("\n@segasi / Data: Our World in Data.\nNumbers correspond to the latest available figure per country within the last 15 days. Last updated on ", Sys.Date(), ".")) +
  theme_p +
  theme(plot.title = element_markdown(),
        legend.position = "none",
        panel.grid.major = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_text(size = 20)) +
  ggsave("03_vis/potential_causes/tests/01_bottom_20_countries_tests_per_thounsand.png", width = 14, height = 14, dpi = 200)


# Total number of tests per thousand people in Latin America ----
owid_covid %>% 
  filter(str_detect(continent, "America"),
         !location %in% c("United States", "Canada"),
         !is.na(total_tests_per_thousand), # Keep only rows with values for total_tests_per_thousand
         !str_detect(iso_code, "OWID")) %>% # Remove regional or world data
  group_by(location) %>%
  filter(date == max(date)) %>% # Keep latest value available for each country
  ungroup() %>% 
  # filter(location == "Brazil") %>% 
  select(date, location, total_tests_per_thousand) %>% 
  slice_head(n = 20) %>% 
  mutate(label_big = ifelse(total_tests_per_thousand > 100, round(total_tests_per_thousand, 1), ""),
         label_small = ifelse(total_tests_per_thousand <= 100, round(total_tests_per_thousand, 1), ""),
         mex_color = ifelse(location == "Mexico", "A", "B"),
         location = ifelse(location == "Brazil", "Brazil*", location)) %>% 
  ggplot(aes(x = total_tests_per_thousand, 
             y = fct_reorder(str_wrap(location, width = 15), total_tests_per_thousand),
             fill = mex_color)) +
  geom_col() +
  geom_text(aes(label = label_big), color = "white", fontface = "bold", family = "Raleway Medium", hjust = 1.2, size = 8) + 
  geom_text(aes(label = label_small), color = "grey20", fontface = "bold", family = "Raleway Medium", hjust = -0.2, size = 8) + 
  scale_x_continuous(expand = c(0, 0),
                     limits = c(-0.1, 500)) +
  scale_fill_manual(values = c("salmon", "#1E6847")) +
  labs(title = "Total number of tests per thousand people in <span style='color:#fa8072;'>Latin America</span>",
       x = NULL, 
       y = NULL,
       caption = str_c("\n@segasi / Data: Our World in Data.\nNumbers correspond to the latest available figure per country. *Brazil's figures correspond to September of 2020. Last\nupdated on ", Sys.Date(), ".")) +
  theme_p +
  theme(plot.title = element_markdown(size = 33),
        plot.caption = element_text(size = 18),
        legend.position = "none",
        panel.grid.major = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_text(size = 22)) +
  ggsave("03_vis/potential_causes/tests/02_tests_per_thounsand_america.png", width = 14.5, height = 15, dpi = 200)


# Daily tests per thousand people (smoothed) ----
owid_covid %>% 
  ggplot(aes(x = date, 
             y = new_tests_smoothed_per_thousand ,
             group = location,
             color = mex_color,
             alpha = mex_color,
             size = mex_color)) +
  geom_line(color = "grey40", alpha = 0.7, size = 0.5) +
  scale_x_date(date_breaks = "1 month", 
               expand = c(0, 0),
               labels = c("Jan 2020", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec", "Jan 2021", "Feb", "", "")) +
  scale_y_log10(breaks = c(0.1, 1, 10, 100)) +
  labs(title = "Daily tests per thousand people (smoothed)",
       x = "",
       y = "Tests per thousand people (log 10)\n",
       caption = str_c("@segasi / Data: Our World in Data, last updated on ", Sys.Date())) +
  theme_p +
  theme(legend.position = "none") +
  ggsave("03_vis/potential_causes/tests/03_a_daily_tests_per_thounsand.png", width = 16, height = 9, dpi = 200)

# Daily tests per thousand people (smoothed), Mexico highlighted ----
owid_covid %>% 
  # glimpse()
  mutate(mex_color = ifelse(location == "Mexico", "A", "B"),
         location = fct_relevel(location, "Mexico", after = Inf)) %>% 
  ggplot(aes(x = date, 
             y = new_tests_smoothed_per_thousand ,
             group = location,
             color = mex_color,
             alpha = mex_color,
             size = mex_color)) +
  geom_line() +
  annotate(geom = "text", label = "Mexico", x = as_date("2021-01-30"), y = 0.44, color = "salmon", family = "Raleway Medium", size = 7) +
  scale_x_date(date_breaks = "1 month", 
               expand = c(0, 0),
               labels = c("Jan 2020", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec", "Jan 2021", "Feb", "", "")) +
  scale_y_log10(breaks = c(0.1, 1, 10, 100)) +
  scale_color_manual(values = c("salmon", "grey80")) +
  scale_alpha_manual(values = c(1, 0.2)) +
  scale_size_manual(values = c(1.5, 0.3)) +
  labs(title = "Daily tests per thousand people (smoothed)",
       x = "",
       y = "Tests per thousand people (log 10)\n",
       caption = str_c("@segasi / Data: Our World in Data, last updated on ", Sys.Date())) +
  theme_p +
  theme(legend.position = "none") +
  ggsave("03_vis/potential_causes/tests/03_b_daily_tests_per_thounsand_mex_highlight.png", width = 16, height = 9, dpi = 200)

### Positive rate ----

# Top-15 Countries with the Highest Positive rate, latest value available  ----
owid_covid %>% 
  filter(!is.na(positive_rate), # Keep only rows with values for positive_rate
         !str_detect(iso_code, "OWID")) %>% # Remove regional or world data
  group_by(location) %>% 
  filter(date == max(date), # Keep latest value available for each country
         date > Sys.Date() - 15) %>% # Remove countries if latest value is older than 15 days from day when chart was created
  ungroup() %>%   
  top_n(n = 15, wt = positive_rate) %>% 
  mutate(mex_color = ifelse(location == "Mexico", "A", "B"),
         location = ifelse(str_detect(location, "of Congo"), "Dem. Rep. of Congo", location)) %>% 
  ggplot(aes(x = positive_rate, 
             y = fct_reorder(location, positive_rate),
             fill = mex_color)) +
  geom_col() +
  geom_text(aes(label = positive_rate), color = "white", fontface = "bold", family = "Raleway Medium", hjust = 1.2, size = 7) +
  scale_x_continuous(expand = c(0, 0),
                     limits = c(-0.005, 0.32)) +
  scale_fill_manual(values = c("salmon", "#1E6847")) +
  labs(title = "Top-15 countries with the <span style='color:#fa8072;'>highest</span> share of COVID-19<br>tests that are positive",
       x = NULL, 
       y = NULL,
       caption = str_c("\n@segasi / Data: Our World in Data.\nNumbers correspond to the latest available figure per country within the last 15 days. Last updated on ", Sys.Date(), ".")) +
  theme_p +
  theme(plot.title = element_markdown(),
        legend.position = "none",
        panel.grid.major = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_text(size = 20)) +
  ggsave("03_vis/potential_causes/pos_rate/01_top_15_countries_positive_rate.png", width = 14, height = 12, dpi = 200)


# Trends of positive rate ----
owid_covid %>% 
  ggplot(aes(x = date, 
             y = positive_rate,
             group = location)) +
  geom_line(color = "#1E6847", alpha = 0.7, size = 0.5) +
  scale_x_date(date_breaks = "1 month", 
               expand = c(0, 0),
               labels = c("Jan 2020", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec", "Jan 2021", "Feb", "", "")) +
  scale_y_continuous(breaks = seq(0, 1, 0.1), 
                     limits = c(0, 0.68)) +
  labs(title = "Share of COVID-19 tests that are positive",
       x = "",
       y = "Positive rate  \n",
       caption = str_c("@segasi / Data: Our World in Data, last updated on ", Sys.Date())) +
  theme_p +
  theme(legend.position = "none") +
  ggsave("03_vis/potential_causes/pos_rate/02_a_positive_rate_historical.png", width = 16, height = 9, dpi = 200)


# Trends of positive rate, Mexico highlighted ----
owid_covid %>% 
  mutate(mex_color = ifelse(location == "Mexico", "A", "B"),
         location = fct_relevel(location, "Mexico", after = Inf)) %>% 
  ggplot(aes(x = date, 
             y = positive_rate,
             group = location, 
             color = mex_color,
             alpha = mex_color,
             size = mex_color)) +
  geom_line() +
  annotate(geom = "text", label = "Mexico", x = as_date("2021-01-30"), y = 0.44, color = "salmon", family = "Raleway Medium", size = 7) +
  scale_x_date(date_breaks = "1 month", 
               expand = c(0, 0),
               labels = c("Jan 2020", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec", "Jan 2021", "Feb", "", "")) +
  scale_y_continuous(breaks = seq(0, 1, 0.1), 
                     limits = c(0, 0.68)) +
  scale_color_manual(values = c("salmon", "#1E6847")) +
  scale_alpha_manual(values = c(1, 0.2)) +
  scale_size_manual(values = c(1, 0.3)) +
  labs(title = "Share of COVID-19 tests that are positive",
       x = "",
       y = "Positive rate  \n",
       caption = str_c("@segasi / Data: Our World in Data, last updated on ", Sys.Date())) +
  theme_p +
  theme(legend.position = "none") +
  ggsave("03_vis/potential_causes/pos_rate/02_b_positive_rate_historical_mex_highlighted.png", width = 16, height = 9, dpi = 200)


# Trends of positive rate, Mexico highlighted + legend ----

# Calculate how many days has Mexico been in the top-5 ranking of positive rate

# Using first as ties method
owid_covid %>% 
  # Remove regional or world data
  filter(!str_detect(iso_code, "OWID")) %>% 
  # Calculate ranking by date
  group_by(date) %>% 
  mutate(rank_positive = rank(-positive_rate, ties.method = "first")) %>% 
  ungroup() %>% 
  # Keep only observations after April 14th, 2020
  filter(date >= "2020-04-15",
         location == "Mexico") %>% 
  count(rank_positive) %>% 
  mutate(cum_n = cumsum(n), # Cummulative number of days 
         total = sum(n), # Total number of days
         prop = n/sum(n), # Proportion of days in each position
         cum_prop = cumsum(prop)) # Cummulative proportion

# Using average as ties method
owid_covid %>% 
  # Remove regional or world data
  filter(!str_detect(iso_code, "OWID")) %>% 
  # Calculate ranking by date
  group_by(date) %>% 
  mutate(rank_positive = rank(-positive_rate, ties.method = "average")) %>% 
  ungroup() %>% 
  # Keep only observations after April 14th, 2020
  filter(date >= "2020-04-15",
         location == "Mexico") %>% 
  count(rank_positive) %>% 
  mutate(cum_n = cumsum(n), # Cummulative number of days 
         total = sum(n), # Total number of days
         prop = n/sum(n), # Proportion of days in each position
         cum_prop = cumsum(prop)) # Cummulative proportion

# Graph
owid_covid %>% 
  mutate(mex_color = ifelse(location == "Mexico", "A", "B"),
         location = fct_relevel(location, "Mexico", after = Inf)) %>% 
  ggplot(aes(x = date, 
             y = positive_rate,
             group = location, 
             color = mex_color,
             alpha = mex_color,
             size = mex_color)) +
  geom_line() +
  annotate(geom = "text", label = "Mexico", x = as_date("2021-01-30"), y = 0.44, color = "salmon", family = "Raleway Medium", size = 7) +
  annotate(geom = "richtext", x = as_date("2020-11-01"), y = 0.6, label = "Mexico has been in the <span style='color:#fa8072;'>top-3</span> of countries<br>with the highest positive rate in <span style='color:#fa8072;'>93.4%</span> of the<br><span style='color:#fa8072;'>320</span> days since April 15th, 2020", family = "Raleway Medium", size = 8, color = "grey40", fill = "transparent", label.color = "transparent") +
  scale_x_date(date_breaks = "1 month", 
               expand = c(0, 0),
               labels = c("Jan 2020", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec", "Jan 2021", "Feb", "", "")) +
  scale_y_continuous(breaks = seq(0, 1, 0.1), 
                     limits = c(0, 0.68)) +
  scale_color_manual(values = c("salmon", "#1E6847")) +
  scale_alpha_manual(values = c(1, 0.2)) +
  scale_size_manual(values = c(1, 0.3)) +
  labs(title = "Share of COVID-19 tests that are positive",
       x = "",
       y = "Positive rate  \n",
       caption = str_c("@segasi / Data: Our World in Data, last updated on ", Sys.Date())) +
  theme_p +
  theme(legend.position = "none") +
  ggsave("03_vis/potential_causes/pos_rate/02_c_positive_rate_historical_mex_highlighted.png", width = 16, height = 9, dpi = 200)



# Trends of positive rate, Mexico highlighted + WHO range ----
owid_covid %>% 
  mutate(mex_color = ifelse(location == "Mexico", "A", "B"),
         location = fct_relevel(location, "Mexico", after = Inf)) %>% 
  ggplot(aes(x = date, 
             y = positive_rate,
             group = location, 
             color = mex_color,
             alpha = mex_color,
             size = mex_color)) +
  geom_line() +
  annotate(geom = "text", label = "Mexico", x = as_date("2021-01-30"), y = 0.44, color = "salmon", family = "Raleway Medium", size = 7) +
  annotate(geom = "rect", xmin = as_date("2020-01-01") + 5, xmax = Sys.Date() - 5, ymin = 0, ymax = 0.1, fill = "steelblue", color = "transparent", alpha = 0.4) +
  annotate(geom = "text", label = "WHO recommended range", x = as_date("2020-12-10"), y = 0.05, color = "grey30", family = "Raleway Medium", size = 7) +
  annotate(geom = "richtext", x = as_date("2020-11-01"), y = 0.6, label = "Mexico has been in the <span style='color:#fa8072;'>top-3</span> of countries<br>with the highest positive rate in <span style='color:#fa8072;'>93.4%</span> of the<br><span style='color:#fa8072;'>320</span> days since April 15th, 2020", family = "Raleway Medium", size = 8, color = "grey40", fill = "transparent", label.color = "transparent") +
  scale_x_date(date_breaks = "1 month", 
               expand = c(0, 0),
               labels = c("Jan 2020", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec", "Jan 2021", "Feb", "", "")) +
  scale_y_continuous(breaks = seq(0, 1, 0.1), 
                     limits = c(0, 0.68)) +
  scale_color_manual(values = c("salmon", "#1E6847")) +
  scale_alpha_manual(values = c(1, 0.2)) +
  scale_size_manual(values = c(1, 0.3)) +
  labs(title = "Share of COVID-19 tests that are positive",
       x = "",
       y = "Positive rate  \n",
       caption = str_c("@segasi / Data: Our World in Data, last updated on ", Sys.Date())) +
  theme_p +
  theme(legend.position = "none") +
  ggsave("03_vis/potential_causes/pos_rate/02_d_positive_rate_historical_mex_highlighted_who_range.png", width = 16, height = 9, dpi = 200)









### Fiscal measures ----

# Fiscal measures in response to the COVID-19 pandemic ----
imf %>% 
  # Exclude Antigua and Barbados because its value of eqlg_percent_gdp is 1121
  filter(eqlg_percent_gdp < 100) %>% 
  ggplot(aes(x = asfr_percent_gdp/100, y = eqlg_percent_gdp/100)) +
  geom_point(size = 5, shape = 16, color = "grey40", alpha = 0.4) +
  labs(title = "Fiscal measures in response to the COVID-19 pandemic, 2020", 
       subtitle = "As percentage of GDP\n",
       x = "\nAdditional spending and foregone revenues",
       y = "Equity, loans, and guarantees\n ",
       caption = str_c("\n@segasi / Data: IMF, as of December 31st, 2020.")) +
  scale_x_continuous(limits = c(0, 20.2/100), breaks = seq(0, 0.2, 0.025), labels = percent) +
  scale_y_continuous(limits = c(0, 42/100), labels = percent_format(accuracy = 1)) +
  theme_p +
  theme(plot.caption = element_text(size = 20),
        axis.text = element_text(size = 22),
        axis.title = element_text(size = 24),
        legend.position = "none") +
  ggsave("03_vis/potential_causes/fiscal_measures/01_a_fiscal_measures.png", width = 16, height = 9, dpi = 200)


# Fiscal measures in response to the COVID-19 pandemic, highlight Mexico ----
imf %>% 
  # Exclude Antigua and Barbados because its value of eqlg_percent_gdp is 1121
  filter(eqlg_percent_gdp < 100) %>% 
  # Create variable to later color Mexico's point
  mutate(mex_color = ifelse(country == "Mexico", "B", "A")) %>%  
  # Arrange rows so Mexico is the last observation. This is necesary in order to plot it over any other point
  arrange(mex_color) %>% 
  ggplot(aes(x = asfr_percent_gdp/100, y = eqlg_percent_gdp/100, color = mex_color, alpha = mex_color)) +
  geom_point(size = 5, shape = 16) +
  annotate(geom = "text", label = "Mexico", x = 0, y = 6/100, color = "#1E6847", family = "Raleway Medium", size = 7) +
  geom_curve(x = 0, y = 4.5/100, xend = 0.4/100, yend = 2/100, arrow = arrow(length = unit(0.03, "npc")), curvature = 0.2, size = 1, color = "grey30") +
  scale_x_continuous(limits = c(0, 20.2/100), breaks = seq(0, 0.2, 0.025), labels = percent) +
  scale_y_continuous(limits = c(0, 42/100), labels = percent_format(accuracy = 1)) +
  scale_alpha_manual(values = c(0.5, 1)) +
  scale_color_manual(values = c("grey80", "#1E6847")) +
  labs(title = "Fiscal measures in response to the COVID-19 pandemic, 2020", 
       subtitle = "As percentage of GDP\n",
       x = "\nAdditional spending and foregone revenues",
       y = "Equity, loans, and guarantees\n ",
       caption = str_c("\n@segasi / Data: IMF, as of December 31st, 2020.")) +
  theme_p +
  theme(plot.caption = element_text(size = 20),
        axis.text = element_text(size = 22),
        axis.title = element_text(size = 24),
        legend.position = "none") +
  ggsave("03_vis/potential_causes/fiscal_measures/01_b_fiscal_measures.png", width = 16, height = 9, dpi = 200)



# Fiscal measures in response to the COVID-19 pandemic, highlight Mexico vs. Advanced Economies ----
imf %>% 
  # Exclude Antigua and Barbados because its value of eqlg_percent_gdp is 1121
  filter(eqlg_percent_gdp < 100) %>% 
  # Create variable to later color Mexico's point
  mutate(point_colors = case_when(group_gral == "Advanced economies" ~ "B",
                                  country == "Mexico" ~ "Z", 
                                  TRUE ~ "A"),
         labels_points = ifelse(asfr_percent_gdp > 10 & group_gral == "Advanced economies"| eqlg_percent_gdp > 10 & group_gral == "Advanced economies", country, "")) %>%  
  # Arrange rows so Mexico is the last observation. This is necesary in order to plot it over any other point
  arrange(point_colors) %>%
  ggplot(aes(x = asfr_percent_gdp/100, y = eqlg_percent_gdp/100, color = point_colors, alpha = point_colors)) +
  geom_point(size = 5, shape = 16) +
  annotate(geom = "text", label = "Mexico", x = 0, y = 6/100, color = "#1E6847", family = "Raleway Medium", size = 7) +
  geom_text_repel(aes(label = labels_points), family = "Raleway Medium", size = 6, color = "grey30",box.padding = 0.5, max.overlaps = Inf) +
  annotate(geom = "richtext", x = 9/100, y = 42/100, label = "<span style='color:#1E6847;'>Mexico</span> vs. <span style='color:#fa8072;'>Advanced economies</span>", family = "Raleway Medium", size = 10, color = "grey30", hjust = 0, lineheight = 1, label.color = "transparent", fill = "transparent") +
  geom_curve(x = 0, y = 4.5/100, xend = 0.4/100, yend = 2/100, arrow = arrow(length = unit(0.03, "npc")), curvature = 0.2, size = 1, color = "grey30") +
  scale_x_continuous(limits = c(0, 20.2/100), breaks = seq(0, 0.2, 0.025), labels = percent) +
  scale_y_continuous(limits = c(0, 42/100), labels = percent_format(accuracy = 1)) +
  scale_alpha_manual(values = c(0.2, 0.6, 1)) +
  scale_color_manual(values = c("grey80", "salmon", "#1E6847")) +
  labs(title = "Fiscal measures in response to the COVID-19 pandemic, 2020", 
       subtitle = "As percentage of GDP\n",
       x = "\nAdditional spending and foregone revenues",
       y = "Equity, loans, and guarantees\n ",
       caption = str_c("\n@segasi / Data: IMF, as of December 31st, 2020.")) +
  theme_p +
  theme(plot.caption = element_text(size = 20),
        axis.text = element_text(size = 22),
        axis.title = element_text(size = 24),
        legend.position = "none") +
  ggsave("03_vis/potential_causes/fiscal_measures/01_c_fiscal_measures.png", width = 16, height = 9, dpi = 200)

# Fiscal measures in response to the COVID-19 pandemic, highlight Mexico vs. Emerging Economies ----
imf %>% 
  # Exclude Antigua and Barbados because its value of eqlg_percent_gdp is 1121
  filter(eqlg_percent_gdp < 100) %>% 
  # count(group_gral)
  # Create variable to later color Mexico's point
  mutate(point_colors = case_when(group_gral == "Emerging markets" & country != "Mexico"~ "B",
                                  country == "Mexico" ~ "Z", 
                                  TRUE ~ "A"),
         labels_points = ifelse(asfr_percent_gdp > 7 & group_gral == "Emerging markets" | eqlg_percent_gdp > 8 & group_gral == "Emerging markets", country, "")) %>%  
  # Arrange rows so Mexico is the last observation. This is necesary in order to plot it over any other point
  arrange(point_colors) %>%
  ggplot(aes(x = asfr_percent_gdp/100, y = eqlg_percent_gdp/100, color = point_colors, alpha = point_colors)) +
  geom_point(size = 5, shape = 16) +
  geom_text_repel(aes(label = labels_points), family = "Raleway Medium", size = 6, color = "grey30",box.padding = 0.5, max.overlaps = Inf, seed = 1) +
  annotate(geom = "text", label = "Mexico", x = 0, y = 6/100, color = "#1E6847", family = "Raleway Medium", size = 7) +
  annotate(geom = "richtext", x = 9/100, y = 42/100, label = "<span style='color:#1E6847;'>Mexico</span> vs. <span style='color:#fa8072;'>Emerging economies</span>", family = "Raleway Medium", size = 10, color = "grey30", hjust = 0, lineheight = 1, label.color = "transparent", fill = "transparent") +
  geom_curve(x = 0, y = 4.5/100, xend = 0.4/100, yend = 2/100, arrow = arrow(length = unit(0.03, "npc")), curvature = 0.2, size = 1, color = "grey30") +
  scale_x_continuous(limits = c(0, 20.2/100), breaks = seq(0, 0.2, 0.025), labels = percent) +
  scale_y_continuous(limits = c(0, 42/100), labels = percent_format(accuracy = 1)) +
  scale_alpha_manual(values = c(0.3, 0.6, 1)) +
  scale_color_manual(values = c("grey80", "salmon", "#1E6847")) +
  labs(title = "Fiscal measures in response to the COVID-19 pandemic, 2020", 
       subtitle = "As percentage of GDP\n",
       x = "\nAdditional spending and foregone revenues",
       y = "Equity, loans, and guarantees\n ",
       caption = str_c("\n@segasi / Data: IMF, as of December 31st, 2020.")) +
  theme_p +
  theme(plot.caption = element_text(size = 20),
        axis.text = element_text(size = 22),
        axis.title = element_text(size = 24),
        legend.position = "none") +
  ggsave("03_vis/potential_causes/fiscal_measures/01_d_fiscal_measures.png", width = 16, height = 9, dpi = 200)


# Fiscal measures in response to the COVID-19 pandemic, highlight Mexico vs. Selected Low-Income Developing Countries ----
imf %>% 
  # Exclude Antigua and Barbados because its value of eqlg_percent_gdp is 1121
  filter(eqlg_percent_gdp < 100) %>% 
  # count(group_gral)
  # Create variable to later color Mexico's point
  mutate(point_colors = case_when(group_gral == "Selected Low-Income Developing Countries" ~ "B",
                                  country == "Mexico" ~ "Z", 
                                  TRUE ~ "A")) %>%  
  # Arrange rows so Mexico is the last observation. This is necesary in order to plot it over any other point
  arrange(point_colors) %>%
  ggplot(aes(x = asfr_percent_gdp/100, y = eqlg_percent_gdp/100, color = point_colors, alpha = point_colors)) +
  geom_point(size = 5, shape = 16) +
  annotate(geom = "text", label = "Mexico", x = 0, y = 6/100, color = "#1E6847", family = "Raleway Medium", size = 7) +
  annotate(geom = "richtext", x = 9/100, y = 42/100, label = "<span style='color:#1E6847;'>Mexico</span> vs. <span style='color:#fa8072;'>Low-Income Dev. Countries</span>", family = "Raleway Medium", size = 10, color = "grey30", hjust = 0, lineheight = 1, label.color = "transparent", fill = "transparent") +
  geom_curve(x = 0, y = 4.5/100, xend = 0.4/100, yend = 2/100, arrow = arrow(length = unit(0.03, "npc")), curvature = 0.2, size = 1, color = "grey30") +
  scale_x_continuous(limits = c(0, 20.2/100), breaks = seq(0, 0.2, 0.025), labels = percent) +
  scale_y_continuous(limits = c(0, 42/100), labels = percent_format(accuracy = 1)) +
  scale_alpha_manual(values = c(0.3, 0.6, 1)) +
  scale_color_manual(values = c("grey80", "salmon", "#1E6847")) +
  labs(title = "Fiscal measures in response to the COVID-19 pandemic, 2020", 
       subtitle = "As percentage of GDP\n",
       x = "\nAdditional spending and foregone revenues",
       y = "Equity, loans, and guarantees\n ",
       caption = str_c("\n@segasi / Data: IMF, as of December 31st, 2020.")) +
  theme_p +
  theme(plot.caption = element_text(size = 20),
        axis.text = element_text(size = 22),
        axis.title = element_text(size = 24),
        legend.position = "none") +
  ggsave("03_vis/potential_causes/fiscal_measures/01_e_fiscal_measures.png", width = 16, height = 9, dpi = 200)




### Other measures (ECLAC) ----

# Number of measures adopted by Latin America's 10 largest economies during the COVID-19 pandemic, by topic ----

cepal_by_topic %>% 
  filter(pais %in% c("BRA", "MEX", "ARG", "COL", "CHL", 
                     "PER", "DOM", "ECU", "GTM", "PAN")) %>%
  group_by(pais) %>% 
  mutate(total = sum(num_acciones)) %>% 
  ungroup() %>% 
  mutate(pais = case_when(pais == "BRA" ~ "Brazil",
                          pais == "MEX" ~ "Mexico",
                          pais == "ARG" ~ "Argentina",
                          pais == "COL" ~ "Colombia",
                          pais == "CHL" ~ "Chile",
                          pais == "PER" ~ "Peru",
                          pais == "DOM" ~ "Dom. Rep.",
                          pais == "ECU" ~ "Ecuador",
                          pais == "GTM" ~ "Guatemala",
                          pais == "PAN" ~ "Panama"),
         etiqueta = str_c(pais, "\n(", total, ")")) %>% 
  ggplot(aes(x = fct_reorder(etiqueta, total), 
             y = fct_rev(str_wrap(tema, width = 6)), fill = num_acciones)) +
  geom_tile(color = "grey40") +
  geom_text(aes(label = num_acciones), size = 7) +
  scale_fill_gradient(low = "white", high = "#1E6847") +
  labs(title = str_wrap("Number of measures adopted by Latin America's 10 largest economies during the COVID-19 pandemic, by topic", width = 58),
       x = "",
       y = NULL,
       caption = "\n@segasi / Data: ECLAC") +
  theme_p +
  theme(plot.title = element_text(size = 37),
        plot.caption = element_text(size = 18),
        axis.text.x = element_text(size = 18),
        axis.text.y = element_text(size = 20),
        # axis.text.y = element_text(size = 25),
        legend.position = "none") +
  ggsave("03_vis/potential_causes/general_measures/01_mesures_vs_pandemic.png", width = 16, heigh = 10, dpi = 200) 
