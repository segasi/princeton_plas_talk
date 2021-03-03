### Packages ----
library(pacman)
p_load(cowplot, extrafont,  fishualize, gganimate, GGally, ggforce, ggmap, ggrepel, gifski, glue, ggtext, Hmisc, janitor, lubridate, RColorBrewer, readxl, rworldmap, rworldxtra, tidyverse, scales, sf, sysfonts, treemapify, viridis, wesanderson, zoo)

### General setup ----
Sys.setlocale("LC_ALL", "en_US.UTF-8") 
options(scipen = 9999)

### Define graphics theme ----

# Load fonts
font_add_google(name = "Poppins", "poppins")
# font_add_google(name = "Raleway Medium", "Raleway Medium")

# Define theme
theme_p <-  
  theme_minimal() +
  theme(text = element_text(family = "Raleway Medium", color = "grey35"),
        plot.background = element_rect(color = "white", fill = "white"),
        plot.title = element_text(size = 35, face = "bold", margin = margin(10,0,20,0), family = "Poppins", color = "grey25"),
        plot.title.position = "plot",
        plot.subtitle = element_text(size = 26, face = "bold", colour = "#666666", margin = margin(0, 0, 20, 0), family = "Raleway Medium"),
        plot.caption = element_text(hjust = 0, size = 15),
        plot.caption.position = "plot",
        panel.grid = element_line(linetype = 3, color = "grey90"), 
        panel.grid.minor = element_blank(),
        legend.position = "bottom",
        legend.title = element_text(size = 16, face = "bold", family = "Raleway"),
        legend.text = element_text(size = 14, family = "Raleway Medium"),
        legend.title.align = 0.5,
        axis.title = element_text(size = 18, hjust = 1, face = "bold", margin = margin(0,0,0,0), family = "Raleway"),
        axis.text = element_text(size = 16, face = "bold", family = "Raleway Medium"),
        strip.background = element_rect(fill = "grey70", color  = "grey70"),
        strip.text = element_text(color = "white", size = 14))


