rm(list=ls())
setwd("") 

# load packages
suppressMessages(library(ggplot2))
suppressMessages(library(dplyr))
#install.packages("maps")
suppressMessages(library(maps))

# load map and data
world <- map_data("world")

df <- read.table("input.txt", header = TRUE, sep = "\t")
df$ecosystem <- factor(df$ecosystem, levels = c(
  "Acid Mine Drainage", "Activated sludge systems", "Agricultural Land",
  "Artificial Surfaces", "Bare Land", "Coldseeps", "Estuary", "Forest", "Freshwater lake",
  "Glacier", "Grassland",
  "Hadal Trench", "Hotspring", "Hydrothermal Vent", "Mangrove",
  "Oil reserver", "Permafrost", "River", "Saline lake", "Seamount", "Shrubland",
  "Tundra", "Wetland", "Groundwater"
))

#
ecosystem_colors <- c(
  "Acid Mine Drainage" = "#E60012",
  "Activated sludge systems" = "#6B8E23",
  "Agricultural Land" = "#2F4F4F",
  "Artificial Surfaces" = "#A9A9A9",
  "Bare Land" = "#D2B48C",
  "Coldseeps" = "#5F9EA0",
  "Estuary" = "#4682B4",
  "Forest" = "#228B22",
  "Freshwater lake" = "#00BFFF",
  "Glacier" = "#2646E4",
  "Grassland" = "#ADFF2F",
  "Groundwater" = "#704da8",
  "Hadal Trench" = "#191970",
  "Hotspring" = "#f8766d",
  "Hydrothermal_vent" = "#800000",
  "Mangrove" = "#556B2F",
  "Oil reserver" = "#3B3B3B",
  "Permafrost" = "#9C585D",
  "River" = "#1E90FF",
  "Saline lake" = "#F06F25",
  "Seamount" = "#2E8B57",
  "Shrubland" = "#C676FE",
  "Tundra" = "#DCDCDC",
  "Wetland" = "#66CDAA"
)


# 
###3
p_map <- ggplot() +
  geom_map(data = world, map = world,
           aes(map_id = region),
           fill = "#F7F7F7", color = NA, linewidth = 0) +
  geom_point(data = df, aes(x = as.numeric(long), y = as.numeric(lat), color = ecosystem),
             size = 0.2, alpha = 1) +
  scale_color_manual(values = ecosystem_colors) +
  coord_fixed(1.3) +  # 
  theme_bw() +
  theme(
    panel.background = element_rect(fill = '#C7E6F5'),
    panel.grid = element_blank(), 
    axis.text = element_text(size = 10),  # 
    axis.ticks = element_line(color = "black", linewidth = 0.2),
    axis.title = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.3),
    legend.title = element_blank(),
    legend.text = element_text(size = 12),
    legend.position = "none"
  )
p_map
ggsave("Global_sampling_sites.pdf", dpi=1200, plot=p_map, width=10, height=10)

