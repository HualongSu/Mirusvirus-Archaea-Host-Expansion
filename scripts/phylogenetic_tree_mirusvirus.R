#
rm(list=ls())
setwd("")
###
library(tidyverse)
library(ggtree)
library(ggtreeExtra)
library(ggnewscale)
library(ape)
###
##
tree <- read.tree("trimal_gt0.1_MCP_mirus_mafft_fftnsi.phy.treefile")
info <- read.csv("tree_information.csv")

rooted_tree <- midpoint(tree)
#
clade <- info %>% select(Genomes,Clade) %>% 
  rename("label"="Genomes",
         "clade"="Clade")
#
tree_new1 <- full_join(rooted_tree, clade, by = "label")
##
library(ggtree)
library(ggplot2)
library(ggnewscale)
p <- ggtree(tree_new1, aes(color = clade), layout = 'fan', right = TRUE, open.angle = 180) +
  geom_aline(
    linetype = 'longdash',
    color = "#D1D1D1",
    linewidth = 0.25,
    size = 0.025,
    show.legend = TRUE
  ) +
  scale_color_manual(
    name = "Phylum",
    guide = "none",
    values = c(
      "A01" = "#f4f6e9", "A02" = "#d1e8e2", "A03" = "#a8e0db", "A04" = "#7cd6cc", "A05" = "#4bc9c2",
      "A06" = "#27b7b0", "A07" = "#1a9d9b", "A08" = "#0b7473", "A09" = "#005f5c", "E01" = "#ffed8a",
      "E02" = "#ffcb5c", "E03" = "#ff9f24", "E04" = "#ff6f00", "E05" = "#d05400", "M01&M03" = "#C2C0E7",
      "M01&M04" = "#7DA3FF", "M02" = "#4C88FF", "M05" = "#2B6EDD", "M06&M07" = "#1A4FBB",
      "T01" = "#f3a1a1", "T02" = "#e05b5b", "T03" = "#d43d3d"
    )
  ) +
  new_scale_fill() +
  new_scale_color()

print(p)
##
library(ggtree)
library(ggplot2)
library(ggnewscale)
library(ggtreeExtra)
library(dplyr)

# 
clade1 <- clade %>% 
  rename("clade_1" = "clade")

p1 <- p +
  geom_fruit(
    data = clade1,
    geom = geom_bar,
    stat = 'identity',
    width = 1,
    aes(y = label, x = 0.5, fill = clade_1),
    pwidth = 0.2 #1.2
  ) +
  scale_fill_manual(
    name = "Clade",
    guide = "none",
    values = c(
      "A01" = "#f4f6e9", "A02" = "#d1e8e2", "A03" = "#a8e0db", "A04" = "#7cd6cc", "A05" = "#4bc9c2",
      "A06" = "#27b7b0", "A07" = "#1a9d9b", "A08" = "#0b7473", "A09" = "#005f5c", "E01" = "#ffed8a",
      "E02" = "#ffcb5c", "E03" = "#ff9f24", "E04" = "#ff6f00", "E05" = "#d05400", "M01&M03" = "#C2C0E7",
      "M01&M04" = "#7DA3FF", "M02" = "#4C88FF", "M05" = "#2B6EDD", "M06&M07" = "#1A4FBB",
      "T01" = "#f3a1a1", "T02" = "#e05b5b", "T03" = "#d43d3d"
    )
  ) +
  new_scale_fill() +
  new_scale_color()

#
print(p1)

##
# 
p2 <- p1 + 
  geom_fruit(
    data = clade1,
    geom = geom_text,
    mapping = aes(y = label, x = 0, label = label),
    size = 1, 
    color = "black",  # 
    hjust = 0.5, #
    offset = 1     
  )

# 
print(p2)


##
info2 <- info %>%
  select(Genomes, Type)
info2 <- info2 %>%
  mutate(
    type = case_when(
      grepl("^Reference", Type) ~ "Reference",
      grepl("^This study", Type) ~ "This study"
    ),
    value = case_when(
      type == "Reference" ~ 0.5,
      type == "This study" ~ 10
    )
  )

#

p4 <- p2 + 
  geom_fruit(
    data = info2,
    geom = geom_point,
    mapping = aes(y = Genomes, x = value, shape = type, fill = type),
    size = 3,
    color = "black",     # 
    offset = 0.25,       # 
    pwidth = 0.3         # 
  ) + scale_fill_manual(values = c(
    "Reference" = "#AC91A1",
    "This study" = "#C5CDC2"
    
  ),
  guide = "none"
  ) + 
  new_scale_fill()


p4

###
p1

###




# 
p5 <- p1 + 
  geom_fruit(
    data = info2,
    geom = geom_tile,       # 
    stat = "identity",
    mapping = aes(y = Genomes, x = 1, fill = type),  # 
    pwidth = 0.25,          # 
    offset = -0.15          # 
  ) +
  scale_fill_manual(values = c(
    "Reference" = "#8EBAD6",
    "This study" = "#F6c6cb"
  ),
  name = "Genome source", # 
  guide = "none" # 
  ) +
  new_scale_fill()

p5

##
p6 <- p5 + 
  geom_fruit(
    data = info1,
    geom = geom_tile,       # 
    stat = "identity",
    mapping = aes(y = Genomes, x = 1, fill = Source),  # 
    pwidth = 0.25,          # 
    offset = -0.053          # 
  ) +
  scale_fill_manual(values = c(
    "Agricultural Land" = "#2F4F4F",
    "Artificial Surfaces" = "#A9A9A9",
    "Deep sea" = "#212F54", "Estuary" = "#4682B4", "Eukaryotic assemblies" = "#EE7C70",
    "Forest" = "#228B22", "Freshwater" = "#00BFFF", "Glacier" = "#2646E4",
    "Grassland" = "#ADFF2F", "Groundwater" = "#704da8", "Surface sea" = "#93D5DC",
    "Wetland" = "#66CDAA"
    
    
  ),
  name = "Ecosystem types",
  guide="none"
  ) +
  new_scale_fill()

p6


#
ggsave("mcp_tree_final_length_no1.pdf",p6,dpi=800,height = 10, width = 10)
ggsave("mcp_tree_final_length_no2.pdf",p6,dpi=800,height = 5, width = 5)
ggsave("mcp_tree_final_length_no3.pdf",p6,dpi=800,height = 10, width = 5)
ggsave("mcp_tree_final_length_no4.pdf",p6,dpi=800,height = 15, width = 7.5)
##


