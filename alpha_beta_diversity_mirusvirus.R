rm(list=ls())#clear Global Environment
setwd("")
##
library(tidyverse)
da <- read.table("mirusviruses_abundance.txt", header = T, sep = "\t", row.names = 1)
data_group = read.delim("group.txt", row.names = 1)
da %>%  head()

# 
otu <- da %>%
  as.data.frame()

otu
#
#install.packages("microeco")
library(microeco)
data <- microtable$new(otu_table = otu,
                       sample_table = data_group)
data$otu_table
#install.packages("remotes")    #  install.packages("devtools")
#remotes::install_github("ChiLiubio/mecodev")
library(mecodev)
colSums(data$otu_table) # 
max(colSums(data$otu_table)) #
min(colSums(data$otu_table)) #
set.seed(12345)
t1 <- trans_rarefy$new(
  data,
  alphadiv = "Shannon",
  depth   = c(0, 10, 50, 500, seq(1000, 7500, 500))
)


t1$res_rarefy
##
t1$plot_rarefy(color = "group", color_values = rainbow(12), show_point = TRUE)

##
t2 <- trans_rarefy$new(
  data,
  alphadiv = "Simpson",
  depth   = c(0, 10, 50, 500, seq(1000, 7500, 500))
)

t2$res_rarefy
##
t2$plot_rarefy(color = "group", color_values = rainbow(12), show_point = TRUE)

##
library(vegan)

# 
depths <- colSums(otu)

# 
keep <- depths >= 2000
cat("samples：", sum(keep), "/", length(depths), "\n")
otu_filt <- otu[, keep]

# 
set.seed(123)
otu_nom <- data.frame(
  t(rrarefy(t(otu_filt), sample = 2000))
)

#
min(colSums(otu_nom))
##
# 
write.csv(
  otu_filt,
  file = "otu_filt.csv",
  row.names = FALSE
)
data_group = read.delim("group3.txt", row.names = 1)
## 
data <- microtable$new(otu_table = otu_nom,                       
                       sample_table = data_group)
data$otu_table
#data$cal_alphadiv(measures = NULL, PD = FALSE)
a1 <- trans_alpha$new(dataset = data)
alpha <- a1$data_alpha %>%  
  pivot_wider(names_from = Measure,              
              values_from = Value)


#Shannon
library(ggplot2)
library(ggsignif)
col <- c("#367DB0", "#3D9F3C")
scale_color_manual(values = col)

p1 <- ggplot(alpha, aes(x = group, y = Shannon, color = group)) +
  geom_violin(fill = NA, scale = "width", linewidth = 0.8, alpha = 0.7) +
  geom_jitter(size = 1, alpha = 0.7, width = 0.2) +
  stat_summary(fun = median, geom = "point", shape = 3, size = 3, colour = "black") + 
  scale_color_manual(values = col) +
  scale_y_continuous(limits = c(0, 5), breaks = seq(0, 5, 1)) +
  geom_signif(
    comparisons       = list(c("Aquatic", "Terrestrial")),
    test              = wilcox.test,
    map_signif_level  = TRUE,
    y_position        = 4.5,
    tip_length        = 0,
    vjust             = 0.2,
    colour            = "black",   
  ) +
  labs(x = NULL, y = "Shannon diversity", color = NULL) +
  theme_bw() +
  theme(
    axis.text         = element_text(size = 14, color = "black"),
    axis.title.y      = element_text(size = 14, angle = 90),
    panel.grid        = element_blank(),
    panel.border      = element_rect(color = "black", size = 1),
    panel.background  = element_rect(fill = "white", colour = NA),
    plot.title        = element_text(hjust = 0.5),
    legend.position   = "none"   
  )

print(p1)




#Simpsons

library(ggplot2)
library(ggsignif)
col <- c("#367DB0", "#3D9F3C")
scale_color_manual(values = col)
#
max(alpha$Simpson)
min(alpha$Simpson)
p2 <- ggplot(alpha, aes(x = group, y = Simpson, color = group)) +
  geom_violin(fill = NA, scale = "width", linewidth = 0.8, alpha = 0.7) +
  geom_jitter(size = 1, alpha = 0.7, width = 0.2) +
  stat_summary(fun = median, geom = "point", shape = 3, size = 3, colour = "black") +
  scale_color_manual(values = col) +
  scale_y_continuous(limits = c(0, 1.2), breaks = seq(0, 1.2, 0.2)) +
  geom_signif(
    comparisons       = list(c("Aquatic", "Terrestrial")),
    test              = wilcox.test,
    map_signif_level  = TRUE,
    y_position        = 1.1,
    tip_length        = 0,
    vjust             = 0.2,
    colour            = "black",    # 
  ) +
  labs(x = NULL, y = "Simpson", color = NULL) +
  theme_bw() +
  theme(
    axis.text         = element_text(size = 14, color = "black"),
    axis.title.y      = element_text(size = 14, angle = 90),
    panel.grid        = element_blank(),
    panel.border      = element_rect(color = "black", size = 1),
    panel.background  = element_rect(fill = "white", colour = NA),
    plot.title        = element_text(hjust = 0.5),
    legend.position   = "none"     # 
  )

print(p2)


##Pielou
max(alpha$Pielou)
min(alpha$Pielou)
p3 <- ggplot(alpha, aes(x = group, y = Pielou, color = group)) +
  geom_violin(fill = NA, scale = "width", linewidth = 0.8, alpha = 0.7) +
  geom_jitter(size = 1, alpha = 0.7, width = 0.2) +
  stat_summary(fun = median, geom = "point", shape = 3, size = 3, colour = "black") +
  scale_color_manual(values = col) +
  scale_y_continuous(limits = c(0, 1.2), breaks = seq(0, 1.2, 0.2)) +
  geom_signif(
    comparisons       = list(c("Aquatic", "Terrestrial")),
    test              = wilcox.test,
    map_signif_level  = TRUE,
    y_position        = 1.1,
    tip_length        = 0,
    vjust             = 0.2,
    colour            = "black",    #
  ) +
  labs(x = NULL, y = "Pielou", color = NULL) +
  theme_bw() +
  theme(
    axis.text         = element_text(size = 14, color = "black"),
    axis.title.y      = element_text(size = 14, angle = 90),
    panel.grid        = element_blank(),
    panel.border      = element_rect(color = "black", size = 1),
    panel.background  = element_rect(fill = "white", colour = NA),
    plot.title        = element_text(hjust = 0.5),
    legend.position   = "none"     # 
  )

print(p3)


##Fisher
max(alpha$Fisher)
min(alpha$Fisher)
p3.1 <- ggplot(alpha, aes(x = group, y = Fisher, color = group)) +
  geom_violin(fill = NA, scale = "width", linewidth = 0.8, alpha = 0.7) +
  geom_jitter(size = 1, alpha = 0.7, width = 0.2) +
  stat_summary(fun = median, geom = "point", shape = 3, size = 3, colour = "black") + 
  scale_color_manual(values = col) +
  scale_y_continuous(limits = c(0, 80), breaks = seq(0, 80, 10)) +
  geom_signif(
    comparisons       = list(c("Aquatic", "Terrestrial")),
    test              = wilcox.test,
    map_signif_level  = TRUE,
    y_position        = 75,
    tip_length        = 0,
    vjust             = 0.2,
    colour            = "black",    #
  ) +
  labs(x = NULL, y = "Fisher", color = NULL) +
  theme_bw() +
  theme(
    axis.text         = element_text(size = 14, color = "black"),
    axis.title.y      = element_text(size = 14, angle = 90),
    panel.grid        = element_blank(),
    panel.border      = element_rect(color = "black", size = 1),
    panel.background  = element_rect(fill = "white", colour = NA),
    plot.title        = element_text(hjust = 0.5),
    legend.position   = "none"     # 
  )

print(p3.1)
###Chao1
max(alpha$Chao1)
min(alpha$Chao1)
p4 <- ggplot(alpha, aes(x = group, y = Chao1, color = group)) +
  geom_violin(fill = NA, scale = "width", linewidth = 0.8, alpha = 0.7) +
  geom_jitter(size = 1, alpha = 0.7, width = 0.2) +
  stat_summary(fun = median, geom = "point", shape = 3, size = 3, colour = "black") +
  scale_color_manual(values = col) +
  scale_y_continuous(limits = c(0, 400), breaks = seq(0, 400, 50)) +
  geom_signif(
    comparisons       = list(c("Aquatic", "Terrestrial")),
    test              = wilcox.test,
    map_signif_level  = TRUE,
    y_position        = 375,
    tip_length        = 0,
    vjust             = 0.2,
    colour            = "black",    # 
  ) +
  labs(x = NULL, y = "Chao1", color = NULL) +
  theme_bw() +
  theme(
    axis.text         = element_text(size = 14, color = "black"),
    axis.title.y      = element_text(size = 14, angle = 90),
    panel.grid        = element_blank(),
    panel.border      = element_rect(color = "black", size = 1),
    panel.background  = element_rect(fill = "white", colour = NA),
    plot.title        = element_text(hjust = 0.5),
    legend.position   = "none"     # 
  )

print(p4)


###Richness
max(alpha$Observed)
min(alpha$Observed)
p5 <- ggplot(alpha, aes(x = group, y = Observed, color = group)) +
  geom_violin(fill = NA, scale = "width", linewidth = 0.8, alpha = 0.7) +
  geom_jitter(size = 1, alpha = 0.7, width = 0.2) +
  stat_summary(fun = median, geom = "point", shape = 3, size = 3, colour = "black") +
  scale_color_manual(values = col) +
  scale_y_continuous(limits = c(0, 270), breaks = seq(0, 270, 50)) +
  geom_signif(
    comparisons       = list(c("Aquatic", "Terrestrial")),
    test              = wilcox.test,
    map_signif_level  = TRUE,
    y_position        = 250,
    tip_length        = 0,
    vjust             = 0.2,
    colour            = "black",    
  ) +
  labs(x = NULL, y = "Richness", color = NULL) +
  theme_bw() +
  theme(
    axis.text         = element_text(size = 14, color = "black"),
    axis.title.y      = element_text(size = 14, angle = 90),
    panel.grid        = element_blank(),
    panel.border      = element_rect(color = "black", size = 1),
    panel.background  = element_rect(fill = "white", colour = NA),
    plot.title        = element_text(hjust = 0.5),
    legend.position   = "none"     
  )

print(p5)

##ACE

max(alpha$InvSimpson)
min(alpha$InvSimpson)
p6 <- ggplot(alpha, aes(x = group, y = InvSimpson, color = group)) +
  geom_violin(fill = NA, scale = "width", linewidth = 0.8, alpha = 0.7) +
  geom_jitter(size = 1, alpha = 0.7, width = 0.2) +
  stat_summary(fun = median, geom = "point", shape = 3, size = 3, colour = "black") +
  scale_color_manual(values = col) +
  scale_y_continuous(limits = c(0, 60), breaks = seq(0, 60, 10)) +
  geom_signif(
    comparisons       = list(c("Aquatic", "Terrestrial")),
    test              = wilcox.test,
    map_signif_level  = TRUE,
    y_position        = 55,
    tip_length        = 0,
    vjust             = 0.2,
    colour            = "black",   
  ) +
  labs(x = NULL, y = "InvSimpson", color = NULL) +
  theme_bw() +
  theme(
    axis.text         = element_text(size = 14, color = "black"),
    axis.title.y      = element_text(size = 14, angle = 90),
    panel.grid        = element_blank(),
    panel.border      = element_rect(color = "black", size = 1),
    panel.background  = element_rect(fill = "white", colour = NA),
    plot.title        = element_text(hjust = 0.5),
    legend.position   = "none"  
  )

print(p6)


# install.packages("patchwork")
library(patchwork)

#
combined <- (p1 + p2 + p3.1) /
  (p4 + p5 + p6)

# 
print(combined)

ggsave("alpha_diver_combined_plot.pdf", plot = combined, width = 8, height = 8, dpi = 1200)

###
data_pd <- read.table("", header = T, sep = "\t", row.names = 1)
group_pd = read.delim("", row.names = 1)
library(ape)
tree_pd <- read.tree('trimal_gt0.1_mirus_mafft.phy.treefile')

#
data_pd <- t(data_pd)
#install.packages("picante")
library(picante)
pd_whole_tree <- pd(data_pd, tree_pd, include.root = FALSE)
pd_whole_tree
##
pd_clean <- pd_whole_tree[!is.na(pd_whole_tree$PD), ]
pd_clean
##
pd_clean$sample <- row.names(pd_clean)
group_pd$sample <- rownames(group_pd)
pd_clean1 <- merge(pd_clean, group_pd, by = "sample")
##
max(pd_clean1$PD)
min(pd_clean1$PD)
col=c("#367DB0", "#3D9F3C")
p_pd <- ggplot(pd_clean1, aes(x = group, y = PD, color = group)) +
  geom_violin(fill = NA, scale = "width", linewidth = 0.8, alpha = 0.7) +
  geom_jitter(size = 1, alpha = 0.7, width = 0.2) +
  stat_summary(fun = median, geom = "point", shape = 3, size = 3, colour = "black") +
  scale_color_manual(values = col) +
  scale_y_continuous(limits = c(0, 4.5), breaks = seq(0, 4.5, 1)) +
  geom_signif(
    comparisons       = list(c("Aquatic", "Terrestrial")),
    test              = wilcox.test,
    map_signif_level  = TRUE,
    y_position        = 4,
    tip_length        = 0,
    vjust             = 0.2,
    colour            = "black",    # 
  ) +
  labs(x = NULL, y = "Phylogenetic diversity", color = NULL) +
  theme_bw() +
  theme(
    axis.text         = element_text(size = 14, color = "black"),
    axis.title.y      = element_text(size = 14, angle = 90),
    panel.grid        = element_blank(),
    panel.border      = element_rect(color = "black", size = 1),
    panel.background  = element_rect(fill = "white", colour = NA),
    plot.title        = element_text(hjust = 0.5),
    legend.position   = "none"     # 
  )

print(p_pd)
##beta diversity
#### library packages and load the mag and group
library(vroom)
library(ggplot2)
library(vegan)
library(tidyverse)
library(dplyr)
##
t_otu_nom <- t(otu_nom)
#### caculate distance matrix of all samples
set.seed(123)
t_otu_nom_bray <- vegdist(t_otu_nom,method = "bray")
t_otu_nom_bray

### anosim of samples of differnet trenches
anosim_result_dis <- anosim(t_otu_nom_bray, data_group$group, permutations = 999)
summary(anosim_result_dis) #get R and p of anosim
# 1. PERMANOVA
permanova_res2 <- adonis2(
  t_otu_nom_bray ~ group,
  data        = data_group,
  permutations = 999
)
# 
print(permanova_res2)
###1. NMDS
###1.1 NMDS of Trench
nmds <- metaMDS(t_otu_nom_bray, k = 2)
stress <- nmds$stress ### stress
nm_df <- as.data.frame(nmds$points)#get points
nm_df$sample <- row.names(nm_df)  

group<-read.table("group3.txt",
                  sep='\t', header=T,check.names=FALSE )

nm_df1 <- merge(nm_df,group,by="sample") # all sample points
color=c("#367DB0", "#3D9F3C")
p7<-ggplot(data=nm_df1,aes(MDS1, MDS2))+#
  theme_bw()+
  theme(panel.grid = element_blank()) +
  geom_point(aes(fill=group),size=2.5,color="NA",shape=21,alpha=0.7)+#
  labs(x=paste0("NMDS1 (Stress=0.141)"),
       y=paste0("NMDS2 "))+
  scale_fill_manual(values = color)+
  theme(axis.title.x=element_text(size=14,color="black"),#
        axis.title.y=element_text(size=14,angle=90,color="black"),#
        axis.text.y=element_text(size=14,color="black"),#x
        axis.text.x=element_text(size=14,color="black"),
        panel.border = element_rect(size = 1, color = "black"),
        legend.position   = "none"     # 
  )

print(p7)

##pCoA
pcoa <- cmdscale (t_otu_nom_bray,eig=TRUE)
pc12 <- pcoa$points[,1:2]
pc <- round(pcoa$eig/sum(pcoa$eig)*100,digits=2)
pc12 <- as.data.frame(pc12)
pc12$sample <- row.names(pc12)
head(pc12)
eig = summary(eigenvals(pcoa))#over all
axis = paste0("PCoA", 1:ncol(eig))
eig = data.frame(Axis = axis, t(eig)[, -3])
#get pco1 and pco2 of all sample
pco1 = round(eig[1, 3] * 100, 2)
pco2 = round(eig[2, 3] * 100, 2)

pcoa_all <- merge(pc12,group,by="sample")
###
color=c("#367DB0", "#3D9F3C")

p8<-ggplot(data=pcoa_all,aes(x=V1,y=V2))+#
  theme_bw()+
  theme(panel.grid = element_blank()) +
  geom_point(aes(fill=group),size=2.5,color="NA",shape=21,alpha=0.7)+
  labs(x=paste0("PCo1 = 24.43%"),
       y=paste0("PCo2 = 18.62%"))+
  scale_fill_manual(values = color)+
  theme(axis.title.x=element_text(size=14,color="black"),#
        axis.title.y=element_text(size=14,angle=90,color="black"),#
        axis.text.y=element_text(size=14,color="black"),#x
        axis.text.x=element_text(size=14,color="black"),
        panel.border = element_rect(size = 1, color = "black"),
        legend.position   = "none"    
  )
print(p8)

##save figure

combined_alpha_beta_pd <- (p1 + p3.1 + p4) /
  (p5 + p6 + p_pd) / (p7 + p8)

combined_alpha_beta_pd


ggsave("alpha_beta_mirus.pdf", plot = combined_alpha_beta_pd, width = 8, height = 8, dpi = 1200)
















