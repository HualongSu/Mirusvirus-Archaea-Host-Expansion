rm(list=ls())#clear Global Environment
setwd("")
#install.packages("reshape2")
suppressMessages(library(reshape2))
suppressMessages(library(dplyr))
suppressMessages(library(ggplot2))
#install.packages("aplot")
suppressMessages(library(aplot))
#install.packages("cowplot")
suppressMessages(library(cowplot))
df.quality <- read.table("input_final.txt", header = TRUE, sep = "\t") %>%
  mutate(Type1 = factor(Type1, levels = c("near", "high", "medium")))
# 
df.labels <- c()
df.labels <- c(df.labels, paste0("Near complete (", nrow(filter(df.quality, Type1 == "near")), ", ", round(100 * nrow(filter(df.quality, Type1 == "near")) / nrow(df.quality), 2), "%)"))
df.labels <- c(df.labels, paste0("High quality (", nrow(filter(df.quality, Type1 == "high")), ", ", round(100 * nrow(filter(df.quality, Type1 == "high")) / nrow(df.quality), 2), "%)"))
df.labels <- c(df.labels, paste0("Medium quality (", nrow(filter(df.quality, Type1 == "medium")), ", ", round(100 * nrow(filter(df.quality, Type1 == "medium")) / nrow(df.quality), 2), "%)"))

# 
p1.2 <- ggplot(data = df.quality, aes(x = Completeness, y = Contamination,
                                      colour = Type1, shape = Type2)) +
  geom_point(size = 1) +
  labs(x = "Completeness (%)", y = "Contamination (%)", colour = "") +
  scale_color_manual(values = c("#704da8", "#367DB0", "#3D9F3C"), labels = df.labels) +
  scale_shape_manual(values = c("OWN" = 16, "GTDB" = 17)) + 
  scale_x_continuous(limits = c(50, 100)) +
  scale_y_continuous(limits = c(0, 10)) +
  geom_vline(xintercept = 70, linetype = "dashed", color = "black", linewidth = 0.5) +
  annotate("segment",
           x = 90, xend = 100,
           y = 5, yend = 5,
           colour = "black",
           linewidth = 0.5,
           linetype = "dashed") +
  annotate("segment",
           x = 90,
           y = 0, yend = 5,
           colour = "black",
           linewidth = 0.5,
           linetype = "dashed") +
  theme_bw() +
  theme(
    plot.margin = unit(c(0, 0, 0, 0), "cm"),
    plot.background = element_rect(fill = 'transparent', color = NA),
    panel.background = element_rect(fill = 'transparent'),
    panel.border = element_blank(),  #
    panel.grid = element_blank(),
    axis.title = element_text(size = 20, face = "bold"),
    axis.text = element_text(size = 18, face = "bold"),
    axis.ticks = element_line(linewidth = 1),
    axis.line.x = element_line(color = "black", linewidth = 0.5),  # 
    axis.line.y = element_line(color = "black", linewidth = 0.5),  # 
    legend.position = "none",
    legend.text = element_text(size = 20)
  ) +
  guides(colour = guide_legend(override.aes = list(shape = 15, size = 10)))

p1.2





p2.1 <- ggplot(data = df.quality) +
  geom_histogram(aes(x = Completeness, y = after_stat(count / sum(count)), fill = Type1), color = "white", breaks = seq(50, 100, by = 2), width = 0.8, linewidth = 0) +
  scale_fill_manual(values = c("#704da8", "#367DB0", "#3D9F3C")) +
  scale_y_continuous(expand = c(0,0), limits = c(0, 0.105), breaks = seq(0, 0.1, by = 0.02), labels = c("", "", "", "", "", "10%")) +
  labs(x = "", y = "") +
  theme_bw() +
  theme(plot.margin = unit(c(0, 0, 0, 0), "cm"),
        plot.background = element_rect(fill = 'transparent', color = NA),
        panel.background = element_blank(),
        panel.border = element_blank(),
        panel.grid = element_blank(),
        axis.line.y = element_line(linewidth = 0.5),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_line(linewidth = 1),
        axis.ticks.length.y = unit(0.2, "cm"),
        axis.text.x = element_blank(),
        axis.text.y = element_text(size = 18, face = "bold"),
        legend.position = "none")

p2.1

p3.1 <- ggplot(data = df.quality) +
  geom_histogram(aes(x = Contamination, y = after_stat(count / sum(count)), fill = Type1), color = "white", breaks = seq(0, 10, by = 0.5), width = 0.8, linewidth = 0) +
  scale_y_continuous(expand = c(0,0), limits = c(0, 0.25), breaks = seq(0, 0.25, by = 0.05), labels = c("", "", "", "", "", "25%")) +
  scale_fill_manual(values = c("#704da8", "#367DB0", "#3D9F3C")) +
  coord_flip() +
  labs(x = "", y = "") +
  theme_bw() +
  theme(plot.margin = unit(c(0, 0, 0, 0), "cm"),
        plot.background = element_rect(fill = 'transparent', color= NA),
        panel.background = element_blank(),
        panel.border = element_blank(),
        panel.grid = element_blank(),
        axis.line.x = element_line(linewidth = 0.5),
        axis.ticks.x = element_line(linewidth = 1),
        axis.ticks.y = element_blank(),
        axis.ticks.length.x = unit(0.2, "cm"),
        axis.text.x = element_text(size = 18, face = "bold"),
        axis.text.y = element_blank(),
        legend.position = "none")


p3.1
p <- p1.2 %>% insert_top(p2.1, height = 0.25)
p <- p %>% insert_right(p3.1, width = 0.25)
p

ggsave("bins_quality_no_legenth5.pdf", dpi=1200, plot=p, width=10, height=10)
ggsave("final3_bins_quality_no_legenth10.pdf", dpi=1200, plot=p, width=5, height=5)


