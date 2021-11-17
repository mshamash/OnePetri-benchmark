library(tidyverse)
library(ggpubr)
library(ggsignif)
library(reshape2)

sim.benchmark <- read.csv("first50/benchmark-sim.csv", header = T)
device.benchmark <- read.csv("first50/benchmark-device.csv", header = T)
pst.benchmark <- read.csv("first50/benchmark-pst.csv", header = T)
cfuai.benchmark <- read.csv("first50/benchmark-cfuai.csv", header = T)
manual.benchmark <- read.csv("first50/benchmark-manual.csv", header = T)

manual.benchmark.filt <- na.omit(manual.benchmark)
manual.benchmark.filt$TimeToInfer <- as.numeric(manual.benchmark.filt$TimeToInfer)
manual.benchmark.filt$PlaqueCount <- as.numeric(manual.benchmark.filt$PlaqueCount)


### Time Comparison Plots & Stats
total.times <- data_frame(device.benchmark$PlateID, device.benchmark$TimeToInferPlusNMS, sim.benchmark$TimeToInferPlusNMS, cfuai.benchmark$TimeToInfer, pst.benchmark$TimeToInfer, manual.benchmark$TimeToInfer)
colnames(total.times) <- c("PlateID", "OnePetri\n(device)", "OnePetri\n(simulator)", "CFU.AI", "PST", "Manual")

total.times.melt <- melt(total.times, id.vars = "PlateID")
total.times.melt$value <- as.numeric(total.times.melt$value)


ggplot(total.times.melt, aes(x = value)) + 
  geom_histogram() +
  facet_wrap(variable ~ .)

times.plot <- times.plot <- ggplot(total.times.melt, aes(x = variable, y = value)) +
  geom_boxplot(aes(fill=variable)) +
  geom_point(alpha = 0.35, position = position_jitter(width = 0.3)) +
  xlab("Tool") +
  ylab("Time to Result (seconds)") +
  scale_y_continuous(breaks=seq(0, 280, 25)) +
  theme_bw() +
  theme(legend.position = "none") +
  scale_fill_brewer(palette = "Dark2", name = "Tool") +
  geom_signif(comparisons = list(c("OnePetri\n(device)", "OnePetri\n(simulator)"), c("OnePetri\n(device)", "PST"), c("OnePetri\n(device)", "Manual")), 
              map_signif_level = T, 
              textsize = 6, 
              test = "wilcox.test", 
              step_increase = 0.075,
              margin_top = -0.2)

ggsave(file = "times.svg", plot = times.plot, width = 5, height = 5)

times.aov <- aov(value ~ variable, total.times.melt)
summary(times.aov)

pairwise.wilcox.test(total.times.melt$value, total.times.melt$variable, p.adjust.method = "BH")


### Relative error analysis
total.plaques <- data_frame(device.benchmark$PlaqueCount, sim.benchmark$PlaqueCount, cfuai.benchmark$PlaqueCount, pst.benchmark$PlaqueCount, manual.benchmark$PlaqueCount)
colnames(total.plaques) <- c("OnePetri\n(device)", "OnePetri\n(simulator)", "CFU.AI", "PST", "Manual")

total.plaques <- na.omit(total.plaques)

total.plaques.diff <- data_frame(
  100*abs((total.plaques$`OnePetri-device`-total.plaques$Manual)/total.plaques$Manual),
  100*abs((total.plaques$`OnePetri-simulator`-total.plaques$Manual)/total.plaques$Manual),
  100*abs((total.plaques$`CFU.AI`-total.plaques$Manual)/total.plaques$Manual),
  100*abs((total.plaques$PST-total.plaques$Manual)/total.plaques$Manual)
)
colnames(total.plaques.diff) <- c("OnePetri\n(device)", "OnePetri\n(simulator)", "CFU.AI", "PST")

summary(total.plaques.diff)

total.plaques.melt <- melt(total.plaques.diff)

rel.diff.plot <- ggplot(total.plaques.melt, aes(x = variable, y = value)) +
  geom_boxplot(aes(fill=variable)) +
  geom_point(alpha = 0.35, position = position_jitter(width = 0.3)) +
  xlab("Tool") +
  ylab("Relative Error to Manual Counts (%)") +
  scale_y_continuous(limits = c(0, 135), breaks=seq(-100, 120, 20)) +
  theme_bw() +
  theme(legend.position = "none") +
  scale_fill_brewer(palette = "Dark2", name = "Tool") +
  geom_signif(comparisons = list(c("OnePetri\n(device)", "CFU.AI"), c("OnePetri\n(device)", "PST")), 
              map_signif_level = T, 
              textsize = 6, 
              test = "wilcox.test", 
              step_increase = 0.035,
              margin_top = -0.655,
              tip_length = 0.0175)

ggsave(file = "rel-diff.svg", plot = rel.diff.plot, width = 4, height = 5)

plaques.aov <- aov(value ~ variable, total.plaques.melt)
summary(plaques.aov)

pairwise.wilcox.test(total.plaques.melt$value, total.plaques.melt$variable, p.adjust.method = "BH")


### Correlation between error and plaque counts
total.plaques.diff.withmanual <- data_frame(
  100*abs((total.plaques$`OnePetri
(device)`-total.plaques$Manual)/total.plaques$Manual),
  100*abs((total.plaques$`OnePetri
(simulator)`-total.plaques$Manual)/total.plaques$Manual),
  100*abs((total.plaques$`CFU.AI`-total.plaques$Manual)/total.plaques$Manual),
  100*abs((total.plaques$PST-total.plaques$Manual)/total.plaques$Manual),
  abs(total.plaques$Manual)
)

colnames(total.plaques.diff.withmanual) <- c("OnePetri-Device", "OnePetri-Simulator", "CFU.AI", "PST", "TrueCount")

total.plaques.diff.withmanual <- melt(total.plaques.diff.withmanual, id.vars = "TrueCount")
colnames(total.plaques.diff.withmanual) <- c("TrueCount", "Tool", "ErrorRate")

rel.diff.corr.plot <- ggplot(total.plaques.diff.withmanual, aes(x = TrueCount, y = ErrorRate, colour = Tool, shape = Tool)) +
  geom_point(size = 2) +
  geom_smooth(method="lm", se = F) +
  scale_y_continuous(limits = c(0, 135), breaks=seq(-100, 120, 20)) +
  theme_bw() +
  scale_color_brewer(palette = "Dark2", name = "Tool") +
  xlab("Manual Plaque Count") +
  ylab("Relative Error to Manual Counts (%)")

ggsave(file = "rel-diff-corr.svg", plot = rel.diff.corr.plot, width = 9, height = 3.5)


corr.onepetri.dev <- cor(filter(total.plaques.diff.withmanual, Tool == "OnePetri-Device")[, -c(2)], method = "pearson")
corr.onepetri.sim <- cor(filter(total.plaques.diff.withmanual, Tool == "OnePetri-Simulator")[, -c(2)], method = "pearson")
corr.cfuai <- cor(filter(total.plaques.diff.withmanual, Tool == "CFU.AI")[, -c(2)], method = "pearson")
corr.pst <- cor(filter(total.plaques.diff.withmanual, Tool == "PST")[, -c(2)], method = "pearson")

corr.onepetri.dev.r2 <- corr.onepetri.dev ^ 2
corr.onepetri.sim.r2 <- corr.onepetri.sim ^ 2
corr.cfuai.r2 <- corr.cfuai ^ 2
corr.pst.r2 <- corr.pst ^ 2


### Model Metrics Plots
petri.metrics <- read.csv("wandb/petri-dish-metrics.csv", header = T)
plaque.metrics <- read.csv("wandb/plaque-metrics.csv", header = T)

precision <- ggplot(petri.metrics, aes(x = epoch, y = precision)) +
  geom_line(aes(color = "Petri dish")) +
  geom_line(data = plaque.metrics, aes(color = "Plaque")) +
  scale_color_manual("Model", limits=c("Petri dish", "Plaque"), values = c("#4b9b7a", "#ca6727")) +
  xlab("") + 
  theme_bw() +
  scale_x_continuous(limits = c(0,500), expand = expansion(mult = c(0, .025))) +
  scale_y_continuous(limits = c(0,1.00), expand = expansion(mult = c(0, .025)))


recall <- ggplot(petri.metrics, aes(x = epoch, y = recall)) +
  geom_line(aes(color = "Petri dish")) +
  geom_line(data = plaque.metrics, aes(color = "Plaque")) +
  scale_color_manual("Model", limits=c("Petri dish", "Plaque"), values = c("#4b9b7a", "#ca6727")) +
  xlab("") + 
  theme_bw() +
  scale_x_continuous(limits = c(0,500), expand = expansion(mult = c(0, .025))) +
  scale_y_continuous(limits = c(0,1.00), expand = expansion(mult = c(0, .025)))


mAP0.5 <- ggplot(petri.metrics, aes(x = epoch, y = mAP0.5)) +
  geom_line(aes(color = "Petri dish")) +
  geom_line(data = plaque.metrics, aes(color = "Plaque")) +
  scale_color_manual("Model", limits=c("Petri dish", "Plaque"), values = c("#4b9b7a", "#ca6727")) +
  theme_bw() +
  scale_x_continuous(limits = c(0,500), expand = expansion(mult = c(0, .025))) +
  scale_y_continuous(limits = c(0,1.00), expand = expansion(mult = c(0, .025)))


mAP0.5_0.95 <- ggplot(petri.metrics, aes(x = epoch, y = mAP0.5_0.95)) +
  geom_line(aes(color = "Petri dish")) +
  geom_line(data = plaque.metrics, aes(color = "Plaque")) +
  scale_color_manual("Model", limits=c("Petri dish", "Plaque"), values = c("#4b9b7a", "#ca6727")) +
  theme_bw() +
  scale_x_continuous(limits = c(0,500), expand = expansion(mult = c(0, .025))) +
  scale_y_continuous(limits = c(0,1.00), expand = expansion(mult = c(0, .025)))

plots_arranged <- ggarrange(precision, recall, mAP0.5, mAP0.5_0.95, nrow = 2, ncol = 2, common.legend = T, legend = "bottom")
final_plots_arranged <- annotate_figure(plots_arranged, top = text_grob("Object Detection Model Metrics", color = "black", face = "bold", size = 14))

ggsave(file = "metrics-plots.svg", plot = final_plots_arranged, width = 8, height = 6)
