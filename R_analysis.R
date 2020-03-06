#Data anaylsis for stable istope data
setwd('C:\\Users\\kayle\\OneDrive\\Documents\\Research\\SEF')
library(tidyverse)
library(ggplot2)
library(plotly)
library(ggpubr) 
library(ggsignif) 
library(viridis)
library(dplyr)

#Importing data
data = read.csv('SI_Mouse_Data.csv', header = TRUE)
View(data)

#Re-ordering variables
data$season = ordered(data$season, levels = c("Spring", "Summer", "Fall", "Winter"))

#Renaming variables
nitrogen = data$d15N
carbon = data$d13C
habitat = data$habitat
season = data$season

#Creating boxplots to visualize the data
#d13C v. habitat type
ggplot(data, aes(habitat, carbon, fill = habitat)) + 
  geom_boxplot(outlier.size = 2.5, lwd = 1.5, alpha = 0.7) + 
  theme_classic() + 
  scale_fill_manual(values=c('#009E73','#E69F00', '#999999', '#56B4E9')) +
  theme(legend.position = "none", 
        text = element_text(size = 25),
        axis.title = element_blank(),
        panel.border = element_blank()) + 
  geom_jitter(size =2, width = 0.2, alpha = 0.2) +
  geom_signif(test="wilcox.test", comparisons = combn(levels(data$habitat),2, simplify = F), step_increase = 0.15, p.adjust.method = "BH", map_signif_level = TRUE) +  
  stat_compare_means(label.y = 2) +
  scale_y_continuous(limits=c(-25, 2), breaks = c(-25,-20,-15,-10))
#d13C v. season
ggplot(data, aes(season, carbon, fill = season)) + 
  geom_boxplot(outlier.size = 2.5, lwd = 1.5, alpha = 0.7) + 
  theme_classic() + 
  scale_fill_grey() +
  theme(legend.position = "none", 
        text = element_text(size = 25),
        axis.title = element_blank(),
        panel.border = element_blank()) + 
  geom_jitter(size =2, width = 0.2, alpha = 0.2) +
  stat_compare_means(label.y = 2) +
  scale_y_continuous(limits=c(-25, 2), breaks = c(-25,-20,-15,-10))
#d15N v. habitat type
ggplot(data, aes(habitat, nitrogen, fill = habitat)) + 
  geom_boxplot(outlier.size = 2.5, lwd = 1.5, alpha = 0.7) + 
  theme_classic() + 
  scale_fill_manual(values=c('#009E73','#E69F00', '#999999', '#56B4E9')) +
  theme(legend.position = "none", 
        text = element_text(size = 25),
        axis.title = element_blank(),
        panel.border = element_blank()) + 
  geom_jitter(size =2, width = 0.2, alpha = 0.2) +
  geom_signif(test="wilcox.test", comparisons = combn(levels(data$habitat),2, simplify = F), step_increase = 0.15, p.adjust.method = "BH", map_signif_level = TRUE) +  
  stat_compare_means(label.y = 40) +
  scale_y_continuous(name="Stopping distance", limits=c(7, 40), breaks = c(10, 15, 20, 25))
#d15N v. season
ggplot(data, aes(season, nitrogen, fill = season)) + 
  geom_boxplot(outlier.size = 2.5, lwd = 1.5, alpha = 0.7) + 
  theme_classic() + 
  scale_fill_grey() +
  theme(legend.position = "none", 
        text = element_text(size = 25),
        axis.title = element_blank(),
        panel.border = element_blank()) + 
  geom_jitter(size =2, width = 0.2, alpha = 0.2) +
  geom_signif(test="wilcox.test", comparisons = combn(levels(data$season),2, simplify = F), step_increase = 0.15, p.adjust.method = "BH", map_signif_level = TRUE) +  
  stat_compare_means(label.y = 40) +
  scale_y_continuous(name="Stopping distance", limits=c(7, 40), breaks = c(10, 15, 20, 25))

#Summary statistics...
group_by(data, habitat) %>% 
  summarise(
    count = n(),
    mean = mean(carbon, na.rm = TRUE),
    sd = sd(carbon, na.rm = TRUE),
    median = median(carbon, na.rm= TRUE),
    IQR = IQR(carbon, na.rm = TRUE)
  )
group_by(data, season) %>% 
  summarise(
    count = n(),
    mean = mean(carbon, na.rm = TRUE),
    sd = sd(carbon, na.rm = TRUE),
    median = median(carbon, na.rm= TRUE),
    IQR = IQR(carbon, na.rm = TRUE)
  )
group_by(data, habitat) %>% 
  summarise(
    count = n(),
    mean = mean(nitrogen, na.rm = TRUE),
    sd = sd(nitrogen, na.rm = TRUE),
    median = median(nitrogen, na.rm= TRUE),
    IQR = IQR(nitrogen, na.rm = TRUE)
  )
group_by(data, season) %>% 
  summarise(
    count = n(),
    mean = mean(nitrogen, na.rm = TRUE),
    sd = sd(nitrogen, na.rm = TRUE),
    median = median(nitrogen, na.rm= TRUE),
    IQR = IQR(nitrogen, na.rm = TRUE)
  )

#Kruskal-Wallis tests, alpha = 0.05
kruskal.test(carbon~habitat, data = data)
kruskal.test(carbon~season, data = data)
kruskal.test(nitrogen~habitat, data = data)
kruskal.test(nitrogen~season, data = data)

#Pairwise analyses in order to determine where significant differences lie:
pairwise.wilcox.test(carbon, habitat, p.adjust.method = "BH")
pairwise.wilcox.test(carbon, season, p.adjust.method = "BH")
pairwise.wilcox.test(nitrogen, habitat, p.adjust.method = "BH")
pairwise.wilcox.test(nitrogen, season, p.adjust.method = "BH")




