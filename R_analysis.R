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

#Summary Statistics for d13C v. habitat types
group_by(data, habitat) %>% 
  summarise(
    count = n(),
    mean = mean(carbon, na.rm = TRUE),
    sd = sd(carbon, na.rm = TRUE),
    median = median(carbon, na.rm= TRUE),
    IQR = IQR(carbon, na.rm = TRUE)
  )

#summary statistics for d13C v. season
group_by(data, season) %>% 
  summarise(
    count = n(),
    mean = mean(carbon, na.rm = TRUE),
    sd = sd(carbon, na.rm = TRUE),
    median = median(carbon, na.rm= TRUE),
    IQR = IQR(carbon, na.rm = TRUE)
  )

#Summary statsitics for d15N v. habtiat types
group_by(data, habitat) %>% 
  summarise(
    count = n(),
    mean = mean(nitrogen, na.rm = TRUE),
    sd = sd(nitrogen, na.rm = TRUE),
    median = median(nitrogen, na.rm= TRUE),
    IQR = IQR(nitrogen, na.rm = TRUE)
  )

#Summary Statistics for d15N v. season
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
#Results
#Kruskal-Wallis rank sum test
#data:  carbon by habitat
#Kruskal-Wallis chi-squared = 104.54, df = 3, p-value < 2.2e-16
#INTERPRETING: since p < 0.05, there are significant differences in the mean carbon isotopic levels between habitats

kruskal.test(carbon~season, data = data)
#Results
#Kruskal-Wallis rank sum test
#data:  carbon by season
#Kruskal-Wallis chi-squared = 6.3392, df = 3, p-value = 0.09623
#INTERPRETING: since p > 0.05, there are not significant differences in the mean carbon isotopic levels between seasons

kruskal.test(nitrogen~habitat, data = data)
#Results
#Kruskal-Wallis rank sum test
#data:  nitrogen by habitat
#Kruskal-Wallis chi-squared = 65.018, df = 3, p-value = 4.973e-14
#INTERPRETING: since p < 0.05, there are significant differences in the mean nitrogen isotope levels between habitats

kruskal.test(nitrogen~season, data = data)
#Results
#Kruskal-Wallis rank sum test
#data:  nitrogen by season
#Kruskal-Wallis chi-squared = 9.5358, df = 3, p-value = 0.02295
#INTERPRETING: since p < 0.05, there are significant differences in the mean nitrogen isotope levels between seasons

#Pairwise analyses in order to determine where significant differences lie:
pairwise.wilcox.test(carbon, habitat, p.adjust.method = "BH")
#	Pairwise comparisons using Wilcoxon rank sum test 
#data:  carbon and habitat 
#         Forest  Herbland Shrub  
#Herbland 2.5e-06 -        -      
#Shrub    0.00047 1.4e-12  -      
#Wetland  2.2e-11 0.07373  < 2e-16
#P value adjustment method: BH 
#INTERPRETATION: There are significant differences between the Forest and Herbland, Forest and Shrub, Forest and Wetland, Shrub and Herbland, and Shrub and Wetland. There are not significant differences between the Herbland and the wetland in the mean carbon isotope levels.

pairwise.wilcox.test(carbon, season, p.adjust.method = "BH")
#	Pairwise comparisons using Wilcoxon rank sum test 
#data:  carbon and season 
#       Fall Spring Summer
#Spring 0.14 -      -     
#Summer 0.12 0.80   -     
#Winter 0.80 0.37   0.39  
#P value adjustment method: BH 
#INTERPRETATION: There are no significant differences between the mean carbon isotope levels, as shown above. (I forgot)

pairwise.wilcox.test(nitrogen, habitat, p.adjust.method = "BH")
#Pairwise comparisons using Wilcoxon rank sum test 
#data:  nitrogen and habitat 
#         Forest  Herbland Shrub  
#Herbland 8.4e-13 -        -      
#Shrub    0.00044 0.00064  -      
#Wetland  1.0e-08 0.80571  0.00400
#P value adjustment method: BH 
#INTERPRETATION: There are significant differences between the mean nitrogen isotope levels of Forest and Herbland, Shrub and Forest, Forest and Wetland, Shrub and Herbland, and Shrub and Wetland. There are no significant differences between the Herbland and Wetland.

pairwise.wilcox.test(nitrogen, season, p.adjust.method = "BH")
#	Pairwise comparisons using Wilcoxon rank sum test 
#data:  nitrogen and season 
#       Fall  Spring Summer
#Spring 0.016 -      -     
#Summer 0.193 0.425  -     
#Winter 0.078 0.666  0.666 
#P value adjustment method: BH 
#INTERPRETATION: There are significant differences between the nitrogen isotope levels for Spring and Fall. There are no significant differences between the nitrogen isotope levels between any other seasons.



