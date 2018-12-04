library(devtools)
library(ggplot2)
library(ggridges)

ggplot(gd99, aes(y = as.character(Time), x = Weight_Choice, fill = as.factor(Choice_Proportion))) +
  geom_density_ridges(scale = 3, alpha = 0.8) + 
  theme_ridges() +
  scale_fill_brewer(palette = 2) +
  scale_y_discrete(limits=c("-40","-30","-20","-10","0","10","20","30","40")) +
  labs(y = "Time", x = "Choice Proportion x WTP Difference Proportion", fill = "Proportion of Gamble", title = "Games & Dishes")

ggplot(st99, aes(y = as.character(Time), x = Weight_Choice, fill = as.factor(Choice_Proportion))) +
  geom_density_ridges(scale = 3, alpha = 0.8) + 
  scale_fill_brewer(palette = 4) +
  theme_ridges() +
  scale_y_discrete(limits=c("-40","-30","-20","-10","0","10","20","30","40")) +
  labs(y = "Time", x = "Choice Proportion x WTP Difference Proportion", fill = "Proportion of Gamble", title = "Sports & Traffic Jam")

ggplot(mv99, aes(y = as.character(Time), x = Weight_Choice, fill = as.factor(Choice_Proportion))) +
  geom_density_ridges(scale = 3, alpha = 0.8) + 
  scale_fill_brewer(palette = 5) +
  theme_ridges() +
  scale_y_discrete(limits=c("-40","-30","-20","-10","0","10","20","30","40")) +
  labs(y = "Time", x = "Choice Proportion x WTP Difference Proportion", fill = "Proportion of Gamble", title = "Sports & Traffic Jam")



## times evaluation?