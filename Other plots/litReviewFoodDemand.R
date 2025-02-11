library(ggplot2)
library(dplyr)
library(tidyr)
library(readxl)

#load in data
litData=read_excel("Data/inputs/publishedDemandProj.xlsx")

#Separate data from this study and other studies
demand = litData %>% filter(source != "This study", ssp != "No class")
thisStudyDemand = litData %>% filter(source == "This study")

#Boxplot comparing projections by SSP
ggplot(data = demand, aes(x=ssp, y = proj)) +
  geom_boxplot(outlier.shape= NA, color = "darkgrey") +
  geom_point(color = "black", size=1.5, alpha=0.2)+
  geom_point(data = thisStudyDemand, aes(color="cornflowerblue"), size = 3.5)+
  labs(x = "",
       y = "Projected demand (kcal/cap/day)",
       color = "")+
  theme_minimal()+
  theme(
    text = element_text(size=18),
    axis.text.x = element_text(size = 16),
    axis.text.y = element_text(size = 14),
    legend.text = element_text(size = 18)
  )+
  theme(strip.text.x = element_text(size = 14))+
  theme(panel.border = element_rect(color = "grey", fill=NA))+
  theme(strip.background = element_rect(color = "grey", fill=NA),
        strip.text = element_text(color = "black"))+
  scale_color_manual(values = "cornflowerblue", labels = "This Study")+
  facet_wrap(~year)

