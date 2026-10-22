library(ggplot2)
library(dplyr)
library(tidyr)
library(readxl)

#load in data
litData=read_excel("Data/input/publishedDemandProj.xlsx")

#Separate data from this study and other studies
demand = litData %>% filter(source != "This study", ssp != "No class")
thisStudyDemand = litData %>% filter(source == "This study")

#Boxplot comparing projections by SSP (Figure 5)
ggplot(data = demand, aes(x=ssp, y = proj)) +
  geom_boxplot(outlier.shape= NA, color = "darkgrey") +
  geom_point(color = "black", size=1.5, alpha=0.2)+
  geom_point(data = thisStudyDemand, aes(color="cornflowerblue"), size = 4)+
  labs(x = "",
       y = "Projected demand (kcal/cap/day)",
       color = "")+
  theme_minimal(base_size =22)+
  theme(panel.border = element_rect(color = "grey", fill=NA))+
  theme(strip.background = element_rect(color = "grey", fill=NA),
        strip.text = element_text(color = "black"))+
  scale_color_manual(values = "cornflowerblue", labels = "This Study")+
  facet_wrap(~year)

