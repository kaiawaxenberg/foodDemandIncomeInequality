library(ggplot2)
library(dplyr)
library(tidyr)

###################Load and summarise results data###################

#Load raw results and join population data
file_path = "Data/results/inequality"
ssps = c("SSP1", "SSP2", "SSP3", "SSP4", "SSP5")
countryDemandOpt = data.frame()
for (ssp in ssps){
  demandOptTemp =  read.csv(file.path(file_path, ssp, "countryDemandOpt.txt")) %>% mutate(SSP=ssp)
  countryDemandOpt=rbind(countryDemandOpt, demandOptTemp)
  rm(demandOptTemp)
}
countries = read.csv("Data/inputs/countries.csv")
countryDemandOpt = left_join(countryDemandOpt, countries, join_by(Country==Area)) %>% 
  select(!c(PlumGroup, FaoArea, Population, M49)) %>% mutate(decile = as.integer(substring(decile, 2)))

#Sum animal calories and other calories for each region and decile, for plotting
totalKcal = countryDemandOpt %>% group_by(Country, decile, SSP, Year, population, Region) %>%
  summarise(otherKcal = sum(rebasedKcal[which(!(commodity %in% c("Ruminants","Monogastrics")))]), animalKcal=sum(rebasedKcal[which(commodity %in% c("Ruminants","Monogastrics"))])) %>% 
  group_by(Region, decile, SSP, Year) %>%
  summarise(otherKcal = weighted.mean(otherKcal, population), animalKcal = weighted.mean(animalKcal, population)) %>%
  pivot_longer(cols=c(otherKcal, animalKcal))

#Calculate average global calorie demand by commodity 
globalDemand = countryDemandOpt %>% group_by(SSP, Year, commodity) %>%
  summarise(rebasedKcal = weighted.mean(rebasedKcal, population)) %>% mutate(Region = "World")

#Calculate demand across deciles by region and commodity
regionDemand = countryDemandOpt %>% group_by(Region, SSP, Year, commodity) %>%
  summarise(rebasedKcal = weighted.mean(rebasedKcal, population))

###################Generate Figures###################

#Generate Figure from SI1
ggplot(data=filter(regionDemand, Year==2100), aes(x=reorder(Region, -rebasedKcal), y=rebasedKcal, fill=reorder(commodity, rebasedKcal))) +
  geom_col(data = filter(globalDemand, Year==2100)) + 
  geom_col() + 
  labs(x = "",
       y = "Food Demand (kcal/cap/day)",
       fill = "Commodity")+
  theme_light()+
  facet_wrap(~SSP)+
  scale_fill_brewer(palette="Set2")+
  theme_minimal()+
  theme(
    strip.text.x = element_text(size = 14),
    strip.text.y = element_text(size = 14), 
    text = element_text(size=14),
    axis.text.x = element_text(size = 13),
    axis.text.y = element_text(size = 12),
    legend.text = element_text(size = 12))+
  theme(axis.text.x = element_text(angle = 75, hjust = 1))

#Generate Figure 1
totalKcal = totalKcal %>% mutate(RegionLab = gsub("^((\\S+\\s+\\S+\\s+))", "\\1\n", Region)) %>%
  mutate(RegionLab = case_when(RegionLab=="Sub-Saharan Africa" ~ "Sub-Saharan \n Africa", 
         TRUE ~ RegionLab))
avg_data = totalKcal %>% filter(Year==2100) %>% group_by(RegionLab, SSP, decile) %>% 
  mutate(value= case_when(name=="otherKcal" ~ sum(value),
                          name=="animalKcal"~ value)) %>% group_by(RegionLab, SSP, name)%>%
  summarise(value = mean(value)) 


ggplot(data=totalKcal %>% filter(Year==2100), aes(x=decile, fill=reorder(name, -value), y=value)) +
  geom_col(width = 0.84, position="stack") +
  geom_hline(data = avg_data, aes(yintercept = value,color = "Regional average"), 
             linetype = "dashed")+
  labs(x = "Income Decile",
       y = "Food Demand (kcal/cap/day)",
       fill = "", 
       linetype="")+
  theme_minimal()+
  scale_color_manual(name ="",values = c("Regional average" = "#2F2F2F")) +
  scale_fill_brewer(palette="Set2", labels = c("animalKcal" = "Animal-based", "otherKcal" = "Plant-based"))+
  facet_grid(SSP~RegionLab, labeller = labeller(label_wrap_gen(width = 5)))+
  scale_x_continuous(breaks = c(2,4,6, 8, 10))+
  theme(plot.title = element_text(hjust = 0.5, size = 14), text=element_text(size = 12))+
  theme(axis.text.x = element_text(size=8), axis.text.y = element_text(size=8), legend.position="bottom")+
  theme(
    strip.text.x = element_text(size = 14),
    strip.text.y = element_text(size = 14), 
    text = element_text(size=14),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12),
    legend.text = element_text(size = 13)
  )+
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank()
  )

###################For comparison to other studies###################

#For comparison to Martinez (2024)
countryDecileDemand = countryDemandOpt %>% group_by(Country, decile, SSP, Year) %>%
  summarise(rebasedKcal =sum(rebasedKcal)) %>% ungroup() %>% filter(Year==2020, SSP=="SSP2") %>%
  group_by(Country, SSP, Year) %>%
  mutate(diff = rebasedKcal[which(decile==10)] - rebasedKcal[which(decile==1)])

#For comparison to Bijl (2017)
countryDiff = countryDemandOpt %>% group_by(Country, decile, SSP, Year) %>%
  summarise(rebasedKcal =sum(rebasedKcal)) %>% ungroup() %>% filter(Year==2050, SSP=="SSP2") %>%
  group_by(Country, SSP, Year) %>%
  summarise(diff = mean(rebasedKcal[which(decile==10 | decile==9)]) - mean(rebasedKcal[which(decile==1 | decile ==2)]))
regionDiff = countryDemandOpt %>% filter(Year==2050, SSP=="SSP2") %>% group_by(Country, decile, SSP, Year, population, Region) %>%
  summarise(rebasedKcal = sum(rebasedKcal)) %>% 
  group_by(Region, decile, SSP, Year) %>%
  summarise(rebasedKcal = weighted.mean(rebasedKcal, population)) %>% group_by(Region, SSP, Year) %>%
  summarise(diff = mean(rebasedKcal[which(decile==10 | decile==9)]) - mean(rebasedKcal[which(decile==1 | decile ==2)]))

#For comparison to Bodirsky (2015)
commodityDemand = globalDemand %>% filter(Year ==2050 | Year ==2100) %>% group_by(Year, SSP) %>% mutate(percKcal=rebasedKcal/sum(rebasedKcal))
fracAnimal = commodityDemand %>% summarise(fracAnimal = sum(rebasedKcal[which(commodity=="Ruminants" | commodity == "Monogastrics")])/sum(rebasedKcal))
