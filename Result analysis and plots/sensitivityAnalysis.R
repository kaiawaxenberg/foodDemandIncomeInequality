library(dplyr)
library(tidyr)
library(ggplot2)
library(DescTools)

##########load results for all scenarios##########
scenarios = c("plus0.1", "plus0.05", "minus0.1", "minus0.05", "noChange")
resultsDir = "Data/results/sensitivity"
lc = data.frame()
countryDemand = data.frame()
countryDemandOpt = data.frame()
for (scenario in scenarios){
  lcTemp =  read.csv(file.path(resultsDir, scenario, "lc.txt")) %>% mutate(Scenario=scenario)
  demandTemp =  read.csv(file.path(resultsDir, scenario, "countryDemand.txt")) %>% mutate(Scenario=scenario)
  demandOptTemp =  read.csv(file.path(resultsDir, scenario, "countryDemandOpt.txt")) %>% mutate(Scenario=scenario)

  lc=rbind(lc, lcTemp)
  countryDemand=rbind(countryDemand, demandTemp)
  countryDemandOpt=rbind(countryDemandOpt, demandOptTemp)

  rm(demandOptTemp, demandTemp, lcTemp)
}

##### Plot global food demand by sensitivity scenario #####
countryDiet = countryDemandOpt %>% group_by(Country, Year, decile, population, Scenario) %>% summarise(rebasedKcal = sum(rebasedKcal))
totalGlobalKcal = countryDiet %>% group_by(Year, Scenario) %>% summarise(rebasedKcal = weighted.mean(rebasedKcal, population)) %>%  mutate(YearNo = Year -2019) 
ggplot(data=totalGlobalKcal, aes(x=YearNo, y=rebasedKcal, color = Scenario)) +
  geom_line(linewidth=1) + 
  labs(title = "Total global food demand",
       x = "Year",
       y = "Global food Demand (kcal/cap/day)",
       color = "Inequality scenario")+
  theme_light()+
  theme(plot.title = element_text(hjust = 0.5, size = 14), text=element_text(size = 12))

#####Calculate differences between the control and scenarios#######
totalKcalDiff= totalGlobalKcal %>% filter(Year==2070) %>% mutate(refKcal = rebasedKcal[which(Scenario=="noChange")]) %>% 
  mutate(diff = rebasedKcal - refKcal) %>% mutate(percDiff = round(100* diff/refKcal,2))

animalKcalDiff = countryDemandOpt %>% filter(commodity=="Monogastrics" | commodity =="Ruminants") %>%
  group_by(Year, Scenario, Country, decile, population) %>% summarise(animalKcal = sum(rebasedKcal)) %>%
  group_by(Year, Scenario) %>% summarise(animalKcal = weighted.mean(animalKcal, population))%>% 
  group_by(Year) %>% mutate(ref = animalKcal[which(Scenario=="noChange")]) %>% 
  mutate(diff = animalKcal - ref) %>% mutate(percDiff = 100* diff/ref)

lcDiff = lc %>% mutate(AgLand = Cropland + Pasture) %>% group_by(Year) %>% 
  mutate(RefPasture = Pasture[which(Scenario=="noChange")] , RefAgLand  = AgLand[which(Scenario=="noChange")], RefCropland = Cropland[which(Scenario=="noChange")]) %>%
  mutate(AgLandDiff = AgLand - RefAgLand, PastureDiff = Pasture - RefPasture, CroplandDiff = Cropland - RefCropland ) %>% 
  mutate(PasturePercDiff = 100* PastureDiff/RefPasture, CroplandPercDiff = 100* CroplandDiff/RefCropland, AgLandPercDiff = 100* AgLandDiff/RefAgLand)

rm(totalGlobalKcal, lcDiff, totalKcalDiff, animalKcalDiff)

#####Plot Commodity Prices############
plotData = countryDemand %>% group_by(Year, Scenario, Commodity) %>% summarise(ConsumerPrice = mean(ConsumerPrice))
ggplot(data=plotData, aes(x=Year, color = Scenario, y=ConsumerPrice)) +
  geom_line()+
  labs(title = "Average national commodity prices",
       x = "Year",
       y = "Consumer Price",
       color = "Gini coefficient scenario") + 
  theme_minimal()+
  facet_wrap(~Commodity, scales = "free")+
  theme(plot.title = element_text(hjust = 0.5, size = 14), text=element_text(size = 12))
rm(plotData)

##### MERGE NATIONAL INCOME DATA WITH DEMAND DATA #####

#Set up country income data to merge with demand data
countries =read.csv("Data/inputs/countries.csv")
gdp2020 = read.csv("Data/inputs/ssp.csv") %>% filter(SSP == "SSP2", Year ==2020) %>% rename(gdp2020 = gdp, population2020= population) %>% 
  dplyr::select(Iso3, gdp2020, population2020)
incomeGroups = left_join(countries, gdp2020, join_by(Iso3))
incomeGroups = incomeGroups %>% dplyr::select(Area, IncomeGroup, gdp2020, population2020) %>% 
  mutate(gdpPc2020=gdp2020/population2020)
top10perc = incomeGroups %>% 
  arrange(desc(gdpPc2020))%>%
  mutate(n=n()) %>% 
  filter(row_number() <= round(n * .1)) 
 for(i in 1:nrow(incomeGroups)){
  if(incomeGroups[i,]$Area %in% top10perc$Area){
    incomeGroups[i,]$IncomeGroup = "Very high income"
  }
}
rm(top10perc)
incomeGroups = incomeGroups %>% dplyr::select(Area, IncomeGroup)

#create data tables with national income and demand, including change in demand from the control
countryIncomeKcal = countryDemandOpt %>% left_join(incomeGroups, join_by(Country==Area)) %>%
  group_by(Country, Year, decile, commodity) %>% mutate(refKcal = rebasedKcal[which(Scenario=="noChange")]) %>% 
  mutate(diff = rebasedKcal - refKcal) %>% 
  mutate(percDiff = 100* diff/refKcal)
rm(incomeGroups)

##### PLOT COMMODITY DEMAND BY INCOME GROUP #####
plotData = countryIncomeKcal %>% filter(Year ==2070, IncomeGroup !=0, Scenario!="noChange") %>% group_by(commodity, IncomeGroup, Scenario)%>%
  summarise(ref = mean(refKcal), rebasedKcal = mean(rebasedKcal), diff = (rebasedKcal - ref))

ggplot(data=plotData, aes(x=commodity, fill = Scenario, y=diff)) +
  geom_bar(position="dodge", stat="identity")+
  labs(title = "Sensitivity of global food demand to the Gini coefficient",
       x = "Commodity",
       y = "Change in demand from control scenario (kcal/cap/day)",
       fill = "Gini coefficient scenario") + 
  facet_wrap(~IncomeGroup)+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5, size = 14), text=element_text(size = 12))

rm(plotData)

##### Look at initial gini coefficient
#calculate startGinis from gdpPcDistribution.R
giniBaseline = read.csv("Data/inputs/giniBaseline.csv") %>%
  left_join(countries, join_by(ISO==Iso3)) %>% group_by(Area) %>% summarise(startGini=mean(gini)) %>% dplyr::select(Area, startGini)

kcalWithInitialGini = left_join(countryIncomeKcal, giniBaseline, join_by(Country==Area))

#Aim to plot gini coefficient vs the ln of 2070 kcal and food demand
kcalWithGini = kcalWithInitialGini %>% mutate(change = as.double(gsub("[^0-9.-]", "", Scenario))) %>%
  mutate(direction = ifelse((Scenario=="minus0.1" | Scenario =="minus0.05"), -1, 1)) %>%
  mutate(change = change * direction)%>%
  mutate(gini = startGini + (change/50)*(Year-2020)) %>% filter(!is.na(gini))

#by global commodity demand
kcalWithGiniCommodityGlobal = kcalWithGini %>% group_by(commodity, Scenario, Year) %>% summarise(gini = mean(gini), rebasedKcal = mean(rebasedKcal))
ggplot(data=kcalWithGiniCommodityGlobal, aes(x= gini, y=rebasedKcal, color=commodity)) +
  geom_point()+
  labs(x = "Average Global Gini Coefficient",
       y = "Demand (kcal/cap/day)") + 
  theme_minimal()+
  facet_wrap(~commodity, scales = "free")+
  theme(legend.position="none")+
  theme(plot.title = element_text(hjust = 0.5, size = 14), text=element_text(size = 12))

