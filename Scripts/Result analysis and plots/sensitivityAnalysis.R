library(dplyr)
library(tidyr)
library(ggplot2)
library(DescTools)

## Load results for all scenarios
scenarios = c("plus0.1", "plus0.05", "minus0.1", "minus0.05", "noChange")
resultsDir = "Data/results/sensitivity"
lc = data.frame()
countryDemandOpt = data.frame()
for (scenario in scenarios){
  lcTemp =  read.csv(file.path(resultsDir, scenario, "lc.txt")) %>% mutate(Scenario=scenario)
  demandOptTemp =  read.csv(file.path(resultsDir, scenario, "countryDemandOpt.txt")) %>% mutate(Scenario=scenario)

  lc=rbind(lc, lcTemp)
  countryDemandOpt=rbind(countryDemandOpt, demandOptTemp)

  rm(demandOptTemp, lcTemp)
}

## Plot global food demand by sensitivity scenario
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

## Calculate differences between the control and scenarios (SI 6)
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

## Create demand tables including change in demand from the control
countryIncomeKcal = countryDemandOpt %>% 
  group_by(Country, Year, decile, commodity) %>% 
  mutate(refKcal = rebasedKcal[which(Scenario=="noChange")]) %>% 
  mutate(diff = rebasedKcal - refKcal) %>% 
  mutate(percDiff = 100* diff/refKcal)

## Load baseline gini coefficients from inequalityDataPrep.R
countries = read.csv("Data/input/countries.csv")
giniBaseline = read.csv("Data/input/giniBaseline.csv") %>%
  left_join(countries, join_by(ISO==Iso3)) %>% 
  group_by(Area) %>% 
  summarise(startGini=mean(gini)) %>% 
  dplyr::select(Area, startGini)

## Combine gini coefficient and demand data
kcalWithInitialGini = left_join(countryIncomeKcal, giniBaseline, join_by(Country==Area))

## Compute final gini coefficients based on sensitivity scenarios
kcalWithGini = kcalWithInitialGini %>% 
  mutate(change = as.double(gsub("[^0-9.-]", "", Scenario))) %>%
  mutate(direction = ifelse((Scenario=="minus0.1" | Scenario =="minus0.05"), -1, 1)) %>%
  mutate(change = change * direction)%>%
  mutate(gini = startGini + (change/50)*(Year-2020)) %>% filter(!is.na(gini))

## Plot global commodity demand vs average intranational inequality (SI 6)
kcalWithGiniCommodityGlobal = kcalWithGini %>% group_by(commodity, Scenario, Year) %>% 
  summarise(gini = weighted.mean(gini, population), 
            rebasedKcal = weighted.mean(rebasedKcal, population))
ggplot(data=kcalWithGiniCommodityGlobal, aes(x= gini, y=rebasedKcal, color = Scenario)) +
  geom_point()+
  labs(x = "Average Global Gini Coefficient",
       y = "Demand (kcal/cap/day)") + 
  theme_minimal()+
  facet_wrap(~commodity, scales = "free")+
  theme(legend.position="none")+
  theme(plot.title = element_text(hjust = 0.5, size = 14), text=element_text(size = 12))

