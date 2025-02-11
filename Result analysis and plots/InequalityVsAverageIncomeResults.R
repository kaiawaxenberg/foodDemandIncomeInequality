library(ggplot2)
library(dplyr)
library(tidyr)
library(stringr)

##function to rename ensembles for plotting 
readAndSplitCols = function(filename, colToSplit, colForNewNames){
  
  dt = read.csv(filename) %>%
    separate(colToSplit, into=c(colForNewNames, colToSplit), sep="_") %>%
    mutate(colForNewNames = as.factor(colForNewNames))
  dt
}

##########Global Land Cover#############

##Read in files
lcInequality =readAndSplitCols(filename='Data/results/inequality/lc_concat.txt', colToSplit='Scenario',colForNewNames='Ensemble')%>% mutate(sce = "Inequality")
lcNoInequality =readAndSplitCols(filename='Data/results/no_inequality/lc_concat.txt', colToSplit='Scenario',colForNewNames='Ensemble')%>% mutate(sce = "No inequality")
lc = rbind(lcInequality, lcNoInequality)
lc_combined = lc %>% group_by(Ensemble, sce, Year) %>% summarise(sdCropland = sd(Cropland), sdPasture = sd(Pasture), sdIrrigCrop=sd(IrrigCrop), sdFertCrop = sd(FertCrop), 
                                           Cropland = mean(Cropland), Pasture = mean(Pasture), IrrigCrop=mean(IrrigCrop), FertCrop = mean(FertCrop))

#Calculate difference between runs with and without inequality
final = lc %>% filter(Year ==2100)
diffResults = data.frame(Ensemble = character(0), diffFert = numeric(0), diffPasture = numeric(0), 
                         diffIrrig = numeric(0), diffCrop= numeric(0), diffArea = numeric(0))
for(SSP in unique(lc_combined$Ensemble)){
  ineq = lc_combined %>% filter(Year == 2100, Ensemble==SSP, sce =="Inequality") %>% mutate(agArea = sum(Cropland, Pasture))
  no_ineq = lc_combined %>% filter(Year == 2100, Ensemble==SSP, sce =="No inequality") %>% mutate(agArea = sum(Cropland, Pasture))
  
  diffFert = 100 * (mean(ineq$FertCrop) - mean(no_ineq$FertCrop)) /mean(no_ineq$FertCrop)
  diffCrop = 100 * (mean(ineq$Cropland) - mean(no_ineq$Cropland)) /mean(no_ineq$Cropland)
  diffPasture = 100 * (mean(ineq$Pasture) - mean(no_ineq$Pasture)) /mean(no_ineq$Pasture)
  diffIrrig = 100 * (mean(ineq$IrrigCrop) - mean(no_ineq$IrrigCrop)) /mean(no_ineq$IrrigCrop)
  diffArea = 100 * (mean(ineq$agArea) - mean(no_ineq$agArea)) /mean(no_ineq$agArea)
  
  
  diffResults = diffResults %>% add_row(Ensemble = SSP, diffFert =diffFert , diffIrrig = diffIrrig,
                                  diffCrop = diffCrop, diffPasture = diffPasture, diffArea=diffArea)
}

#Function for comparison plots
plotLcWithSd = function(df, column, sdColumn, name) {
  ggplot(data=df, aes(x=Year, y = column, color = sce)) +
    geom_line(aes(y = column, color = sce), linewidth = 1) + 
    geom_ribbon(aes(y = column, ymin = column - sdColumn, ymax = column + sdColumn, fill = sce), alpha = .2)+
    labs(x = "Year",
         y = paste(name),
         color = "sce", 
         fill = "sce")+
    theme_minimal()+
    facet_wrap(~Ensemble)+
    theme(strip.text.x = element_text(size = 11))+
    scale_color_hue(labels=c("No inequality" = "Average Income", "Inequality"="Inequality"))+
    scale_fill_hue(labels=c("No inequality" = "Average Income", "Inequality"="Inequality"))+
    labs(fill = "Modelling approach", color = "Modelling approach")+
    #ylim(0,NA)+
    theme(panel.border = element_rect(color = "lightgray", fill = NA))+
    theme(plot.title = element_text(hjust = 0.5, size = 14), text=element_text(size = 11))
}

#Plot differences in land use outputs across model runs
plotLcWithSd(lc_combined, lc_combined$Cropland, lc_combined$sdCropland, "Cropland Area (Mha)")
plotLcWithSd(lc_combined, lc_combined$Pasture, lc_combined$sdPasture, "Pasture Area (Mha)")
plotLcWithSd(lc_combined, lc_combined$FertCrop, lc_combined$sdFertCrop, "Nitrogen Application (Mt)")
plotLcWithSd(lc_combined, lc_combined$IrrigCrop, lc_combined$sdIrrigCrop, "Irrigation Water Withdrawal (km3)")


#Plot of land data for SI
plotData = lc_combined %>% pivot_longer(cols = c(Cropland, Pasture, IrrigCrop, FertCrop), names_to = "Variable", values_to = "value")%>%
  pivot_longer(cols = c(sdCropland,sdPasture,sdIrrigCrop,sdFertCrop),names_to = "Variable_SD",values_to = "sd") %>%
  mutate(Variable_SD = gsub("sd", "", Variable_SD)) %>%
  filter(Variable == Variable_SD) %>% 
  select(-Variable_SD) %>% mutate(Variable = case_when(Variable == "Cropland"~"Cropland area (Mha)",
                                                       Variable == "Pasture"~"Pasture area (Mha)",
                                                         Variable == "FertCrop"~"Nitrogen application (Mt)",
                                                         Variable == "IrrigCrop"~"Irrigation water withdrawal (km3)"))%>%
  mutate(Variable = factor(Variable, levels = c("Cropland area (Mha)", "Pasture area (Mha)", "Nitrogen application (Mt)", "Irrigation water withdrawal (km3)")))

ggplot(data=plotData, aes(x=Year, y = value, linetype = sce, color = Ensemble)) +
  geom_line(linewidth=0.7) + 
  geom_ribbon(aes(y = value, ymin = value - sd, ymax = value + sd, fill = Ensemble), color=NA, alpha = .06)+
  labs(x = "Year",
       y ="Model projection value")+
  theme_minimal()+
  facet_wrap(~Variable, scales = "free_y")+
  scale_color_brewer(labels=c("No inequality" = "Average Income", "Inequality"="Inequality"),palette="Set1")+
  scale_fill_brewer(labels=c("No inequality" = "Average Income", "Inequality"="Inequality"),palette="Set1")+
  labs(fill = "SSP scenario", color = "SSP scenario",linetype="Model approach")+
  #ylim(0,NA)+
  theme(panel.border = element_rect(color = "lightgray", fill = NA))+
  theme(
    strip.text = element_text(size = 15),
    text = element_text(size=14),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12),
    legend.text = element_text(size = 14))

########Compare total Global Demand#############

##read in files
demandInequality =readAndSplitCols(filename='Data/results/inequality/globalKcal_concat.txt', colToSplit='Scenario',colForNewNames='Ensemble')%>% mutate(sce = "Inequality")
demandNoInequality =readAndSplitCols(filename='Data/results/no_inequality/globalKcal_concat.txt', colToSplit='Scenario',colForNewNames='Ensemble')%>% mutate(sce = "No inequality")
demand = rbind(demandInequality, demandNoInequality)

#Combine data accross Monte Carlo Scenarios
totalDemand_combined = demand %>% 
  group_by(Ensemble, sce, Scenario, Year) %>% summarise (rebasedKcal = sum(rebasedKcal)) %>%
  group_by(Ensemble, sce, Year) %>% summarise(sdKcal = sd(rebasedKcal), meanKcal = mean(rebasedKcal))

#Plot difference in total Kcal demand 
plotLcWithSd(totalDemand_combined, totalDemand_combined$meanKcal, totalDemand_combined$sdKcal, " Food Demand (kcal/cap/day)")

#Calculate differences in results between model runs
diff = function(df){
  diff = df %>% filter(Year ==2100) %>% 
    group_by(Ensemble) %>% arrange(sce, .by_group=TRUE) %>%
    summarise(diff = 100*(meanKcal[1] - meanKcal[2])/meanKcal[2])
  return(diff)
}
diffTotal = diff(totalDemand_combined)

########Compare Global Commodity Demand#############

#Create tables for demand by commodity
animalProds = demand %>% filter(Commodity == "Ruminants" | Commodity == "Monogastrics") %>% group_by(Ensemble, sce, Scenario, Year) %>% 
  summarise (rebasedKcal = sum(rebasedKcal)) %>% group_by(Ensemble, sce, Year) %>% 
  summarise(sdKcal = sd(rebasedKcal), meanKcal = mean(rebasedKcal))
commodityDemand = demand %>% group_by(Ensemble, sce, Year, Commodity) %>% 
  summarise(sdKcal = sd(rebasedKcal), meanKcal = mean(rebasedKcal)) 
fruitVeg = commodityDemand %>% filter(Commodity == "FruitVeg")
staples = commodityDemand %>% filter(Commodity == "CerealsStarchyRoots")
sugar = commodityDemand %>% filter(Commodity == "Sugar")

#Plot differences in commodity demand across model runs
plotLcWithSd(sugar, sugar$meanKcal, sugar$sdKcal, "Sugar Demand (kcal/cap/day)")
plotLcWithSd(staples, staples$meanKcal, staples$sdKcal, "Cereal and Starchy Root Demand (kcal/cap/day)")
plotLcWithSd(fruitVeg, fruitVeg$meanKcal, fruitVeg$sdKcal, "Fruit and Veg Demand (kcal/cap/day)")
plotLcWithSd(animalProds, animalProds$meanKcal, animalProds$sdKcal, "Animal product demand (kcal/cap/day)")

#Calculate differences in commodity demand between model runs
diffAnimalProds = diff(animalProds)
diffFruitVeg = diff(fruitVeg)
diffStaples = diff(staples)
diffSugar = diff(sugar)

######## Generate Figure 3 ############
plotData = commodityDemand %>%
  filter(Commodity %in% c("Ruminants", "Monogastrics")) %>%
  group_by(Ensemble, Year, sce) %>%
  summarise(meanKcal=sum(meanKcal), sdKcal = sqrt(sum((sdKcal)^2))) %>%
  mutate(Commodity="Animal products") %>%
  rbind(totalDemand_combined %>% mutate(Commodity="All commodities"))%>%
  mutate(sce = case_when(sce == "No inequality"~ "AverageIncome",
                         sce=="Inequality" ~ "IncomeDeciles")) %>%
  pivot_wider(names_from=sce, values_from=c(meanKcal, sdKcal)) %>%
  mutate(percDiff=100*(meanKcal_IncomeDeciles - meanKcal_AverageIncome)/meanKcal_AverageIncome) %>%
  mutate(percDiffSD = sqrt((sdKcal_IncomeDeciles / meanKcal_AverageIncome)^2 + ((meanKcal_IncomeDeciles - meanKcal_AverageIncome) * sdKcal_AverageIncome / meanKcal_AverageIncome^2)^2) * 100) %>%
  mutate(diff=(meanKcal_IncomeDeciles - meanKcal_AverageIncome)) %>%
  mutate(diffSD = sqrt((sdKcal_IncomeDeciles)^2 + (sdKcal_AverageIncome)^2))

ggplot(data=plotData, aes(x=Year, y = percDiff, color=Commodity, fill=Commodity)) +
  geom_line(linewidth = 1) +
  geom_ribbon(aes(ymin = percDiff - percDiffSD, ymax = percDiff + percDiffSD), alpha = .2)+
  labs(x = "Year",
       y = "Difference in projected demand (%)",
       color = "sce", 
       fill = "sce")+
  geom_hline(yintercept = 0, color = "#333333", linetype="dashed")+
  theme_minimal()+
  theme(
    strip.text = element_text(size = 18), 
    text = element_text(size=18),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 14),
    legend.text = element_text(size = 16))+
  facet_wrap(~Ensemble)+
  scale_color_hue(labels=c("No inequality" = "Average Income", "Inequality"="Income deciles"))+
  scale_fill_hue(labels=c("No inequality" = "Average Income", "Inequality"="Income deciles"))+
  labs(fill = "Commodity", color = "Commodity")+
  theme(panel.border = element_rect(color = "lightgray", fill = NA))

######## Generate Figure 4 ############
plotData = totalDemand_combined %>% mutate(Commodity="All commodities") %>%
  rbind(commodityDemand) %>% filter(Year==2100) %>%
  mutate(sce = case_when(sce == "No inequality"~ "AverageIncome",
                         sce=="Inequality" ~ "IncomeDeciles")) %>%
  pivot_wider(names_from=sce, values_from=c(meanKcal, sdKcal)) %>%
  mutate(Commodity= case_when(Commodity=="CerealsStarchyRoots" ~ "Starchy staples",
                              Commodity =="FruitVeg" ~ "Fruit & Veg", 
                              TRUE ~ Commodity))%>%
  mutate(percDiff=100*(meanKcal_IncomeDeciles - meanKcal_AverageIncome)/meanKcal_AverageIncome) %>%
  mutate(percDiffSD = sqrt((sdKcal_IncomeDeciles / meanKcal_AverageIncome)^2 + ((meanKcal_IncomeDeciles - meanKcal_AverageIncome) * sdKcal_AverageIncome / meanKcal_AverageIncome^2)^2) * 100) %>%
  mutate(diff=(meanKcal_IncomeDeciles - meanKcal_AverageIncome)) %>%
  mutate(diffSD = sqrt((sdKcal_IncomeDeciles)^2 + (sdKcal_AverageIncome)^2))

ggplot(data=plotData, aes(x=Ensemble, y = percDiff ,fill = Commodity)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
  geom_hline(yintercept = 0, color = "black") +  # Darker horizontal line at y = 0
  geom_errorbar(aes(ymin = percDiff - percDiffSD, ymax = percDiff + percDiffSD), 
                position = position_dodge(width = 0.9), 
                width = 0.25, color="darkgrey") +
  labs(x = "Scenario",
       y = "Model Difference in 2100 (%)",
       fill = "Commodity")+
  scale_fill_brewer(palette="Set2")+
  theme_minimal()+
  facet_wrap(~Commodity, ncol=4)+
  theme(
    strip.text = element_text(size = 18),
    text = element_text(size=18),
    axis.text.x = element_text(size = 14),
    axis.text.y = element_text(size = 14))+
  theme(legend.position = "none")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

######## Compile SI data table ############
si = totalDemand_combined %>% mutate(Commodity="All commodities") %>%
  rbind(commodityDemand) %>% filter(Year ==2100) %>%
  mutate(sce = case_when(sce == "No inequality"~ "AvgIncome",
                         sce=="Inequality" ~ "Deciles"),
         meanKcal = round(meanKcal, 2), sdKcal = round(sdKcal,2)) %>%
  pivot_wider(names_from=sce, values_from=c(meanKcal, sdKcal)) %>%
  mutate(diff=100*(meanKcal_Deciles - meanKcal_AvgIncome)/meanKcal_Deciles) %>%
  mutate(diff=round(diff,2)) %>%
  select(-Year)
