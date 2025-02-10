library(dplyr)
library(ggplot2)
library(data.table)
require(maps)
library(mapdata)
library(scales)

###################### Mapping inequality and income inputs for SI ##############

######load general data##############
world_map = map_data("world")
map_countries = as.data.frame(unique(world_map$region))
countries = read.csv("Data/inputs/countries.csv")

########Map national income per capita ##############

ssp = read.csv("Data/inputs/ssp.csv")%>% filter(Year==2100 | Year ==2020) %>% left_join(countries, by="Iso3") %>% 
  mutate(Area = case_when(Area == "Bolivia (Plurinational State of)" ~ "Bolivia",
                          Area == "Brunei Darussalam" ~ "Brunei",
                          Area == "Democratic People's Republic of Korea" ~ "North Korea",
                          Area == "Republic of Korea" ~ "South Korea",
                          Area == "Republic of Moldova" ~ "Moldova",
                          Area == "United States of America" ~ "USA",
                          Area == "United Kingdom" ~ "UK",
                          Area == "Syrian Arab Republic" ~ "Syria",
                          Area == "Venezuela (Bolivarian Republic of)" ~ "Venezuela",
                          Area == "Turkiye" ~ "Turkey",
                          Area == "Russian Federation" ~ "Russia",
                          Area == "Viet Nam" ~ "Vietnam",
                          Area == "Cabo Verde" ~ "Cape Verde",
                          Area == "Congo" ~ "Republic of Congo",
                          Area == "Cote d'Ivoire" ~ "Ivory Coast",
                          Area == "Czechia" ~ "Czech Republic",
                          Area == "Eswatini" ~ "Swaziland",
                          Area == "Iran (Islamic Republic of)" ~ "Iran",
                          Area == "Lao People's Democratic Republic" ~ "Laos",
                          Area == "Saint Vincent and the Grenadines" ~ "Saint Vincent",
                          Area == "Trinidad and Tobago" ~ "Trinidad",
                          Area == "United Republic of Tanzania" ~ "Tanzania",
                             TRUE ~ Area)) %>%
  mutate(gdpPc = gdp/population)

#Check country mapping
unique(ssp$Area[which(!(ssp$Area %in% world_map$region))])
unique(world_map$region[which(!(world_map$region %in% ssp$Area))])

#Plot map of GDP/capita
map_data <- merge(world_map, ssp %>% filter(Year==2100), by.x = "region", by.y = "Area", all.x = TRUE)
ggplot() +
  geom_map( 
    data = map_data %>% filter(!is.na(SSP)), map = world_map, 
    aes(long, lat, map_id = region, fill= gdpPc)) +
  theme_void() +
  labs(title = "", legend = "", caption = "")+
  labs(title = "", fill = "GDP/cap (2005 USD)", 
       caption = "")+
  facet_wrap(~SSP)+
  scale_fill_gradient2(labels=label_comma(), low = "#f7fcfd", mid="#deebf7" , high ="#811b7c")+
  theme(plot.title = element_text(hjust = 0.5, size = 14), text=element_text(size = 14))

#################Plot map of inequality for each SSP########################

#Prepare inequality data
inequality = read.csv("Data/inputs/sspGinis.csv") %>% left_join(countries, join_by(ISO==Iso3))
regions = read.csv("Data/inputs/all.csv")
inequality = left_join(inequality, regions, join_by(ISO == alpha.3)) %>% select(ISO, sce, year, gini, sub.region, region, name, Area, PlumGroup) %>% 
  ungroup() %>% rename(countryName=name)
ssp = read.csv("Data/inputs/ssp.csv")

#Fill in missing country data
countriesToAdd = setdiff(ssp$Iso3, inequality_change$ISO)
for(scenario in unique(inequality_change$sce)){
  for(country in countriesToAdd){
    avgRegion = regions[which(regions$alpha.3==country),]$sub.region
    regionData = inequality %>% filter(sub.region == avgRegion, sce==scenario)
    regionAvgGini = mean(regionData$gini)
    inequality = inequality %>% add_row(ISO = country, sce = scenario, gini = regionAvgGini, sub.region = avgRegion)
  }
}
inequality = inequality %>% left_join(countries, join_by(ISO==Iso3))%>% 
  mutate(Area = ifelse(is.na(Area.x), Area.y, Area.x)) %>% 
  mutate(Area = case_when(Area == "Bolivia (Plurinational State of)" ~ "Bolivia",
                          Area == "Brunei Darussalam" ~ "Brunei",
                          Area == "Democratic People's Republic of Korea" ~ "North Korea",
                          Area == "Republic of Korea" ~ "South Korea",
                          Area == "Republic of Moldova" ~ "Moldova",
                          Area == "United States of America" ~ "USA",
                          Area == "United Kingdom" ~ "UK",
                          Area == "Syrian Arab Republic" ~ "Syria",
                          Area == "Venezuela (Bolivarian Republic of)" ~ "Venezuela",
                          Area == "Turkiye" ~ "Turkey",
                          Area == "Russian Federation" ~ "Russia",
                          Area == "Viet Nam" ~ "Vietnam",
                          Area == "Cabo Verde" ~ "Cape Verde",
                          Area == "Congo" ~ "Republic of Congo",
                          Area == "Cote d'Ivoire" ~ "Ivory Coast",
                          Area == "Czechia" ~ "Czech Republic",
                          Area == "Eswatini" ~ "Swaziland",
                          Area == "Iran (Islamic Republic of)" ~ "Iran",
                          Area == "Lao People's Democratic Republic" ~ "Laos",
                          Area == "Saint Vincent and the Grenadines" ~ "Saint Vincent",
                          Area == "Trinidad and Tobago" ~ "Trinidad",
                          Area == "United Republic of Tanzania" ~ "Tanzania",
                          TRUE ~ Area))

#Check country mapping
unique(inequality$Area[which(!(inequality_change$Area %in% world_map$region))])
unique(world_map$region[which(!(world_map$region %in% inequality$Area))])

#Plot a map of gini coefficients in 2100
map_data <- merge(world_map, inequality %>% filter(year==2100), by.x = "region", by.y = "Area", all.x = TRUE) %>% filter(!is.na(sce))
ggplot() +
  geom_map( 
    data = map_data, map = world_map, 
    aes(long, lat, map_id = region, fill= gini)) +
  scale_fill_distiller(palette="BuPu", direction=1)+
  theme_void() +  
  facet_wrap(~sce)+
  labs(title = "", fill = "Gini coefficient", 
       caption = "")+
  theme(plot.title = element_text(hjust = 0.5, size = 20), text=element_text(size = 15))

#Plot a map on change in Gini coefficients from 2020-2100
inequality_change = inequality %>% group_by(Area, sce, ISO) %>%
  summarise(giniChange = gini[year==2100] - gini[year==2020])
map_data <- merge(world_map, inequality_change, by.x = "region", by.y = "Area", all.x = TRUE) %>% filter(!is.na(sce))
ggplot() +
  geom_map( 
    data = map_data, map = world_map, 
    aes(long, lat, map_id = region, fill= giniChange)) +
  scale_fill_gradient2(low = "palegreen4", mid = "#f5f5f5", high = "red3") +  # Adjust color scale
  theme_void() +  
  #theme(panel.background = element_rect(colour = "grey", size=0.1))+
  facet_wrap(~sce)+
  labs(title = "", fill = "Change in Gini", 
       caption = "")+
  theme(plot.title = element_text(hjust = 0.5, size = 20), text=element_text(size = 15))
