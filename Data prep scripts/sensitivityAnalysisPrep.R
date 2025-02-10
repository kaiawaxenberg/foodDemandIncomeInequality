library(dplyr)
library(ggplot2)
library(data.table)

########R script to translate income distribution by decile to Gini coefficients and back again##############
#adapted from Narayan et al, 2023

#Load baseline income distribution
decileShares = read.csv("Data/inputs/narayan_deciles.csv") %>% mutate(decile = as.double(substring(Category, 2)))
decileBaseline = decileShares %>% filter(year==2020, sce=="SSP2")

#Function to compute gini coefficient from income shares by decile
#input data must have columns "decile", "pred_shares", "ISO", "year", "ssp"
compute_gini_deciles<- function(df){
    
    df = df %>% 
    mutate(share_of_richer_pop = 1 - 0.1*decile) %>% 
     mutate(score = pred_shares *(0.1+ (2*share_of_richer_pop))) %>%   
     group_by(ISO, year) %>% 
     mutate(gini= 1- sum(score)) %>% # the gini coefficient is 1 - the area under the lorenz curve
      ungroup()
           
     return(df)}

#Translate baseline income distributions to Gini coefficients and save as a file
giniBaseline = compute_gini_deciles(decileBaseline)%>% na.omit()%>% 
  select(ISO, year, gini) %>% distinct()
write.csv(giniBaseline, file = "Data/inputs/giniBaseline.csv", row.names=FALSE)

#Compute average % change in the Gini coefficient represented by a 0.1 increase
giniChange = giniBaseline %>%mutate(percChange=100*0.1/gini) 
mean(giniChange$percChange)

#Fill in baseline Gini for missing countries
regions = read.csv("Data/inputs/all.csv")
ssp = read.csv("Data/inputs/ssp.csv") %>% filter(Year==2020, SSP=="SSP2") %>% mutate(gdpPc = gdp/population)
giniBaseline = left_join(giniBaseline, regions, join_by(ISO == alpha.3)) %>% select(ISO, year, gini, sub.region)
for(country in setdiff(ssp$Iso3, giniBaseline$ISO)){
    avgRegion = regions[which(regions$alpha.3==country),]$sub.region
    regionData = giniBaseline %>% filter(sub.region == avgRegion)
    regionAvgGini = mean(regionData$gini)
    giniBaseline = giniBaseline %>% add_row(year = 2020, ISO = country, gini = regionAvgGini, sub.region = avgRegion)
}

#Compute new Gini coefficients for sensitvity analysis
changes = c(+0.05,+0.1,-0.1,-0.05) #change this value for sensitivity analysis
yrs = seq(2021, 2070)
countries = unique(giniBaseline$ISO)
for(change in changes){
  giniData = giniBaseline
  for (country in countries){
    initialGini = giniData[which(giniData$year ==2020 & giniData$ISO == country),]$gini
    finalGini = initialGini + change
    for (yr in yrs){
      #alter gini coefficient linearly over 50 years
      ratio = (yr - (head(yrs, n=1) -1)) / (tail(yrs, n=1) - (head(yrs, n=1) -1))
      giniData = giniData %>% add_row(year = yr, ISO = country, gini = (ratio*finalGini + (1- ratio)* initialGini))
    }
  }
  #join GDP per capita data
  giniData = giniData %>% left_join(ssp, join_by(ISO==Iso3)) %>% select(ISO, year, gini, gdpPc) %>%
    mutate(id = paste0(ISO,year)) %>% na.omit()
  #Save data table to the environment
  file_name= case_when(change==+0.05~"giniPlus0.05",
                       change==-0.05~"giniMinus0.05",
                       change==+0.1~"giniPlus0.1",
                       change==-0.1~"giniMinus0.1")
  assign(file_name, giniData)
}

#Check a linear increase/decrease has been achieved
ggplot(data=giniMinus0.1,aes(x=year,y=gini))+
  geom_smooth()

#Functions to compute income distribution from gini coefficient, assuming lognormal distribution
#From Narayan, 2023
erfinv <- function (x) qnorm((1 + x)/2)/sqrt(2)
compute_lognormal_dist <- function(mean_income, gini, max_income, len_sample){
  
  sd <- 2 * erfinv(gini)
  m <- log(mean_income) -((sd^2)/2)  
  
  draws3 <- dlnorm(seq(0, max_income, length.out=len_sample), m, sd)
  
  draw_d <- as.data.frame(draws3)  %>% 
    mutate(gdpPc = seq(0, max_income, length.out=len_sample)) %>% 
    rename(density = draws3 )
  
  return(draw_d)
}
compute_lognormal_country <- function(df){
  mean <- df$gdpPc
  gini <- df$gini
  max_income <- df$gdpPc*8.54
  len_sample <- 2000

  results <- compute_lognormal_dist(mean_income = mean,
                                    gini = gini,
                                    max_income = max_income,
                                    len_sample = len_sample)
  results$ISO = df$ISO
  results$year = df$year
  return(results)
}
decileShares= function(df){
  downscaled_results = df %>% group_by(ISO, year) %>% 
    arrange(gdpPc) %>% 
    mutate(tot_density = sum(density),
           tot_income = sum(density * gdpPc),
           cut_off = tot_density * 0.1,
           cut_off_d1 = cut_off,
           cut_off_d2 = cut_off * 2,
           cut_off_d3 =  cut_off * 3,
           cut_off_d4 =  cut_off * 4,
           cut_off_d5 = cut_off * 5,
           cut_off_d6 =  cut_off * 6,
           cut_off_d7 =  cut_off * 7,
           cut_off_d8 =  cut_off * 8,
           cut_off_d9 =  cut_off * 9,
           cum_density = cumsum(density)) %>% ungroup() %>% 
    mutate(decile = if_else(cum_density < cut_off_d1, "d1",
                      if_else(cum_density < cut_off_d2, "d2",
                         if_else(cum_density < cut_off_d3, "d3",
                            if_else(cum_density < cut_off_d4, "d4",
                                if_else(cum_density < cut_off_d5, "d5",
                                  if_else(cum_density < cut_off_d6, "d6",
                                    if_else(cum_density < cut_off_d7, "d7",
                                      if_else(cum_density < cut_off_d8, "d8",
                                        if_else(cum_density < cut_off_d9, "d9","d10")))))))))) %>% 
    group_by(ISO, year, decile) %>% 
    mutate(gdp_decile = sum(density* gdpPc),
           shares = gdp_decile/tot_income,
           gdpPc_decile = sum(density* gdpPc)/sum(density)
    ) %>% ungroup() %>%
    #adapt downscaled results for input to PLUMv2
    select(ISO, year, decile, shares) %>% 
    arrange(ISO, year, decile) %>% distinct() %>% mutate(sce = "SSP2") %>% rename(Category = decile, pred_shares = shares) %>%
    relocate(sce, .after = year)
  return(downscaled_results)
}

#Compute downscaled income decile shares for each sensitvity scenario
downscaled_plus0.05 <- lapply(giniPlus0.05 %>% split(giniPlus0.05$id),compute_lognormal_country)%>%rbindlist()%>%decileShares()
downscaled_plus0.1 <- lapply(giniPlus0.1 %>% split(giniPlus0.1$id),compute_lognormal_country)%>%rbindlist()%>%decileShares()
downscaled_minus0.05 <- lapply(giniMinus0.05 %>% split(giniMinus0.05$id),compute_lognormal_country)%>%rbindlist()%>%decileShares()
downscaled_minus0.1 <- lapply(giniMinus0.1 %>% split(giniMinus0.1$id),compute_lognormal_country)%>%rbindlist()%>%decileShares()

#save files to input to PLUMv2
write.csv(downscaled_plus0.05, file = "Data/inputs/sensitivity_files/giniPlus0.0.05.csv", row.names=FALSE)
write.csv(downscaled_plus0.1, file = "Data/inputs/sensitivity_files/giniPlus0.1.csv", row.names=FALSE)
write.csv(downscaled_minus0.05, file = "Data/inputs/sensitivity_files/giniMinus0.05.csv", row.names=FALSE)
write.csv(downscaled_minus0.1, file = "Data/inputs/sensitivity_files/giniMinus0.1.csv", row.names=FALSE)
#IMPORTANT: Open and re-save these files locally to avoid errors with the comma separation in PLUMv2

#For SI mapts, compute and save gini coefficients by country for all SSPs, years, and countries
decileShares = read.csv("Data/inputs/narayan_deciles.csv") %>% mutate(decile = as.double(substring(Category, 2)))
giniAll = compute_gini_deciles(decileShares)%>% na.omit() %>% 
  select(ISO, year, sce, gini) %>% distinct()
write.csv(giniAll, file = "Data/inputs/sspGinis.csv", row.names=FALSE)
