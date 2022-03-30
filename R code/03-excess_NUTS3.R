
#########################################################################
# Author: Ainhoa Elena Leger
# Last update: 29-03-2022
#########################################################################




###### Directory and packages ######

rm(list=ls())

dir.in <- ".../In"
dir.out <- ".../Out"

library(tidyverse)
library(ggplot2)
library(grid)
library(gtable)
library(writexl)
library(reshape2)
library(kableExtra)
library(matrixStats)
library(stringr)




###### Raw data ######

setwd(dir.in)

# Load file downloaded from Eurostat
eurostat <- read.table("demo_r_mwk3_10_1_Data.csv", header=TRUE, sep=",")




###### Data preparation ######

# Select desired variables and observations

eurostat <- eurostat %>%
  select(GEO, TIME, Value) %>%
  
  # Divide the variable TIME into two variables year and week 
  mutate(
    # Extract the first 4 digits (year)
    year = as.numeric(str_sub(TIME,1,4)),
    # Extract the last 2 digits removing the leading zeros (iso_week)
    iso_week = as.numeric(sub("^0+", "", str_sub(TIME,6,7)))) %>%
  
  # Rename and relocate the variables
  rename(region = GEO, observed_deaths = Value) %>%
  select(region, year, iso_week, observed_deaths) %>%
  filter(year %in% c(2013:2020))

# Recode as numeric the observed deaths 
# Remove the commas (for thousands)
eurostat$observed_deaths <- as.numeric(gsub(",", "", eurostat$observed_deaths))
unique(eurostat$observed_deaths)

# NA values are assigned where the value is missing
eurostat[is.na(eurostat$observed_deaths)==TRUE,]
# Mayotte 2013 is always missing -> delete Mayotte
eurostat <- eurostat[eurostat$region != "Mayotte",]



###### Create epi-years ######

# Use iso-weeks to construct the variable epi-years
# Each epi-year will have iso-weeks 27-52 (or 27-53 for leap years) 
# and iso-weeks 1-26 of the following year
# Example: epi-year 2009/2010 is defined by iso-weeks 27-53 of 2009
# and iso-weeks 1-26 of 2010

eurostat$epi_year=0

for(i in c(1:dim(eurostat)[1])){
  
  ## Years with 53 weeks
  if (eurostat$year[i] %in% c(2015, 2020)) {
    if (eurostat$iso_week[i]<=26) {
      eurostat$epi_year[i] = paste0(eurostat$year[i]-1, "/", eurostat$year[i]) }   
    else if (eurostat$iso_week[i]>=27 & eurostat$iso_week[i]<=53) {
      eurostat$epi_year[i] = paste0(eurostat$year[i], "/", eurostat$year[i]+1) }
  }
  
  ## Years with 52 weeks
  else if (eurostat$year[i] %in% c(2012:2014, 2016:2019)) {
    if (eurostat$iso_week[i]<=26) {
      eurostat$epi_year[i] = paste0(eurostat$year[i]-1, "/", eurostat$year[i]) }
    else if (eurostat$iso_week[i]>=27 & eurostat$iso_week[i]<=52) {
      eurostat$epi_year[i] = paste0(eurostat$year[i], "/", eurostat$year[i]+1) }
  }
}




###### Later/earlier observed deaths (gregorian calendar) ######

# Compute D- and D+ by epi_year, sex, age_group and country

eurostat1314 <- eurostat %>% 
  filter(epi_year %in% c("2013/2014")) %>%
  group_by(epi_year, region) %>%
  summarize( earlier_obs_deaths = 
               sum( observed_deaths[which(iso_week %in% c(27:52))],
                    observed_deaths[which(iso_week %in% c(1:6))] ),
             later_obs_deaths = 
               sum( observed_deaths[which(iso_week %in% c(7:26))],
                    (1/7)*observed_deaths[which(iso_week == 26)]) ) %>%
  ungroup()

eurostat1415 <- eurostat %>% 
  filter(epi_year %in% c("2014/2015")) %>%
  group_by(epi_year, region) %>%
  summarize( earlier_obs_deaths = 
               sum( (6/7)*observed_deaths[which(iso_week == 27)],
                    observed_deaths[which(iso_week %in% c(28:52))],
                    observed_deaths[which(iso_week %in% c(1:6))],
                    (1/7)*observed_deaths[which(iso_week == 6)] ),
             later_obs_deaths = 
               sum( (6/7)*observed_deaths[which(iso_week == 7)],
                    observed_deaths[which(iso_week %in% c(8:26))],
                    (2/7)*observed_deaths[which(iso_week == 26)]) ) %>%
  ungroup()

eurostat1516 <- eurostat %>% 
  filter(epi_year %in% c("2015/2016")) %>%
  group_by(epi_year, region) %>%
  summarize( earlier_obs_deaths = 
               sum( (5/7)*observed_deaths[which(iso_week == 27)],
                    observed_deaths[which(iso_week %in% c(28:53))],
                    observed_deaths[which(iso_week %in% c(1:5))],
                    (2/7)*observed_deaths[which(iso_week == 6)]),
             later_obs_deaths = 
               sum( (5/7)*observed_deaths[which(iso_week == 7)],
                    observed_deaths[which(iso_week %in% c(7:25))],
                    (3/7)*observed_deaths[which(iso_week == 26)]) ) %>%
  ungroup()

eurostat1617 <- eurostat %>% 
  filter(epi_year %in% c("2016/2017")) %>%
  group_by(epi_year, region) %>%
  summarize( earlier_obs_deaths = 
               sum( (3/7)*observed_deaths[which(iso_week == 27)],
                    observed_deaths[which(iso_week %in% c(27:52))],
                    observed_deaths[which(iso_week %in% c(1:5))],
                    (4/7)*observed_deaths[which(iso_week == 6)] ),
             later_obs_deaths = 
               sum( (3/7)*observed_deaths[which(iso_week == 7)],
                    observed_deaths[which(iso_week %in% c(7:25))],
                    (5/7)*observed_deaths[which(iso_week == 26)]) ) %>%
  ungroup()

eurostat1718 <- eurostat %>% 
  filter(epi_year %in% c("2017/2018")) %>%
  group_by(epi_year, region) %>%
  summarize( earlier_obs_deaths = 
               sum( (2/7)*observed_deaths[which(iso_week == 27)],
                    observed_deaths[which(iso_week %in% c(27:52))],
                    observed_deaths[which(iso_week %in% c(1:5))],
                    (5/7)*observed_deaths[which(iso_week == 6)] ),
             later_obs_deaths = 
               sum( (2/7)*observed_deaths[which(iso_week == 7)],
                    observed_deaths[which(iso_week %in% c(7:25))],
                    (6/7)*observed_deaths[which(iso_week == 26)]) ) %>%
  ungroup()

eurostat1819 <- eurostat %>% 
  filter(epi_year %in% c("2018/2019")) %>%
  group_by(epi_year, region) %>%
  summarize( earlier_obs_deaths = 
               sum( (1/7)*observed_deaths[which(iso_week == 27)],
                    observed_deaths[which(iso_week %in% c(27:52))],
                    observed_deaths[which(iso_week %in% c(1:5))],
                    (6/7)*observed_deaths[which(iso_week == 6)]),
             later_obs_deaths = 
               sum( (1/7)*observed_deaths[which(iso_week == 7)],
                    observed_deaths[which(iso_week %in% c(7:26))]) ) %>%
  ungroup()

eurostat1920 <- eurostat %>% 
  filter(epi_year %in% c("2019/2020")) %>%
  group_by(epi_year, region) %>%
  summarize( earlier_obs_deaths = 
               sum( observed_deaths[which(iso_week %in% c(27:52))],
                    observed_deaths[which(iso_week %in% c(1:6))] ),
             later_obs_deaths = 
               sum( observed_deaths[which(iso_week %in% c(7:26))],
                    (1/7)*observed_deaths[which(iso_week == 26)]) ) %>%
  ungroup()

# Combine all the epi-years
eurostat <- bind_rows(eurostat1314, eurostat1415, eurostat1516, eurostat1617, 
                      eurostat1718, eurostat1819, eurostat1920)




###### Later/earlier ratios ######

# Compute ratios D+/D-, average, sd and cv of upsilon 
# by region (excluding year 2019/2020)

eurostat1 <- eurostat %>%
  mutate( upsilon = later_obs_deaths/earlier_obs_deaths) %>%
  group_by(region) %>%
  mutate(av_upsilon = mean(upsilon[which(epi_year!="2019/2020")]),
         sd_upsilon = sd(upsilon[which(epi_year!="2019/2020")]),
         cv_upsilon = sd_upsilon/av_upsilon,
         lab = paste("mean =", round(av_upsilon,2), ",",
                     "sd =", round(sd_upsilon,2), 
                     "\ncv =", round(cv_upsilon,4)*100, "%")) %>%
  distinct(region, epi_year, .keep_all = TRUE) %>%
  select(c(region, epi_year,
           earlier_obs_deaths, later_obs_deaths, 
           upsilon, av_upsilon, sd_upsilon, cv_upsilon, lab))

# eurostat1
# # A tibble: 1,113 x 9
# # Groups:   region [159]
#    region          epi_year earlier_obs_deat~ later_obs_deaths upsilon av_upsilon sd_upsilon
#    <chr>           <chr>                <dbl>            <dbl>   <dbl>      <dbl>      <dbl>
#  1 A Coruña        2013/20~              7322            4800.   0.656      0.651    0.0174 
#  2 Ain             2013/20~              2379            1489.   0.626      0.623    0.0224 
#  3 Aisne           2013/20~              3164            1999.   0.632      0.627    0.0315 
#  4 Albacete        2013/20~              2208            1442.   0.653      0.641    0.0338 
#  5 Alicante/Alaca~ 2013/20~              8843            5742.   0.649      0.630    0.0153 
#  6 Allier          2013/20~              2579            1660.   0.643      0.652    0.0479 
#  7 Almería         2013/20~              2887            1817.   0.629      0.636    0.00924
#  8 Alpes-de-Haute~ 2013/20~               981             644.   0.657      0.654    0.0633 
#  9 Alpes-Maritimes 2013/20~              6993            4450.   0.636      0.609    0.0255 
# 10 Araba/Álava     2013/20~              1630             937.   0.575      0.598    0.0320 
# # ... with 1,103 more rows, and 2 more variables: cv_upsilon <dbl>, lab <chr>




###### Labels NUTS3 #####

# Create the variable country from the NUTS3 regions
eurostat1 <- eurostat1 %>%
  mutate(country = ifelse(region %in% c("Ain","Aisne","Allier","Alpes-de-Haute-Provence",
                                        "Alpes-Maritimes","Ardèche","Ardennes","Ariège",
                                        "Aube","Aude","Aveyron","Bas-Rhin","Bouches-du-Rhône",
                                        "Calvados","Cantal","Charente","Charente-Maritime",
                                        "Cher","Corrèze","Corse-du-Sud","Côte-d'Or",
                                        "Côtes-d'Armor","Creuse","Deux-Sèvres","Dordogne",
                                        "Doubs","Drôme","Essonne","Eure","Eure-et-Loir",
                                        "Finistère","Gard","Gers","Gironde","Guadeloupe",
                                        "Guyane","Haut-Rhin","Haute-Corse","Haute-Garonne",
                                        "Haute-Loire","Haute-Marne","Haute-Saône","Haute-Savoie",
                                        "Haute-Vienne","Hautes-Alpes","Hautes-Pyrénées",
                                        "Hauts-de-Seine","Hérault","Ille-et-Vilaine","Indre",
                                        "Indre-et-Loire","Isère","Jura","La Réunion","Landes",
                                        "Loir-et-Cher","Loire","Loire-Atlantique","Loiret",
                                        "Lot","Lot-et-Garonne","Lozère","Maine-et-Loire",
                                        "Manche","Marne","Martinique","Mayenne",
                                        "Meurthe-et-Moselle","Meuse","Morbihan","Moselle",
                                        "Nièvre","Nord","Oise","Orne","Paris","Pas-de-Calais",
                                        "Puy-de-Dôme","Pyrénées-Atlantiques","Pyrénées-Orientales",
                                        "Rhône","Saône-et-Loire","Sarthe","Savoie",
                                        "Seine-et-Marne","Seine-Maritime","Seine-Saint-Denis",
                                        "Somme","Tarn","Tarn-et-Garonne","Territoire de Belfort",
                                        "Val-d'Oise","Val-de-Marne","Var","Vaucluse","Vendée",
                                        "Vienne","Vosges","Yonne","Yvelines"), "France", 
                          ifelse(region %in% c("A Coruña","Albacete","Alicante/Alacant",
                                               "Almería","Asturias","Araba/Álava","Ávila",
                                               "Badajoz","Barcelona","Bizkaia","Burgos",
                                               "Cáceres","Cádiz","Cantabria",
                                               "Castellón/Castelló","Ceuta","Ciudad Real",
                                               "Córdoba","Cuenca","Eivissa, Formentera",
                                               "El Hierro","Fuerteventura","Gipuzkoa",
                                               "Girona","Gran Canaria", "Granada","Guadalajara",
                                               "Huelva","Huesca","Jaén","La Gomera","La Palma",
                                               "La Rioja","Lanzarote","León","Lleida","Lugo",
                                               "Madrid","Málaga","Mallorca","Melilla","Menorca",
                                               "Murcia","Navarra","Ourense","Palencia",
                                               "Pontevedra","Salamanca","Segovia","Sevilla","Soria",
                                               "Tarragona","Tenerife","Teruel","Toledo",
                                               "Valencia/València","Valladolid",
                                               "Zamora","Zaragoza"), "Spain", NA)))

# Convert to factors
eurostat1$country <- as.factor(eurostat1$country)




###### FIGURES ratios ######

# Create a function to plot the later/earlier ratios
# The input is the database (i.e., filtered by sex and country)

figure1 <- function(data) {
  
  data %>% 
    filter(epi_year != "2019/2020") %>%
    ggplot(aes(upsilon, epi_year, colour = region)) + 
    geom_point(size = 3) + 
    theme_bw() + 
    facet_wrap(~region, ncol = 15) +   
    labs(x = "Later/earlier ratio", y = "Epiyear", color = "Age group") +
    theme(
      # Axis 
      axis.title.x = element_text(size = 14, face = "bold"),
      axis.title.y = element_text(size = 14, face = "bold"),
      axis.text.x = element_text(size = 11, angle = 45, vjust = 1, hjust = 1),
      axis.text.y = element_text(size = 11),
      # Legend
      legend.title = element_blank(),
      legend.position = "none",
      # Facet
      strip.text.x = element_text(size = 11),
      strip.text.y = element_text(size = 11)) +
    geom_vline(data = data, aes(xintercept = av_upsilon, colour = region),
               size = 2, alpha = 0.5, show.legend = FALSE) +
    geom_text(data = data, aes(label = lab, fontface = "plain"),
              x = 0.7, y = -0.01, size = 2.5, stat = "unique", 
              lineheight = 0.7) +
    geom_segment(aes(x = upsilon, xend = av_upsilon,
                     y = epi_year, yend = epi_year),
                 size = 0.7) +
    expand_limits(y = c(-0.7, 7))
}

ratio.fr <- eurostat1 %>% filter(country == "France")
figure1(ratio.fr)

ratio.sp <- eurostat1 %>% filter(country == "Spain")
figure1(ratio.sp)







###### Excess deaths by age and sex ######

# Compute expected dx and excess dx for year 2019/2020 by region
eurostat2 <- eurostat1 %>% 
  group_by(region) %>%
  mutate(later_exp_deaths = 
           ifelse(epi_year=="2019/2020", round(earlier_obs_deaths*av_upsilon),
                  ifelse(epi_year!="2019/2020", NA, 999)),
         excess_deaths = 
           ifelse(epi_year=="2019/2020", later_obs_deaths - later_exp_deaths,
                  ifelse(epi_year!="2019/2020", NA, 999))) %>%
  ungroup() %>%
  # Subset to later segment data for 2019
  filter(epi_year == "2019/2020") %>%
  distinct(region, .keep_all = TRUE) %>%
  select(c(region, country, 
           later_obs_deaths, later_exp_deaths, excess_deaths))

# eurostat2
# # A tibble: 159 x 5
#   region                  country later_obs_deaths later_exp_deaths excess_deaths
#   <chr>                   <fct>              <dbl>            <dbl>         <dbl>
# 1 A Coruña                Spain              4992.             4973          18.6
# 2 Ain                     France             1822.             1690         132. 
# 3 Aisne                   France             2579.             2091         488. 
# 4 Albacete                Spain              2448.             1475         973. 
# 5 Alicante/Alacant        Spain              6665.             6180         485. 
# 6 Allier                  France             1733.             1706          27.3
# 7 Almería                 Spain              2056.             2069         -12.6
# 8 Alpes-de-Haute-Provence France              663.              722         -59.3
# 9 Alpes-Maritimes         France             4678.             4629          49.4
# 10 Araba/Álava             Spain              1426.             1011         415. 
# # ... with 149 more rows




###### TABLES excess ######

###### Excess death

eurostat4 <- eurostat2 %>%
  mutate(risk_obs = round((excess_deaths/later_obs_deaths)*100,1),
         risk_exp = round((excess_deaths/later_exp_deaths)*100,1))

eurostat4[,c(3,5)] <- round(eurostat4[,c(3,5)])
eurostat4 <- as.data.frame(eurostat4)

###### Spain

eurostat.sp <- eurostat4 %>% filter(country == "Spain")

###### France

eurostat.fr <- eurostat4 %>% filter(country == "France")



