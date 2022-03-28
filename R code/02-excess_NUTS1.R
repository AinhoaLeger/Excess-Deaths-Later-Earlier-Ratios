
#########################################################################
# Author: Ainhoa Elena Leger
# Last update: 27-03-2022
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
library(directlabels)




###### Raw data ######

setwd(dir.in)

# Load file downloaded from HMD
eurostat <- read.table("demo_r_mwk2_10_1_Data.csv", header=TRUE, sep=",")




###### Data preparation ######

# Select desired variables and observations

eurostat <- eurostat %>%
  select(GEO, TIME, SEX, AGE, Value) %>% 
  # Select the more detailed age classification:
  # all categories up to age 89 and then 90+
  filter(SEX %in% c("Females", "Males"),
         AGE %in% c("Less than 10 years",
                    "From 10 to 19 years",
                    "From 20 to 29 years",
                    "From 30 to 39 years",
                    "From 40 to 49 years",
                    "From 50 to 59 years",
                    "From 60 to 69 years",
                    "From 70 to 79 years",
                    "From 80 to 89 years",
                    "90 years or over")) %>%
  # Divide the variable TIME into two variables year and week 
  mutate(
    # Extract the first 4 digits (year)
    year = as.numeric(str_sub(TIME,1,4)),
    # Extract the last 2 digits removing the leading zeros (iso_week)
    iso_week = as.numeric(sub("^0+", "", str_sub(TIME,6,7)))) %>%
  
  # Rename and relocate the variables
  rename(region = GEO, sex = SEX) %>%
  select(region, sex, AGE, year, iso_week, Value) %>%
  filter(year %in% c(2013:2020))


# Recode age groups
# Create a new variable for the age groups 0-60, and then by 10 years
eurostat$age_group <- NA
eurostat$age_group[(eurostat$AGE == "Less than 10 years") |
                     (eurostat$AGE == "From 10 to 19 years") |
                     (eurostat$AGE == "From 20 to 29 years") |
                     (eurostat$AGE == "From 30 to 39 years") |
                     (eurostat$AGE == "From 40 to 49 years") |
                     (eurostat$AGE == "From 50 to 59 years")]   <- "[0,60)"
eurostat$age_group[eurostat$AGE == "From 60 to 69 years"]       <- "[60,69)"
eurostat$age_group[eurostat$AGE == "From 70 to 79 years"]       <- "[70,79)"
eurostat$age_group[eurostat$AGE == "From 80 to 89 years"]       <- "[80,89)"
eurostat$age_group[eurostat$AGE == "90 years or over"]          <- "90+"
eurostat$age_group<-factor(eurostat$age_group)

# Recode as numeric the observed deaths 
# Remove the commas (for thousands)
eurostat$Value <- as.numeric(gsub(",", "", eurostat$Value))
# Aggregate the number of deaths
eurostat <- eurostat %>%
  group_by(region, sex, year, iso_week, age_group) %>%
  summarize(observed_deaths = sum(Value) ) %>%
  ungroup()




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
  group_by(epi_year, sex, age_group, region) %>%
  summarize( earlier_obs_deaths = 
               sum( observed_deaths[which(iso_week %in% c(27:52))],
                    observed_deaths[which(iso_week %in% c(1:6))] ),
             later_obs_deaths = 
               sum( observed_deaths[which(iso_week %in% c(7:26))],
                    (1/7)*observed_deaths[which(iso_week == 26)]) ) %>%
  ungroup()

eurostat1415 <- eurostat %>% 
  filter(epi_year %in% c("2014/2015")) %>%
  group_by(epi_year, sex, age_group, region) %>%
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
  group_by(epi_year, sex, age_group, region) %>%
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
  group_by(epi_year, sex, age_group, region) %>%
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
  group_by(epi_year, sex, age_group, region) %>%
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
  group_by(epi_year, sex, age_group, region) %>%
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
  group_by(epi_year, sex, age_group, region) %>%
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
# by epi_year, sex, age_group and country (excluding year 2019/2020)

eurostat1 <- eurostat %>%
  mutate(upsilon = later_obs_deaths/earlier_obs_deaths) %>%
  group_by(sex, age_group, region) %>%
  mutate(av_upsilon = mean(upsilon[which(epi_year!="2019/2020")]),
         sd_upsilon = sd(upsilon[which(epi_year!="2019/2020")]),
         cv_upsilon = sd_upsilon/av_upsilon,
         lab = paste("mean =", round(av_upsilon,2), ",",
                     "sd =", round(sd_upsilon,2), 
                     "\ncv =", round(cv_upsilon,4)*100, "%")) %>%
  ungroup() %>%
  distinct(sex, age_group, region, epi_year, .keep_all = TRUE) %>%
  select(c(region, sex, age_group, epi_year,
           earlier_obs_deaths, later_obs_deaths, 
           upsilon, av_upsilon, sd_upsilon, cv_upsilon, lab))

# as.data.frame(eurostat1 %>% filter(region=="Corse"))
# region     sex age_group  epi_year earlier_obs_deaths later_obs_deaths   upsilon av_upsilon
# 1   Corse Females    [0,60) 2013/2014           67.00000         35.28571 0.5266525  0.5961065
# 2   Corse Females   [60,69) 2013/2014           63.00000         52.14286 0.8276644  0.6993799
# 3   Corse Females   [70,79) 2013/2014          138.00000         72.57143 0.5258799  0.6557003
# 4   Corse Females   [80,89) 2013/2014          351.00000        214.85714 0.6121286  0.6223064
# 5   Corse Females       90+ 2013/2014          312.00000        199.00000 0.6378205  0.6202339
# 6   Corse   Males    [0,60) 2013/2014          134.00000         81.57143 0.6087420  0.5294651
# 7   Corse   Males   [60,69) 2013/2014          154.00000         87.00000 0.5649351  0.6298235
# 8   Corse   Males   [70,79) 2013/2014          217.00000        112.71429 0.5194207  0.5512376
# 9   Corse   Males   [80,89) 2013/2014          295.00000        192.28571 0.6518160  0.6141932
# 10  Corse   Males       90+ 2013/2014          144.00000        114.57143 0.7956349  0.7060487
# 11  Corse Females    [0,60) 2014/2015           52.57143         38.28571 0.7282609  0.5961065
# 12  Corse Females   [60,69) 2014/2015           64.14286         50.00000 0.7795100  0.6993799
# 13  Corse Females   [70,79) 2014/2015          122.28571         67.42857 0.5514019  0.6557003
# 14  Corse Females   [80,89) 2014/2015          338.42857        209.57143 0.6192486  0.6223064
# 15  Corse Females       90+ 2014/2015          337.42857        215.85714 0.6397121  0.6202339
# 16  Corse   Males    [0,60) 2014/2015          133.85714         65.42857 0.4887940  0.5294651
# 17  Corse   Males   [60,69) 2014/2015          145.57143         96.57143 0.6633955  0.6298235
# 18  Corse   Males   [70,79) 2014/2015          226.14286        121.71429 0.5382186  0.5512376
# 19  Corse   Males   [80,89) 2014/2015          324.28571        225.00000 0.6938326  0.6141932
# 20  Corse   Males       90+ 2014/2015          155.28571        116.14286 0.7479301  0.7060487




###### Labels NUTS1 #####

# Create the variable country from the 14 regions in France and 7 regions in Spain
eurostat1 <- eurostat1 %>%
  mutate(country = ifelse(region %in% c("Auvergne - Rhône-Alpes",
                                        "Bourgogne - Franche-Comté",
                                        "Bretagne",
                                        "Centre - Val de Loire",
                                        "Corse",
                                        "Grand Est",
                                        "Hauts-de-France",
                                        "Île de France",
                                        "Normandie",
                                        "Nouvelle-Aquitaine",
                                        "Occitanie",
                                        "Pays-de-la-Loire",
                                        "Provence-Alpes-Côte d'Azur",
                                        "RUP FR - Régions ultrapériphériques françaises"), 
                          "France", 
                          ifelse(region %in% c("Canarias",
                                               "Centro (ES)",
                                               "Comunidad de Madrid",
                                               "Este (ES)", 
                                               "Noreste (ES)", 
                                               "Noroeste (ES)", 
                                               "Sur (ES)"), "Spain", NA)))
# Convert to factors
eurostat1$country <- as.factor(eurostat1$country)
eurostat1$sex <- as.factor(eurostat1$sex)

# Short label for the regional level
eurostat1$region.short.label <- as.factor(eurostat1$region)
levels(eurostat1$region.short.label) <- list(
  # France
  "FRK" = "Auvergne - Rhône-Alpes",
  "FRC" = "Bourgogne - Franche-Comté",
  "FRH" = "Bretagne",
  "FRB" = "Centre - Val de Loire",
  "FRM" = "Corse",
  "FRF" = "Grand Est",
  "FRE" = "Hauts-de-France",
  "FR1" = "Île de France",
  "FRD" = "Normandie",
  "FRI" = "Nouvelle-Aquitaine",
  "FRJ" = "Occitanie",
  "FRG" = "Pays-de-la-Loire",
  "FRL" = "Provence-Alpes-Côte d'Azur",
  "FRY" = "RUP FR - Régions ultrapériphériques françaises",
  # Spain
  "ES7" = "Canarias",
  "ES4" = "Centro (ES)",
  "ES3" = "Comunidad de Madrid",
  "ES5" = "Este (ES)", 
  "ES2" = "Noreste (ES)", 
  "ES1" = "Noroeste (ES)", 
  "ES6" = "Sur (ES)") 

# Reorder the levels
eurostat1$region.short.label <- 
  factor(eurostat1$region.short.label, 
          c("ES1", "ES2", "ES3", "ES4", "ES5", "ES6", "ES7",
            "FRK", "FRC", "FRH", "FRB", "FRM", "FRF", "FRE", 
            "FR1", "FRD", "FRI", "FRJ", "FRG", "FRL", "FRY"))



###### FIGURES S3-S6 ######

# Create a function to plot the later/earlier ratios
# The input is the database (i.e., filtered by sex and country)

figure1 <- function(data) {
  
  data %>% 
    filter(epi_year != "2019/2020") %>%
    ggplot(aes(upsilon, epi_year, colour = age_group)) + 
    geom_point(size = 3) + 
    facet_grid(rows = vars(region.short.label, sex), 
               cols = vars(age_group)) +   
    theme_bw() + 
    labs(x = "Later/earlier ratio", y = "Epiyear", color = "Age group") +
    theme(
      # Axis 
      axis.title.x = element_text(size = 14, face = "bold"),
      axis.title.y = element_text(size = 14, face = "bold"),
      axis.text.x = element_text(size = 11, angle = 45, vjust = 1, hjust = 1),
      axis.text.y = element_text(size = 11),
      # Legend
      legend.title = element_text(size = 14, face = "bold"),
      legend.position = "bottom",
      legend.text=element_text(size=11),
      # Facet
      strip.text.x = element_text(size = 11),
      strip.text.y = element_text(size = 11)) +
    geom_vline(data = data, aes(xintercept = av_upsilon, colour = age_group),
               size = 2, alpha = 0.5, show.legend = FALSE) +
    # geom_text(x = 0.6) for French females
    #
    #
    #
    geom_text(data = data, aes(label = lab, fontface = "plain"),
              x = 0.6, y = -0.01, size = 3.5, stat = "unique", 
              lineheight = 0.7) +
    geom_segment(aes(x = upsilon, xend = av_upsilon,
                     y = epi_year, yend = epi_year),
                 size = 0.7) +
    expand_limits(y = c(-0.7, 7)) + 
    guides(colour = guide_legend(nrow = 1))
}


setwd(dir.out)

# Figure S3: French females
pdf("FigS3_FRA_Females_regional.pdf", width = 14, height = 17)
ff <- eurostat1 %>% filter(country == "France" & sex == "Females")
figure1(ff)
dev.off()

# Figure A4: French males
geom_text(x = 0.65)
pdf("FigS4_FRA_Males_regional.pdf", width = 14, height = 17)
fm <- eurostat1 %>% filter(country == "France" & sex == "Males")
figure1(fm)
dev.off()

# Figure A5: Spanish females
geom_text(x = 0.7)
pdf("FigS5_ESP_Females_regional.pdf", width = 10, height = 12)
sm <- eurostat1 %>% filter(country == "Spain" & sex == "Females")
figure1(sm)
dev.off()

# Figure A6: Spanish males
geom_text(x = 0.67)
pdf("FigS6_ESP_Males_regional.pdf", width = 10, height = 12)
sf <- eurostat1 %>% filter(country == "Spain" & sex == "Males")
figure1(sf)
dev.off()




###### Excess deaths by age and sex ######

# Compute expected dx and excess dx for year 2019/2020 by sex, age_group, country_code
eurostat2 <- eurostat1 %>% 
  group_by(sex, age_group, region) %>%
  mutate(later_exp_deaths = 
           ifelse(epi_year=="2019/2020", round(earlier_obs_deaths*av_upsilon),
                  ifelse(epi_year!="2019/2020", NA, 999)),
         excess_deaths = 
           ifelse(epi_year=="2019/2020", later_obs_deaths - later_exp_deaths,
                  ifelse(epi_year!="2019/2020", NA, 999))) %>%
  ungroup() %>%
  # Subset to later segment data for 2019
  filter(epi_year == "2019/2020") %>%
  distinct(sex, age_group, region, .keep_all = TRUE) %>%
  select(c(region, sex, age_group, country, 
           later_obs_deaths, later_exp_deaths, excess_deaths))

# as.data.frame(eurostat2 %>% filter(region=="Corse"))
#    region     sex age_group country later_obs_deaths later_exp_deaths excess_deaths
# 1   Corse Females    [0,60)  France         26.14286               31     -4.857143
# 2   Corse Females   [60,69)  France         32.28571               53    -20.714286
# 3   Corse Females   [70,79)  France         80.14286               87     -6.857143
# 4   Corse Females   [80,89)  France        180.14286              185     -4.857143
# 5   Corse Females       90+  France        287.28571              231     56.285714
# 6   Corse   Males    [0,60)  France         70.00000               58     12.000000
# 7   Corse   Males   [60,69)  France         77.42857               83     -5.571429
# 8   Corse   Males   [70,79)  France        140.57143              130     10.571429
# 9   Corse   Males   [80,89)  France        226.85714              195     31.857143
# 10  Corse   Males       90+  France        127.14286              132     -4.857143




###### Bootstrap by age and sex ######

eurostat3 <- eurostat %>%
  mutate(upsilon = later_obs_deaths/earlier_obs_deaths) %>%
  arrange(region, sex, age_group)
as.data.frame(eurostat3)

# Series of 6 ratios from 2013/2014 to 2018/2019 (excluding 19/20)
v <- eurostat3 %>%
  filter(epi_year!="2019/2020") %>%
  select(region, sex, age_group, epi_year, upsilon)
v <- pivot_wider(v, names_from = epi_year, values_from = upsilon)
as.data.frame(v)

# Observed deaths on the earlier and later segment
obs_data <- eurostat3 %>%
  arrange(region, sex, age_group) %>%
  filter(epi_year=="2019/2020") %>%
  select(region, sex, age_group, epi_year,
         earlier_obs_deaths, later_obs_deaths)
as.data.frame(obs_data)

# Bootstrap estimates
B <- 10000
G <- nrow(obs_data)
rstar <- matrix(0,G,B)        # ratios bootstrap
boot.ExpD <- matrix(0,G,B)    # Expected number of deaths
ExpD.star <- matrix(0,G,B)    # Death counts from Poisson 
ObsD <- matrix(
  rep(obs_data$later_obs_deaths,each=B),
  ncol=B, byrow=TRUE)         # Death counts from Poisson 
Excess.star <- matrix(0,G,B)  # Excess deaths

for(i in c(1:G)){
  
  # Repeat 10,000 times to approximate the distribution of expected
  # number of deaths   
  for(j in c(1:B)){ 
    
    # From the 10 later/earlier ratios I drew 12 v* with replacement
    # The mean represents the average later/earlier ratio
    rstar[i,j] <- sample(as.numeric(v[i,4:9]), size=1, replace=T)
    
    # I multiply vbar* by the observed number of deaths in the earlier period
    # to get the expected number of deaths in the later period
    boot.ExpD[i,j] <- rstar[i,j]*obs_data$earlier_obs_deaths[i]
    
    # I assume that the death counts followed a Poisson distribution defined 
    # by the expected number of deaths
    # I randomly chose a death count from the Poisson distribution
    ExpD.star[i,j] <- rpois(1, boot.ExpD[i,j])
    Excess.star[i,j] <- ObsD[i,j]-ExpD.star[i,j]
  }
  
}

Risk.obs.star <- Excess.star/ObsD
Risk.exp.star <- Excess.star/ExpD.star

##### Prediction Interval - quantiles

# I derive the empirical percentiles for expected deaths
tab.int.exp <- cbind(as.data.frame(obs_data[1:3]),
                     round(rowQuantiles(ExpD.star, probs=c(0.025,0.975))))

# I derive the empirical percentiles for excess deaths
tab.int.exc <- cbind(as.data.frame(obs_data[1:3]),
                     round(rowQuantiles(Excess.star, probs=c(0.025,0.975))))

# I derive the empirical percentiles for excess deaths
tab.int.risk.obs <- cbind(as.data.frame(obs_data[1:3]),
                          round(rowQuantiles(Risk.obs.star, probs=c(0.025,0.975))*100,1))

# I derive the empirical percentiles for excess deaths
tab.int.risk.exp <- cbind(as.data.frame(obs_data[1:3]),
                          round(rowQuantiles(Risk.exp.star, probs=c(0.025,0.975))*100,1))




###### TABLES S3-S4 ######

###### Excess death

eurostat4 <- eurostat2 %>%
  mutate(risk_obs = round((excess_deaths/later_obs_deaths)*100,1),
         risk_exp = round((excess_deaths/later_exp_deaths)*100,1))

eurostat4 <- eurostat4[-c(5,6)]
eurostat4[,5] <- round(eurostat4[,5])
eurostat4 <- as.data.frame(eurostat4)

###### Prediction Interval - quantiles

colnames(tab.int.exc)[4:5] <- c("excess_low_int","excess_upp_int")
colnames(tab.int.risk.exp)[4:5] <- c("risk_exp_low_int","risk_exp_upp_int")
colnames(tab.int.risk.obs)[4:5] <- c("risk_obs_low_int","risk_obs_upp_int")

eurostat4 <- 
  left_join(eurostat4, tab.int.exc, by=c("region","sex","age_group")) %>%
  left_join(., tab.int.risk.exp, by=c("region","sex","age_group")) %>%
  left_join(., tab.int.risk.obs, by=c("region","sex","age_group"))

###### Spain

eurostat.sp <- eurostat4 %>% filter(country == "Spain")
eurostat.sp <- eurostat.sp[,-4]

###### France

eurostat.fr <- eurostat4 %>% filter(country == "France")
eurostat.fr <- eurostat.fr[,-4]


setwd(dir.out)
write_xlsx(eurostat.sp,"TabS3_ESP_regional.xlsx")
write_xlsx(eurostat.fr,"TabS4_FRA_regional.xlsx")




###### FIGURES 4b-5b ######

###### Plot b: Excess death risk figures by regions, age group, sex

figure.b <- function(data) {
  
  data %>%
    ggplot(aes(x = age_group, y = risk_obs))+
    geom_point(aes(colour=factor(region.short.label)), size = 4)+
    # geom_errorbar(aes(ymin = risk_obs-risk_obs_low_int,
    #                   ymax = risk_obs+risk_obs_upp_int,
    #                   colour = region.short.label),
    #               width=.3) + 
    geom_line(aes(group = region.short.label, colour = region.short.label),
              size = 1, alpha = 0.5) +
    facet_grid(~sex)+
    theme_minimal()+
    theme(
      # Axis
      axis.text.x = element_text(size = 25, angle = 45, vjust = 1, hjust = 1),
      axis.text.y = element_text(size = 25),
      axis.title.x = element_text(size = 25, face = "bold"),
      axis.title.y = element_text(size = 25, face = "bold"),      
      # Legend
      legend.title = element_blank(),
      legend.text=element_text(size=25),
      # Facets
      strip.text = element_text(size=25)#,
      # plot.margin = unit(c(1,3,1,1), "lines")
    )+
    labs(x="Age", y="Excess death risk (%)") +
    scale_y_continuous(breaks=seq(-15,50,5))  +
    # Label of the regions
    scale_colour_discrete(guide = 'none')  +
    scale_x_discrete(expand=c(0, 1)) +
    geom_dl(aes(label = region.short.label, colour = region.short.label),
            method = list(dl.trans(x = x + 0.2), "last.points", cex = 1.6)) +
    # Horizontal line =0
    geom_hline(yintercept = 0, size = 1, alpha = 0.5, 
               linetype = "dashed", show.legend = FALSE) 
}

eurostat.fr$region.short.label <- as.factor(eurostat.fr$region)
levels(eurostat.fr$region.short.label) <- list(
  # France
  "FRK" = "Auvergne - Rhône-Alpes",
  "FRC" = "Bourgogne - Franche-Comté",
  "FRH" = "Bretagne",
  "FRB" = "Centre - Val de Loire",
  "FRM" = "Corse",
  "FRF" = "Grand Est",
  "FRE" = "Hauts-de-France",
  "FR1" = "Île de France",
  "FRD" = "Normandie",
  "FRI" = "Nouvelle-Aquitaine",
  "FRJ" = "Occitanie",
  "FRG" = "Pays-de-la-Loire",
  "FRL" = "Provence-Alpes-Côte d'Azur",
  "FRY" = "RUP FR - Régions ultrapériphériques françaises") 
eurostat.fr <- eurostat.fr %>% filter(region != "Corse")

eurostat.sp$region.short.label <- as.factor(eurostat.sp$region)
levels(eurostat.sp$region.short.label) <- list(
  # Spain
  "ES7" = "Canarias",
  "ES4" = "Centro (ES)",
  "ES3" = "Comunidad de Madrid",
  "ES5" = "Este (ES)", 
  "ES2" = "Noreste (ES)", 
  "ES1" = "Noroeste (ES)", 
  "ES6" = "Sur (ES)") 

setwd(dir.out)

# France
png("Fig4b_FRA_regional.png", width = 800, height = 800)
eurostat.fr <- eurostat.fr %>% filter(region != "Corse")
figure.b(eurostat.fr)
dev.off()

# Spain
png("Fig5b_ESP_regional.png", width = 800, height = 800)
figure.b(eurostat.sp)
dev.off()




###### Bootstrap total ######

eurostat5 <- eurostat %>%
  group_by(region, epi_year) %>%
  summarize(earlier_obs_deaths = sum(earlier_obs_deaths),
            later_obs_deaths = sum(later_obs_deaths),
            upsilon = later_obs_deaths/earlier_obs_deaths)
as.data.frame(eurostat5)

# Series of 6 ratios from 2013/2014 to 2018/2019 (excluding 19/20)
v <- eurostat5 %>%
  filter(epi_year!="2019/2020") %>%
  select(region, epi_year, upsilon)
v <- pivot_wider(v, names_from = epi_year, values_from = upsilon)
as.data.frame(v)

# Observed deaths on the earlier and later segment
obs_data <- eurostat5 %>%
  filter(epi_year=="2019/2020") %>%
  select(region, epi_year, earlier_obs_deaths, later_obs_deaths)
as.data.frame(obs_data)

# Bootstrap estimates
B <- 10000
G <- nrow(obs_data)
rstar <- matrix(0,G,B)        # ratios bootstrap
boot.ExpD <- matrix(0,G,B)    # Expected number of deaths
ExpD.star <- matrix(0,G,B)    # Death counts from Poisson 
ObsD <- matrix(
  rep(obs_data$later_obs_deaths,each=B),
  ncol=B, byrow=TRUE)         # Death counts from Poisson 
Excess.star <- matrix(0,G,B)  # Excess deaths

for(i in c(1:G)){
  
  # Repeat 10,000 times to approximate the distribution of expected
  # number of deaths   
  for(j in c(1:B)){ 
    
    # From the 10 later/earlier ratios I drew 12 v* with replacement
    # The mean represents the average later/earlier ratio
    rstar[i,j] <- sample(as.numeric(v[i,2:7]), size=1, replace=T)
    
    # I multiply vbar* by the observed number of deaths in the earlier period
    # to get the expected number of deaths in the later period
    boot.ExpD[i,j] <- rstar[i,j]*obs_data$earlier_obs_deaths[i]
    
    # I assume that the death counts followed a Poisson distribution defined 
    # by the expected number of deaths
    # I randomly chose a death count from the Poisson distribution
    ExpD.star[i,j] <- rpois(1, boot.ExpD[i,j])
    Excess.star[i,j] <- ObsD[i,j]-ExpD.star[i,j]
  }
  
}

Risk.obs.star <- round((Excess.star/ObsD)*100,1)
Risk.exp.star <- round((Excess.star/ExpD.star)*100,1)

##### Prediction Interval - quantiles

# I derive the empirical percentiles for excess deaths
tab.int.exc <- cbind(as.data.frame(obs_data[1]),
                     round(rowQuantiles(Excess.star, probs=c(0.025,0.975))))

# I derive the empirical percentiles for excess deaths
tab.int.risk.obs <- cbind(as.data.frame(obs_data[1]),
                          rowQuantiles(Risk.obs.star, probs=c(0.025,0.975)))

# I derive the empirical percentiles for excess deaths
tab.int.risk.exp <- cbind(as.data.frame(obs_data[1]),
                          rowQuantiles(Risk.exp.star, probs=c(0.025,0.975)))




###### FIGURES 4a-5a ######

eurostat6 <- eurostat2 %>%
  group_by(region) %>%
  summarize(later_obs_deaths = round(sum(later_obs_deaths)),
            later_exp_deaths = round(sum(later_exp_deaths)),
            excess_deaths = round(sum(excess_deaths))) %>%
  mutate(risk_obs = round((excess_deaths/later_obs_deaths)*100,1),
         risk_exp = round((excess_deaths/later_exp_deaths)*100,1))
eurostat6 <- as.data.frame(eurostat6)

colnames(tab.int.exc)[2:3] <- c("excess_low_int","excess_upp_int")
colnames(tab.int.risk.exp)[2:3] <- c("risk_exp_low_int","risk_exp_upp_int")
colnames(tab.int.risk.obs)[2:3] <- c("risk_obs_low_int","risk_obs_upp_int")
eurostat6 <-
  left_join(eurostat6, tab.int.exc, by=c("region")) %>%
  left_join(., tab.int.risk.exp, by=c("region")) %>%
  left_join(., tab.int.risk.obs, by=c("region"))

eurostat6 <- eurostat6 %>%
  mutate(country = ifelse(region %in% c("Auvergne - Rhône-Alpes",
                                        "Bourgogne - Franche-Comté",
                                        "Bretagne",
                                        "Centre - Val de Loire",
                                        "Corse",
                                        "Grand Est",
                                        "Hauts-de-France",
                                        "Île de France",
                                        "Normandie",
                                        "Nouvelle-Aquitaine",
                                        "Occitanie",
                                        "Pays-de-la-Loire",
                                        "Provence-Alpes-Côte d'Azur",
                                        "RUP FR - Régions ultrapériphériques françaises"),
                          "France",
                          ifelse(region %in% c("Canarias",
                                               "Centro (ES)",
                                               "Comunidad de Madrid",
                                               "Este (ES)",
                                               "Noreste (ES)",
                                               "Noroeste (ES)",
                                               "Sur (ES)"), "Spain", NA)))
eurostat.sp.tot <- eurostat6 %>% filter(country == "Spain")
eurostat.fr.tot <- eurostat6 %>% filter(country == "France")


###### Plot a: Excess death risk figures by region

figure.a <- function(data) {
  
  data %>%
    ggplot(aes(x=region, y=risk_obs, width=0.65,
               fill=factor(region))) +
    geom_bar(stat="identity", aes(fct_reorder(region, risk_obs)))+
    coord_flip() +
    theme_minimal() +
    theme(
      # Axis 
      axis.title.x = element_text(size = 25, face = "bold"),
      axis.title.y = element_text(size = 25, face = "bold"),
      axis.text.x = element_text(size = 25, angle = 45, vjust = 1, hjust = 1),
      axis.text.y = element_text(size = 25),
      # Legend
      legend.position = "none",
      legend.title = element_blank(),
      legend.text=element_text(size=18)) +
    labs(x="NUTS1 Region", y="Excess death risk (%)") +
    scale_y_continuous(breaks=seq(-5,50,5)) 
  
}

eurostat.fr.tot$region <- as.factor(eurostat.fr.tot$region)
levels(eurostat.fr.tot$region) <- list(
  # France
  "Auvergne - Rhône-Alpes" = "Auvergne - Rhône-Alpes",
  "Bourgogne - Franche-Comté" = "Bourgogne - Franche-Comté",
  "Bretagne" = "Bretagne",
  "Centre - Val de Loire" = "Centre - Val de Loire",
  "Corse" = "Corse",
  "Grand Est" = "Grand Est",
  "Hauts-de-France" = "Hauts-de-France",
  "Île de France" = "Île de France",
  "Normandie" = "Normandie",
  "Nouvelle-Aquitaine" = "Nouvelle-Aquitaine",
  "Occitanie" = "Occitanie",
  "Pays-de-la-Loire" = "Pays-de-la-Loire",
  "Provence-Alpes-Côte d'Azur" = "Provence-Alpes-Côte d'Azur",
  "Régions ultrapériphériques" = "RUP FR - Régions ultrapériphériques françaises") 

eurostat.sp.tot$region <- as.factor(eurostat.sp.tot$region)
levels(eurostat.sp.tot$region) <- list(
  # Spain
  "Canarias" = "Canarias",
  "Centro" = "Centro (ES)",
  "Comunidad de Madrid" = "Comunidad de Madrid",
  "Este" = "Este (ES)", 
  "Noreste" = "Noreste (ES)", 
  "Noroeste" = "Noroeste (ES)", 
  "Sur" = "Sur (ES)") 

setwd(dir.out)

# France
png("Fig4a_FRA_regional.png", width = 700, height = 800)
eurostat.fr.tot <- eurostat.fr.tot %>% filter(region != "Corse")
figure.a(eurostat.fr.tot)
dev.off()

# Spain
png("Fig5a_ESP_regional.png", width = 700, height = 800)
figure.a(eurostat.sp.tot)
dev.off()




###### Comparison 5-years averages ######

# Run the previous code in the sections
# - Raw data
# - Data preparation
# - Create epi-years
# - Later/earlier observed deaths (gregorian calendar)

###### Later/earlier ratios 5-years

# Create a function to apply the later/earlier method on the previous
# five years of data, by age group, sex and country 

method.ratio <- function(epi.hist, epi.pred){
  
  # Select desired variables and observations
  eurostat1 <- eurostat %>%
    mutate(upsilon = later_obs_deaths/earlier_obs_deaths) %>%
    filter(epi_year %in% epi.hist) %>%
    group_by(sex, age_group, region) %>%
    mutate(av_upsilon = mean(upsilon[which(epi_year!=epi.pred)]),
           sd_upsilon = sd(upsilon[which(epi_year!=epi.pred)]),
           cv_upsilon = sd_upsilon/av_upsilon,
           lab = paste("mean =", round(av_upsilon,2), ",",
                       "sd =", round(sd_upsilon,2), ",",
                       "cv =", round(cv_upsilon,4)*100, "%")) %>%
    ungroup()
  
  # Compute expected dx and excess dx for the predicted year
  # by sex, age_group, region
  eurostat2 <- eurostat1 %>% 
    group_by(sex, age_group, region) %>%
    mutate(later_exp_deaths = 
             ifelse(epi_year==epi.pred, 
                    round(earlier_obs_deaths*av_upsilon),
                    ifelse(epi_year!=epi.pred, NA, 999))) %>%
    ungroup()  
  
  # Subset to later segment data for the predicted year
  eurostat2 <- eurostat2 %>%
    filter(epi_year == epi.pred) %>%
    distinct(sex, age_group, region, .keep_all = TRUE) %>%
    select(c(region, sex, age_group, 
             later_obs_deaths, later_exp_deaths)) 
  
  return(eurostat2)
  
}

###### 5-years-averages

# Create a function to apply the 5-years average method on the previous
# five years of data, by age group, sex and country 

method.av <- function(epi.hist, epi.pred){
  
  # Compute expected dx and excess dx for the predicted year
  # by sex, age_group, region
  eurostat2.av <- eurostat %>% 
    filter(epi_year %in% epi.hist) %>%
    group_by(sex, age_group, region) %>%
    mutate(later_exp_deaths = 
             ifelse(epi_year==epi.pred, 
                    mean(later_obs_deaths[which(epi_year!=epi.pred)]),
                    ifelse(epi_year!=epi.pred, NA, 999))) %>%
    ungroup()  
  
  # Subset to later segment data for the predicted year
  eurostat2.av <- eurostat2.av %>%
    filter(epi_year == epi.pred) %>%
    distinct(sex, age_group, region, .keep_all = TRUE) %>%
    select(c(region, sex, age_group, 
             later_obs_deaths, later_exp_deaths)) 
  
  return(eurostat2.av)
  
}

###### Apply the functions to the epiyear 2009/10-2018/2019

epi.hist=c("2013/2014","2014/2015","2015/2016","2016/2017","2017/2018","2018/2019")
epi.pred="2018/2019"
pred.ratio.1819 <- method.ratio(epi.hist, epi.pred)
pred.av.1819 <- method.av(epi.hist, epi.pred)

###### Create a final table with the observed and fitted deaths

names(pred.ratio.1819)[names(pred.ratio.1819) == "later_obs_deaths"] <- "observed.1819"
names(pred.ratio.1819)[names(pred.ratio.1819) == "later_exp_deaths"] <- "expected.ratio.1819"
names(pred.av.1819)[names(pred.av.1819) == "later_exp_deaths"] <- "expected.av.1819"

# Create the final table, for every year I save
# - the observed values, 
# - the fitted values with the later/earlier method, 
# - the fitted values with the 5-years-average method

pred <- cbind(pred.ratio.1819[1:5], pred.av.1819[5])

###### Compute the root mean squared error
rmse <- pred %>%
  mutate(rmse.ratio = 
           sqrt( (expected.ratio.1819-observed.1819)^2 ),
         rmse.av =
           sqrt( (expected.av.1819-observed.1819)^2 )) %>%
  select(region, sex, age_group, rmse.ratio, rmse.av)

#                                    region     sex age_group  rmse.ratio     rmse.av
# 1                  Auvergne - Rhône-Alpes Females    [0,60)   5.2857143   3.4571429
# 2               Bourgogne - Franche-Comté Females    [0,60)  30.0000000  26.2571429
# 3                                Bretagne Females    [0,60)  37.1428571  30.1142857
# 4                                Canarias Females    [0,60)   6.2857143  22.0571429
# 5                   Centre - Val de Loire Females    [0,60)  10.0000000  12.2285714
# 6                             Centro (ES) Females    [0,60)  49.8571429  69.3714286
# 7                     Comunidad de Madrid Females    [0,60)  32.4285714  55.4285714
# 8                                   Corse Females    [0,60)  14.1428571  10.7142857
# 9                               Este (ES) Females    [0,60)  93.8571429  16.3142857
# 10                              Grand Est Females    [0,60)  66.2857143  36.1142857
# 11                        Hauts-de-France Females    [0,60)  16.2857143  54.0285714
# 12                          Île de France Females    [0,60) 124.0000000 106.3142857
# 13                           Noreste (ES) Females    [0,60)  36.0000000  48.2000000
# 14                              Normandie Females    [0,60)  12.5714286  23.6571429
# 15                          Noroeste (ES) Females    [0,60)  24.1428571  18.8000000
# 16                     Nouvelle-Aquitaine Females    [0,60)  31.4285714  37.6000000
# 17                              Occitanie Females    [0,60)  55.7142857  49.5428571
# 18                       Pays-de-la-Loire Females    [0,60)  25.4285714  12.3428571
# 19             Provence-Alpes-Côte d'Azur Females    [0,60)  26.7142857  26.4571429
# 20  Régions ultrapériphériques françaises Females    [0,60)  43.5714286  23.1142857

###### Compute the mean absolute percentage error
mape <- pred %>%
  mutate(mape.ratio = 
           (100) * (abs((observed.1819-expected.ratio.1819)/observed.1819)),
         mape.av = 
           (100) * (abs((observed.1819-expected.av.1819)/observed.1819))) %>%
  select(region, sex, age_group, mape.ratio, mape.av)

mean(mape$mape.ratio)
# [1] 4.237939
mean(mape$mape.av)
# [1] 6.236297




###### Table S5-S6 ######

rmse <- rmse %>%
  pivot_longer(rmse.ratio:rmse.av, 
               names_to = "method", 
               values_to = "rmse")
rmse$rmse <- round(rmse$rmse)

# Spain
rmse.spain <- rmse %>%
  filter(region %in% c("Canarias",
                       "Centro (ES)",
                       "Comunidad de Madrid",
                       "Este (ES)", 
                       "Noreste (ES)", 
                       "Noroeste (ES)", 
                       "Sur (ES)")) 

tab.rmse.spain <- 
  reshape2::dcast(
    reshape2::melt(rmse.spain,id.vars=c("age_group", "region", "sex", "method")),
    age_group+variable~region+sex+method) %>%
  select(-"variable")

# France
rmse.france <- rmse %>%
  filter(region %in% c("Auvergne - Rh?ne-Alpes",
                       "Bourgogne - Franche-Comt?",
                       "Bretagne",
                       "Centre - Val de Loire",
                       "Corse",
                       "Grand Est",
                       "Hauts-de-France",
                       "?le de France",
                       "Normandie",
                       "Nouvelle-Aquitaine",
                       "Occitanie",
                       "Pays-de-la-Loire",
                       "Provence-Alpes-C?te d'Azur",
                       "RUP FR - R?gions ultrap?riph?riques fran?aises"))
rmse.france$rmse <- round(rmse.france$rmse)

tab.rmse.france <- 
  reshape2::dcast(
    reshape2::melt(rmse.france, id.vars=c("age_group", "region", "sex", "method")),
    age_group+variable~region+sex+method) %>%
  select(-"variable")


setwd(dir.out)
write_xlsx(tab.rmse.spain,"TabS5_ESP_regional.xlsx")
write_xlsx(tab.rmse.france,"TabS6_FRA_regional.xlsx")







