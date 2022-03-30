
#########################################################################
# Author: Ainhoa Elena Leger
# Last update: 30-03-2022
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

# Load file downloaded from HMD
stmf <- read.table("stmf.csv", header=TRUE, sep=",", skip = 2)




###### Data preparation ######

# Select desired variables and observations

stmf <- stmf %>%
  select(CountryCode, Year, Week, Sex, D0_14:D85p) %>% 
  filter(CountryCode %in% c("FRATNP", "ESP"),
         Year %in% c(2004:2020),
         Sex %in% c("f", "m")) %>%
  # Create the variable age groups and observed deaths
  pivot_longer(D0_14:D85p, 
               names_to = "age_group", 
               values_to = "observed_deaths") %>%
  rename(country_code = CountryCode,
         sex = Sex,
         year = Year,
         iso_week = Week) %>%
  relocate(country_code, sex, age_group, year, observed_deaths, iso_week)

# Recode as factor and rename labels
# Country
stmf$country_code <- as.factor(stmf$country_code)
levels(stmf$country_code) <- list(France = "FRATNP", Spain = "ESP")
# Sex
stmf$sex <- as.factor(stmf$sex)
levels(stmf$sex) <- list(Female = "f", Male = "m")
# Age group
stmf$age_group <- as.factor(stmf$age_group)
levels(stmf$age_group) <- list("[0,15)" = "D0_14",
                               "[15,65)" = "D15_64",
                               "[65,75)" = "D65_74",
                               "[75,85)" = "D75_84",
                               "85+" = "D85p") 




###### Create epi-years ######

# Use iso-weeks to construct the variable epi-years
# Each epi-year will have iso-weeks 27-52 (or 27-53 for leap years) 
# of one year and iso-weeks 1-26 of the following year
# Example: epi-year 2009/2010 is defined by iso-weeks 27-53 of 2009
# and iso-weeks 1-26 of 2010

stmf$epi_year=0

for(i in c(1:dim(stmf)[1])){
  
  ## Years with 53 weeks (2009)
  if (stmf$year[i] %in% c(2004, 2009, 2015, 2020)) {
    if (stmf$iso_week[i]<=26) {
      stmf$epi_year[i] = paste0(stmf$year[i]-1, "/", stmf$year[i]) }   
    else if (stmf$iso_week[i]>=27 & stmf$iso_week[i]<=53) {
      stmf$epi_year[i] = paste0(stmf$year[i], "/", stmf$year[i]+1) }
  }
  
  ## Years with 52 weeks
  else if (stmf$year[i] %in% c(2005:2008, 2010:2014, 2016:2019)) {
    if (stmf$iso_week[i]<=26) {
      stmf$epi_year[i] = paste0(stmf$year[i]-1, "/", stmf$year[i]) }
    else if (stmf$iso_week[i]>=27 & stmf$iso_week[i]<=52) {
      stmf$epi_year[i] = paste0(stmf$year[i], "/", stmf$year[i]+1) }
  }
}




###### Later/earlier observed deaths (gregorian calendar) ######

# Compute D- and D+ by epi_year, sex, age_group and country

stmf0405 <- stmf %>% 
  filter(epi_year %in% c("2004/2005")) %>%
  group_by(epi_year, sex, age_group, country_code) %>%
  summarize( earlier_obs_deaths = 
               sum( (4/7)*observed_deaths[which(iso_week == 27)],
                    observed_deaths[which(iso_week %in% c(28:53))],
                    observed_deaths[which(iso_week %in% c(1:5))],
                    (3/7)*observed_deaths[which(iso_week == 6)]),
             later_obs_deaths = 
               sum( (4/7)*observed_deaths[which(iso_week == 7)],
                    observed_deaths[which(iso_week %in% c(7:25))],
                    (3/7)*observed_deaths[which(iso_week == 26)])) %>%
               ungroup()

stmf0506 <- stmf %>% 
  filter(epi_year %in% c("2005/2006")) %>%
  group_by(epi_year, sex, age_group, country_code) %>%
  summarize( earlier_obs_deaths = 
               sum( (3/7)*observed_deaths[which(iso_week == 27)],
                    observed_deaths[which(iso_week %in% c(27:52))],
                    observed_deaths[which(iso_week %in% c(1:5))],
                    (4/7)*observed_deaths[which(iso_week == 6)]),
             later_obs_deaths = 
               sum( (3/7)*observed_deaths[which(iso_week == 7)],
                    observed_deaths[which(iso_week %in% c(7:25))],
                    (5/7)*observed_deaths[which(iso_week == 26)])) %>%
               ungroup()

stmf0607 <- stmf %>% 
  filter(epi_year %in% c("2006/2007")) %>%
  group_by(epi_year, sex, age_group, country_code) %>%
  summarize( earlier_obs_deaths = 
               sum( (2/7)*observed_deaths[which(iso_week == 27)],
                    observed_deaths[which(iso_week %in% c(27:52))],
                    observed_deaths[which(iso_week %in% c(1:5))],
                    (5/7)*observed_deaths[which(iso_week == 6)]),
             later_obs_deaths = 
               sum( (2/7)*observed_deaths[which(iso_week == 7)],
                    observed_deaths[which(iso_week %in% c(7:25))],
                    (6/7)*observed_deaths[which(iso_week == 26)])) %>%
               ungroup()

stmf0708 <- stmf %>% 
  filter(epi_year %in% c("2007/2008")) %>%
  group_by(epi_year, sex, age_group, country_code) %>%
  summarize( earlier_obs_deaths = 
               sum( (1/7)*observed_deaths[which(iso_week == 27)],
                    observed_deaths[which(iso_week %in% c(27:52))],
                    observed_deaths[which(iso_week %in% c(1:5))],
                    (6/7)*observed_deaths[which(iso_week == 6)]),
             later_obs_deaths = 
               sum( (1/7)*observed_deaths[which(iso_week == 7)],
                    observed_deaths[which(iso_week %in% c(7:26))])) %>%
               ungroup()

stmf0809 <- stmf %>% 
  filter(epi_year %in% c("2008/2009")) %>%
  group_by(epi_year, sex, age_group, country_code) %>%
  summarize( earlier_obs_deaths = 
               sum( (6/7)*observed_deaths[which(iso_week == 27)],
                    observed_deaths[which(iso_week %in% c(28:52))],
                    observed_deaths[which(iso_week %in% c(1:6))],
                    (1/7)*observed_deaths[which(iso_week == 6)]),
             later_obs_deaths = 
               sum( (6/7)*observed_deaths[which(iso_week == 7)],
                    observed_deaths[which(iso_week %in% c(8:26))],
                    (2/7)*observed_deaths[which(iso_week == 26)])) %>%
  ungroup()

stmf0910 <- stmf %>% 
  filter(epi_year %in% c("2009/2010")) %>%
  group_by(epi_year, sex, age_group, country_code) %>%
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

stmf1011 <- stmf %>% 
  filter(epi_year %in% c("2010/2011")) %>%
  group_by(epi_year, sex, age_group, country_code) %>%
  summarize(earlier_obs_deaths = 
              sum( (4/7)*observed_deaths[which(iso_week == 27)],
                   observed_deaths[which(iso_week %in% c(27:52))],
                   observed_deaths[which(iso_week %in% c(1:5))],
                   (3/7)*observed_deaths[which(iso_week == 6)]),
            later_obs_deaths =  
              sum( (4/7)*observed_deaths[which(iso_week == 7)],
                   observed_deaths[which(iso_week %in% c(7:25))],
                   (4/7)*observed_deaths[which(iso_week == 26)]) ) %>%
  ungroup()

stmf1112 <- stmf %>% 
  filter(epi_year %in% c("2011/2012")) %>%
  group_by(epi_year, sex, age_group, country_code) %>%
  summarize( earlier_obs_deaths = 
               sum( (3/7)*observed_deaths[which(iso_week == 27)],
                    observed_deaths[which(iso_week %in% c(27:52))],
                    observed_deaths[which(iso_week %in% c(1:5))],
                    (4/7)*observed_deaths[which(iso_week == 6)]),
             later_obs_deaths = 
               sum( (3/7)*observed_deaths[which(iso_week == 7)],
                    observed_deaths[which(iso_week %in% c(7:25))],
                    (5/7)*observed_deaths[which(iso_week == 26)]) ) %>%
  ungroup()

stmf1213 <- stmf %>% 
  filter(epi_year %in% c("2012/2013")) %>%
  group_by(epi_year, sex, age_group, country_code) %>%
  summarize( earlier_obs_deaths = 
               sum( (1/7)*observed_deaths[which(iso_week == 27)],
                    observed_deaths[which(iso_week %in% c(27:52))],
                    observed_deaths[which(iso_week %in% c(1:5))],
                    (6/7)*observed_deaths[which(iso_week == 6)] ),
             later_obs_deaths = 
               sum( (1/7)*observed_deaths[which(iso_week == 7)],
                    observed_deaths[which(iso_week %in% c(7:26))]) ) %>%
  ungroup()

stmf1314 <- stmf %>% 
  filter(epi_year %in% c("2013/2014")) %>%
  group_by(epi_year, sex, age_group, country_code) %>%
  summarize( earlier_obs_deaths = 
               sum( observed_deaths[which(iso_week %in% c(27:52))],
                    observed_deaths[which(iso_week %in% c(1:6))] ),
             later_obs_deaths = 
               sum( observed_deaths[which(iso_week %in% c(7:26))],
                    (1/7)*observed_deaths[which(iso_week == 26)]) ) %>%
  ungroup()

stmf1415 <- stmf %>% 
  filter(epi_year %in% c("2014/2015")) %>%
  group_by(epi_year, sex, age_group, country_code) %>%
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

stmf1516 <- stmf %>% 
  filter(epi_year %in% c("2015/2016")) %>%
  group_by(epi_year, sex, age_group, country_code) %>%
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

stmf1617 <- stmf %>% 
  filter(epi_year %in% c("2016/2017")) %>%
  group_by(epi_year, sex, age_group, country_code) %>%
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

stmf1718 <- stmf %>% 
  filter(epi_year %in% c("2017/2018")) %>%
  group_by(epi_year, sex, age_group, country_code) %>%
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

stmf1819 <- stmf %>% 
  filter(epi_year %in% c("2018/2019")) %>%
  group_by(epi_year, sex, age_group, country_code) %>%
  summarize( earlier_obs_deaths = 
               sum( (1/7)*observed_deaths[which(iso_week == 27)],
                    observed_deaths[which(iso_week %in% c(27:52))],
                    observed_deaths[which(iso_week %in% c(1:5))],
                    (6/7)*observed_deaths[which(iso_week == 6)]),
             later_obs_deaths = 
               sum( (1/7)*observed_deaths[which(iso_week == 7)],
                    observed_deaths[which(iso_week %in% c(7:26))]) ) %>%
  ungroup()

stmf1920 <- stmf %>% 
  filter(epi_year %in% c("2019/2020")) %>%
  group_by(epi_year, sex, age_group, country_code) %>%
  summarize( earlier_obs_deaths = 
               sum( observed_deaths[which(iso_week %in% c(27:52))],
                    observed_deaths[which(iso_week %in% c(1:6))] ),
             later_obs_deaths = 
               sum( observed_deaths[which(iso_week %in% c(7:26))],
                    (1/7)*observed_deaths[which(iso_week == 26)]) ) %>%
  ungroup()

# Combine all the epi-years
stmf <- bind_rows(stmf0405, stmf0506, stmf0607, stmf0708, stmf0809, 
                  stmf0910, stmf1011, stmf1112, stmf1213, stmf1314, 
                  stmf1415, stmf1516, stmf1617, stmf1718, stmf1819, 
                  stmf1920)




###### Later/earlier ratios (new - based on 5,10,15 years of time series) ######

# Compute ratios D+/D-, average, sd and cv of upsilon 
# by epi_year, sex, age_group and country (excluding year 2019/2020)

###### Later/earlier ratios - 5 years 

stmf5y <- stmf %>%
  mutate(upsilon = later_obs_deaths/earlier_obs_deaths) %>%
  group_by(sex, age_group, country_code) %>%
  summarize(av_upsilon = mean(upsilon[which(epi_year %in% c("2014/2015","2015/2016","2016/2017","2017/2018","2018/2019"))]),
            sd_upsilon = sd(upsilon[which(epi_year %in% c("2014/2015","2015/2016","2016/2017","2017/2018","2018/2019"))]),
            cv_upsilon = sd_upsilon/av_upsilon) %>%
  mutate(years = "5 years") %>%
  ungroup()

###### Later/earlier ratios - 10 years 

stmf10y <- stmf %>%
  mutate(upsilon = later_obs_deaths/earlier_obs_deaths) %>%
  group_by(sex, age_group, country_code) %>%
  summarize(av_upsilon = mean(upsilon[which(epi_year %in% c("2009/2010","2010/2011","2011/2012","2012/2013","2013/2014",
                                                            "2012/2013","2015/2016","2016/2017","2017/2018","2018/2019"))]),
            sd_upsilon = sd(upsilon[which(epi_year %in% c("2009/2010","2010/2011","2011/2012","2013/2014","2013/2014",
                                                          "2014/2015","2015/2016","2016/2017","2017/2018","2018/2019"))]),
            cv_upsilon = sd_upsilon/av_upsilon) %>%
  mutate(years = "10 years") %>%
  ungroup()

###### Later/earlier ratios - 15 years

stmf15y <- stmf %>%
  mutate(upsilon = later_obs_deaths/earlier_obs_deaths) %>%
  group_by(sex, age_group, country_code) %>%
  summarize(av_upsilon = mean(upsilon[which(epi_year %in% c("2004/2005","2005/2006","2006/2007","2007/2008","2008/2009",
                                                            "2009/2010","2010/2011","2011/2012","2012/2013","2013/2014",
                                                            "2012/2013","2015/2016","2016/2017","2017/2018","2018/2019"))]),
            sd_upsilon = sd(upsilon[which(epi_year %in% c("2004/2005","2005/2006","2006/2007","2007/2008","2008/2009",
                                                          "2009/2010","2010/2011","2011/2012","2013/2014","2013/2014",
                                                          "2014/2015","2015/2016","2016/2017","2017/2018","2018/2019"))]),
            cv_upsilon = sd_upsilon/av_upsilon) %>%
  mutate(years = "15 years") %>%
  ungroup()

stmf.yall <- rbind(stmf5y,stmf10y,stmf15y)
stmf.yall$years <- as.factor(stmf.yall$years)
stmf.yall$years <- factor(stmf.yall$years, 
                          levels = c("5 years", "10 years", "15 years"))

# stmf.yall
# # A tibble: 60 x 7
#   sex    age_group country_code av_upsilon sd_upsilon cv_upsilon years  
#   <fct>  <fct>     <fct>             <dbl>      <dbl>      <dbl> <fct>  
# 1 Female [0,15)    France            0.606    0.0217     0.0358  5 years
# 2 Female [0,15)    Spain             0.589    0.0472     0.0800  5 years
# 3 Female [15,65)   France            0.625    0.0130     0.0207  5 years
# 4 Female [15,65)   Spain             0.622    0.00494    0.00794 5 years
# 5 Female [65,75)   France            0.644    0.0109     0.0170  5 years
# 6 Female [65,75)   Spain             0.629    0.0211     0.0335  5 years
# 7 Female [75,85)   France            0.623    0.0260     0.0418  5 years
# 8 Female [75,85)   Spain             0.622    0.0191     0.0307  5 years
# 9 Female 85+       France            0.637    0.0412     0.0646  5 years
# 10 Female 85+       Spain             0.643    0.0255     0.0397  5 years
# # ... with 50 more rows




###### Figure A1 ######


# Define a transformation of the y-axis, encapsulated in trans_new() method

library(scales)
squish_trans <- function(from, to, factor) {
  
  trans <- function(x) {
    if (any(is.na(x))) return(x)
    # get indices for the relevant regions
    isq <- x > from & x < to
    ito <- x >= to
    # apply transformation
    x[isq] <- from + (x[isq] - from)/factor
    x[ito] <- from + (to - from)/factor + (x[ito] - to)
    return(x)
  }
  
  inv <- function(x) {
    if (any(is.na(x))) return(x)
    # get indices for the relevant regions
    isq <- x > from & x < from + (to - from)/factor
    ito <- x >= from + (to - from)/factor
    # apply transformation
    x[isq] <- from + (x[isq] - from) * factor
    x[ito] <- to + (x[ito] - (from + (to - from)/factor))
    return(x)
  }
  
  # return the transformation
  return(trans_new("squished", trans, inv))
}



setwd(dir.out)
pdf("FigA1_ESP_FRANTP_sensitivity_v1.pdf", width = 10, height = 12)

stmf.yall %>% 
  ggplot(aes(x = age_group)) +
  # Average upsilon
  geom_point(aes(y = av_upsilon, colour = sex), shape = 16, size = 3) +
  # Standard deviation
  geom_errorbar(aes(ymin = av_upsilon-sd_upsilon,
                    ymax = av_upsilon+sd_upsilon,
                    colour = sex),
                width=.3) + 
  # Coefficients of variation
  geom_point(aes(y = cv_upsilon, colour = sex), shape = 8, size=3) + 
  facet_grid(rows = vars(years), cols = vars(country_code)) + 
  theme_bw() +
  labs(x = "Age group", y = "Later/earlier ratios - mean, sd, cv", color = "Sex") +
  theme(
    # Axis 
    axis.title.x = element_text(size = 16, face = "bold"),
    axis.title.y = element_text(size = 16, face = "bold"),
    axis.text.x = element_text(size = 13, angle = 45, vjust = 1, hjust = 1),
    axis.text.y = element_text(size = 13),
    # Legend 
    legend.title = element_text(size = 16, face = "bold"),
    legend.position = "bottom",
    legend.text=element_text(size=13),
    # Facet 
    strip.text.x = element_text(size = 13),
    strip.text.y = element_text(size = 13)) +
  # Squash the unused part of y-axis by a factor of 10
  scale_y_continuous(trans = squish_trans(0.1, 0.5, 5),
                     breaks = seq(0, 0.7, by = 0.1)) 
  
dev.off()

















