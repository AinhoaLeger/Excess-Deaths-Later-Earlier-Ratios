
#########################################################################
# Author: Ainhoa Elena Leger
# Last update: 26-03-2022
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

# Load xstmf (elaborated by Silvi and Jonas) for comparisons 
load("2020-12-16-xstmf.RData")

# Load file downloaded from ECDC for covid deaths
ecdc <- readxl::read_xlsx("2020-21-10-ecdc.xlsx")




###### Data preparation ######

# Select desired variables and observations
# Transform the dataset from wide to long format

stmf <- stmf %>%
  select(CountryCode, Year, Week, Sex, D0_14:D85p) %>% 
  filter(CountryCode %in% c("FRATNP", "ESP"),
         Year %in% c(2009:2020),
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
# and iso-weeks 1-26 of the following year
# Example: epi-year 2009/2010 is defined by iso-weeks 27-53 of 2009
# and iso-weeks 1-26 of 2010

stmf$epi_year=0

for(i in c(1:dim(stmf)[1])){

  ## Years with 53 weeks (2009)
  if (stmf$year[i] %in% c(2009, 2015, 2020)) {
    if (stmf$iso_week[i]<=26) {
      stmf$epi_year[i] = paste0(stmf$year[i]-1, "/", stmf$year[i]) }   
    else if (stmf$iso_week[i]>=27 & stmf$iso_week[i]<=53) {
      stmf$epi_year[i] = paste0(stmf$year[i], "/", stmf$year[i]+1) }
    }
  
  ## Years with 52 weeks
  else if (stmf$year[i] %in% c(2010:2014, 2016:2019)) {
    if (stmf$iso_week[i]<=26) {
    stmf$epi_year[i] = paste0(stmf$year[i]-1, "/", stmf$year[i]) }
    else if (stmf$iso_week[i]>=27 & stmf$iso_week[i]<=52) {
    stmf$epi_year[i] = paste0(stmf$year[i], "/", stmf$year[i]+1) }
  }
}




###### Later/earlier observed deaths (gregorian calendar) ######

# Compute D- and D+ by epi_year, sex, age_group and country

stmf0910 <- stmf %>% 
  filter(epi_year %in% c("2009/2010")) %>%
  group_by(epi_year, sex, age_group, country_code) %>%
  summarize( earlier_obs_deaths = 
               sum( (5/7)*observed_deaths[which(iso_week == 27)],
                    observed_deaths[which(iso_week %in% c(27:53))],
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
stmf <- bind_rows(stmf0910, stmf1011, stmf1112, stmf1213, stmf1314, stmf1415, 
                  stmf1516, stmf1617, stmf1718, stmf1819, stmf1920)




###### FIGURE 2 ######

ratios.tot <- stmf %>%
  group_by(epi_year, country_code) %>%
  # Sum deaths on the later and earlier peroid by epi-year and country_code
  summarize(earlier_obs_deaths_tot = sum(earlier_obs_deaths),
            later_obs_deaths_tot = sum(later_obs_deaths)) %>%
  distinct(country_code, epi_year, .keep_all = TRUE) %>%
  # Later/earlier ratios by country and year
  mutate(upsilon = later_obs_deaths_tot/earlier_obs_deaths_tot) %>%
  group_by(country_code) %>%
  # Average, sd, and cv
  mutate(av_upsilon = mean(upsilon[which(epi_year!="2019/2020")]),
         sd_upsilon = sd(upsilon[which(epi_year!="2019/2020")]),
         cv_upsilon = sd_upsilon/av_upsilon,
         lab = paste("mean =", round(av_upsilon,3), 
                     ",\nsd =", round(sd_upsilon,3), 
                     ",\ncv =", round(cv_upsilon,4)*100, "%")) %>%
  ungroup()

ratios.av <- ratios.tot %>%
  distinct(country_code, av_upsilon, .keep_all = TRUE) %>%
  select(c(country_code, av_upsilon, lab))

ratios.tot <- as.data.frame(ratios.tot)
ratios.av <- as.data.frame(ratios.av)


fig2 <- ratios.tot %>%
  filter(epi_year != "2019/2020") %>%
  ggplot(aes(x = epi_year, y = upsilon, color = country_code)) + 
  geom_point(size = 3) + 
  scale_color_brewer(palette = "Set1", direction=-1)+
  theme_minimal() + 
  facet_grid(rows = vars(country_code)) +
  geom_text(aes(label = country_code), x = 9, y = 0.67, 
            hjust = 1.5, vjust = 1.5, size = 7) +
  geom_line(aes(group = country_code)) +
  labs(x = "Epiyear", y = "Later/earlier ratio", color = "Country") +
  theme(
    # Axis 
    axis.title.x = element_text(size = 20, face = "bold"),
    axis.title.y = element_text(size = 20, face = "bold"),
    axis.text.x = element_text(size = 17, angle = 45, vjust = 1, hjust = 1),
    axis.text.y = element_text(size = 17),
    # Legend
    legend.position = "none",
    # Facet
    strip.background = element_blank(),
    strip.text = element_blank()) + 
  scale_x_discrete(breaks=c("2009/2010","2010/2011","2011/2012",
                            "2012/2013","2013/2014","2014/2015",
                            "2015/2016","2016/2017","2017/2018","2018/2019"),
                   labels=c("2009-10","2010-11","2011-12",
                            "2012-13","2013-14","2014-15",
                            "2015-16","2016-17","2017-18","2018-19")) +
  geom_hline(data = ratios.av, aes(yintercept = av_upsilon, color = country_code), size=1.25) 

setwd(dir.out)
ggsave("Fig2_national.png", plot = fig2, width = 12 , height =8)




###### Later/earlier ratios ######

# Compute ratios D+/D-, average, sd and cv of upsilon 
# by epi_year, sex, age_group and country (excluding year 2019/2020)

stmf1 <- stmf %>%
  mutate(upsilon = later_obs_deaths/earlier_obs_deaths) %>%
  group_by(sex, age_group, country_code) %>%
  mutate(av_upsilon = mean(upsilon[which(epi_year!="2019/2020")]),
         sd_upsilon = sd(upsilon[which(epi_year!="2019/2020")]),
         cv_upsilon = sd_upsilon/av_upsilon,
         lab = paste("mean =", round(av_upsilon,2), 
                     ",\nsd =", round(sd_upsilon,2), 
                     ",\ncv =", round(cv_upsilon,4)*100, "%")) %>%
  ungroup() %>%
  distinct(sex, age_group, country_code, epi_year, .keep_all = TRUE) %>%
  select(c(country_code, sex, age_group, epi_year,
           earlier_obs_deaths, later_obs_deaths, 
           upsilon, av_upsilon, sd_upsilon, cv_upsilon, lab))
  

# as.data.frame(stmf1)
#    country_code    sex age_group  epi_year earlier_obs_deaths later_obs_deaths   upsilon
# 1        France Female    [0,15) 2009/2010          1185.9933         691.7549 0.5832705
# 2         Spain Female    [0,15) 2009/2010           693.1429         375.2857 0.5414262
# 3        France Female   [15,65) 2009/2010         21378.6823       12996.7262 0.6079292
# 4         Spain Female   [15,65) 2009/2010         11568.4286        6833.7143 0.5907211
# 5        France Female   [65,75) 2009/2010         15902.1115        9442.2272 0.5937719
# 6         Spain Female   [65,75) 2009/2010         11233.5714        6818.1429 0.6069435
# 7        France Female   [75,85) 2009/2010         44767.6791       27863.9819 0.6224129
# 8         Spain Female   [75,85) 2009/2010         35745.1429       21624.4286 0.6049613
# 9        France Female       85+ 2009/2010         80965.1767       51634.8812 0.6377418
# 10        Spain Female       85+ 2009/2010         56132.1429       35279.4286 0.6285067
# 11       France   Male    [0,15) 2009/2010          1522.6827         915.7402 0.6013992
# 12        Spain   Male    [0,15) 2009/2010           843.4286         460.2857 0.5457317
# 13       France   Male   [15,65) 2009/2010         45937.1193       28223.1302 0.6143862
# 14        Spain   Male   [15,65) 2009/2010         26393.4286       15745.0000 0.5965500
# 15       France   Male   [65,75) 2009/2010         28660.2830       18073.9624 0.6306275
# 16        Spain   Male   [65,75) 2009/2010         22613.7143       13727.2857 0.6070337
# 17       France   Male   [75,85) 2009/2010         52222.9875       32401.7876 0.6204507
# 18        Spain   Male   [75,85) 2009/2010         42186.4286       26316.5714 0.6238161
# 19       France   Male       85+ 2009/2010         43648.0347       27827.6653 0.6375468
# 20        Spain   Male       85+ 2009/2010         30946.4286       19954.5714 0.6448102




###### FIGURE S2 ######

setwd(dir.out)
pdf("FigS2_national.pdf", width = 10, height = 12)

stmf1 %>% 
  filter(epi_year != "2019/2020") %>%
  ggplot(aes(upsilon, epi_year, colour = age_group)) + 
  geom_point(size = 3) + 
  facet_grid(rows = vars(country_code, sex), 
             cols = vars(age_group)) +   
  theme_bw() + 
  labs(x = "Later/earlier ratio", y = "Epiyear", color = "Age group") +
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
  geom_vline(data = stmf1, aes(xintercept = av_upsilon, colour = age_group),
             size = 2, alpha = 0.5, show.legend = FALSE) +
  geom_text(data = stmf1, aes(label = lab, fontface = "plain"),
            x = 0.625, y = -0.2, size = 4, stat = "unique", 
            lineheight = 0.7) +
  geom_segment(aes(x = upsilon, xend = av_upsilon,
                   y = epi_year, yend = epi_year),
               size = 0.7) +
  expand_limits(y = c(-1.5, 11))

dev.off()




###### Stationary series ######

# Ljung-Box test  
# Augmented Dickey-Fuller (ADF) 
# Kwiatkowski-Phillips-Schmidt-Shin (KPSS)

library(tseries)
options(warn=-1)
lag.length = 10


##### By country

ratios.france <- ratios.tot %>% filter(country_code=="France") %>% select(upsilon)
ratios.spain <- ratios.tot %>% filter(country_code=="Spain") %>% select(upsilon)
ratios.france <- pull(ratios.france)
ratios.spain <- pull(ratios.spain)

Box.test(ratios.france, lag=lag.length, type="Ljung-Box") 
adf.test(ratios.france)
# p-value = 0.8825
kpss.test(ratios.france, null="Trend")

Box.test(ratios.spain, lag=lag.length, type="Ljung-Box") 
adf.test(ratios.spain)
# p-value = 0.9538
kpss.test(ratios.spain, null="Trend")

##### By country, sex, age

ratios.fr.fem.0 <- stmf1 %>% filter(country_code=="France", sex=="Female", age_group=="[0,15)") %>% select(upsilon) 
ratios.fr.fem.0 <- pull(ratios.fr.fem.0)
Box.test(ratios.fr.fem.0, lag=lag.length, type="Ljung-Box") 
# p-value = 0.02184
adf.test(ratios.fr.fem.0)
kpss.test(ratios.fr.fem.0, null="Trend")

ratios.fr.fem.15 <- stmf1 %>% filter(country_code=="France", sex=="Female", age_group=="[15,65)") %>% select(upsilon) 
ratios.fr.fem.15 <- pull(ratios.fr.fem.15)
Box.test(ratios.fr.fem.15, lag=lag.length, type="Ljung-Box") 
adf.test(ratios.fr.fem.15)
# p-value = 0.6507
kpss.test(ratios.fr.fem.15, null="Trend")

ratios.fr.fem.65 <- stmf1 %>% filter(country_code=="France", sex=="Female", age_group=="[65,75)") %>% select(upsilon)  
ratios.fr.fem.65 <- pull(ratios.fr.fem.65)
Box.test(ratios.fr.fem.65, lag=lag.length, type="Ljung-Box") 
# p-value = 0.04511
adf.test(ratios.fr.fem.65)
# p-value = 0.6727
kpss.test(ratios.fr.fem.65, null="Trend")

ratios.fr.fem.75 <- stmf1 %>% filter(country_code=="France", sex=="Female", age_group=="[75,85)") %>% select(upsilon)  
ratios.fr.fem.75 <- pull(ratios.fr.fem.75)
Box.test(ratios.fr.fem.75, lag=lag.length, type="Ljung-Box") 
adf.test(ratios.fr.fem.75)
# p-value = 0.9438
kpss.test(ratios.fr.fem.75, null="Trend")

ratios.fr.fem.85 <- stmf1 %>% filter(country_code=="France", sex=="Female", age_group=="85+") %>% select(upsilon)  
ratios.fr.fem.85 <- pull(ratios.fr.fem.85)
Box.test(ratios.fr.fem.85, lag=lag.length, type="Ljung-Box") 
adf.test(ratios.fr.fem.85)
# p-value = 0.9322
kpss.test(ratios.fr.fem.85, null="Trend")


ratios.fr.mal.0 <- stmf1 %>%filter(country_code=="France", sex=="Male", age_group=="[0,15)") %>% select(upsilon)  
ratios.fr.mal.0 <- pull(ratios.fr.mal.0)
Box.test(ratios.fr.mal.0, lag=lag.length, type="Ljung-Box") 
# stationary
adf.test(ratios.fr.mal.0)
# p-value = 0.09853
kpss.test(ratios.fr.mal.0, null="Trend")

ratios.fr.mal.15 <- stmf1 %>% filter(country_code=="France", sex=="Male", age_group=="[15,65)") %>% select(upsilon)  
ratios.fr.mal.15 <- pull(ratios.fr.mal.15)
Box.test(ratios.fr.mal.15, lag=lag.length, type="Ljung-Box") 
# stationary
adf.test(ratios.fr.mal.15)
# p-value = 0.6889
kpss.test(ratios.fr.mal.15, null="Trend")

ratios.fr.mal.65 <- stmf1 %>% filter(country_code=="France", sex=="Male", age_group=="[65,75)") %>% select(upsilon) 
ratios.fr.mal.65 <- pull(ratios.fr.mal.65)
Box.test(ratios.fr.mal.65, lag=lag.length, type="Ljung-Box") 
# stationary
adf.test(ratios.fr.mal.65)
# p-value = 0.6196
kpss.test(ratios.fr.mal.65, null="Trend")

ratios.fr.mal.75 <- stmf1 %>% filter(country_code=="France", sex=="Male", age_group=="[75,85)") %>% select(upsilon)  
ratios.fr.mal.75 <- pull(ratios.fr.mal.75)
Box.test(ratios.fr.mal.75, lag=lag.length, type="Ljung-Box") 
# stationary
adf.test(ratios.fr.mal.75)
# p-value = 0.8901
kpss.test(ratios.fr.mal.75, null="Trend")

ratios.fr.mal.85 <- stmf1 %>% filter(country_code=="France", sex=="Male", age_group=="85+") %>% select(upsilon) 
ratios.fr.mal.85 <- pull(ratios.fr.mal.85)
Box.test(ratios.fr.mal.85, lag=lag.length, type="Ljung-Box") 
adf.test(ratios.fr.mal.85)
# p-value = 0.8424
kpss.test(ratios.fr.mal.85, null="Trend")


ratios.sp.fem.0 <- stmf1 %>% filter(country_code=="Spain", sex=="Female", age_group=="[0,15)") %>% select(upsilon)  
ratios.sp.fem.0 <- pull(ratios.sp.fem.0)
Box.test(ratios.sp.fem.0, lag=lag.length, type="Ljung-Box") 
adf.test(ratios.sp.fem.0)
# p-value = 0.7737
kpss.test(ratios.sp.fem.0, null="Trend")
# 0.02614

ratios.sp.fem.15 <- stmf1 %>% filter(country_code=="Spain", sex=="Female", age_group=="[15,65)") %>% select(upsilon)  
ratios.sp.fem.15 <- pull(ratios.sp.fem.15)
Box.test(ratios.sp.fem.15, lag=lag.length, type="Ljung-Box") 
# p-value = 0.00606
adf.test(ratios.sp.fem.15)
# p-value = 0.9616
kpss.test(ratios.sp.fem.15, null="Trend")

ratios.sp.fem.65 <- stmf1 %>% filter(country_code=="Spain", sex=="Female", age_group=="[65,75)") %>% select(upsilon)  
ratios.sp.fem.65 <- pull(ratios.sp.fem.65)
Box.test(ratios.sp.fem.65, lag=lag.length, type="Ljung-Box") 
adf.test(ratios.sp.fem.65)
# p-value = 0.4104
kpss.test(ratios.sp.fem.65, null="Trend")

ratios.sp.fem.75 <- stmf1 %>% filter(country_code=="Spain", sex=="Female", age_group=="[75,85)") %>% select(upsilon)  
ratios.sp.fem.75 <- pull(ratios.sp.fem.75)
Box.test(ratios.sp.fem.75, lag=lag.length, type="Ljung-Box") 
adf.test(ratios.sp.fem.75)
# p-value = 0.9802
kpss.test(ratios.sp.fem.75, null="Trend")

ratios.sp.fem.85 <- stmf1 %>% filter(country_code=="Spain", sex=="Female", age_group=="85+") %>% select(upsilon)  
ratios.sp.fem.85 <- pull(ratios.sp.fem.85)
Box.test(ratios.sp.fem.85, lag=lag.length, type="Ljung-Box") 
adf.test(ratios.sp.fem.85)
# p-value = 0.9733
kpss.test(ratios.sp.fem.85, null="Trend")


ratios.sp.mal.0 <- stmf1 %>% filter(country_code=="Spain", sex=="Male", age_group=="[0,15)") %>% select(upsilon)  
ratios.sp.mal.0 <- pull(ratios.sp.mal.0)
Box.test(ratios.sp.mal.0, lag=lag.length, type="Ljung-Box") 
adf.test(ratios.sp.mal.0)
# p-value = 0.8718
kpss.test(ratios.sp.mal.0, null="Trend")

ratios.sp.mal.15 <- stmf1 %>% filter(country_code=="Spain", sex=="Male", age_group=="[15,65)") %>% select(upsilon)  
ratios.sp.mal.15 <- pull(ratios.sp.mal.15)
Box.test(ratios.sp.mal.15, lag=lag.length, type="Ljung-Box") 
adf.test(ratios.sp.mal.15)
# p-value = 0.1889
kpss.test(ratios.sp.mal.15, null="Trend")

ratios.sp.mal.65 <- stmf1 %>% filter(country_code=="Spain", sex=="Male", age_group=="[65,75)") %>% select(upsilon) 
ratios.sp.mal.65 <- pull(ratios.sp.mal.65)
Box.test(ratios.sp.mal.65, lag=lag.length, type="Ljung-Box") 
adf.test(ratios.sp.mal.65)
# p-value = 0.8855
kpss.test(ratios.sp.mal.65, null="Trend")

ratios.sp.mal.75 <- stmf1 %>% filter(country_code=="Spain", sex=="Male", age_group=="[75,85)") %>% select(upsilon)  
ratios.sp.mal.75 <- pull(ratios.sp.mal.75)
Box.test(ratios.sp.mal.75, lag=lag.length, type="Ljung-Box") 
adf.test(ratios.sp.mal.75)
# p-value = 0.8956
kpss.test(ratios.sp.mal.75, null="Trend")

ratios.sp.mal.85 <- stmf1 %>% filter(country_code=="Spain", sex=="Male", age_group=="85+") %>% select(upsilon) 
ratios.sp.mal.85 <- pull(ratios.sp.mal.85)
Box.test(ratios.sp.mal.85, lag=lag.length, type="Ljung-Box") 
adf.test(ratios.sp.mal.85)
# p-value = 0.9569
kpss.test(ratios.sp.mal.85, null="Trend")




###### Excess deaths by age and sex ######

# Compute expected dx and excess dx for year 2019/2020 by sex, age_group, country_code
stmf2 <- stmf1 %>% 
  group_by(sex, age_group, country_code) %>%
  mutate(later_exp_deaths = 
           ifelse(epi_year=="2019/2020", round(earlier_obs_deaths*av_upsilon),
                  ifelse(epi_year!="2019/2020", NA, 999)),
         excess_deaths = 
           ifelse(epi_year=="2019/2020", later_obs_deaths - later_exp_deaths,
                  ifelse(epi_year!="2019/2020", NA, 999))) %>%
  ungroup()%>%
  # Subset to later segment data for 2019
  filter(epi_year == "2019/2020") %>%
  distinct(sex, age_group, country_code, .keep_all = TRUE) %>%
  select(c(country_code, sex, age_group, 
           later_obs_deaths, later_exp_deaths, excess_deaths))

# as.data.frame(stmf2)
#    country_code    sex age_group later_obs_deaths later_exp_deaths excess_deaths
# 1        France Female    [0,15)         551.7313              592    -40.268672
# 2         Spain Female    [0,15)         250.5714              242      8.571429
# 3        France Female   [15,65)       12183.5244            11873    310.524429
# 4         Spain Female   [15,65)        8080.4286             7264    816.428571
# 5        France Female   [65,75)       13717.8673            12891    826.867271
# 6         Spain Female   [65,75)        8907.8571             7311   1596.857143
# 7        France Female   [75,85)       24931.1897            22350   2581.189747
# 8         Spain Female   [75,85)       23185.2857            17291   5894.285714
# 9        France Female       85+       75652.5444            69255   6397.544369
# 10        Spain Female       85+       62528.2857            47868  14660.285714
# 11       France   Male    [0,15)         719.0232              786    -66.976792
# 12        Spain   Male    [0,15)         305.0000              317    -12.000000
# 13       France   Male   [15,65)       23995.2820            22623   1372.281973
# 14        Spain   Male   [15,65)       15627.7143            14322   1305.714286
# 15       France   Male   [65,75)       25486.4185            23535   1951.418458
# 16        Spain   Male   [65,75)       18123.0000            14579   3544.000000
# 17       France   Male   [75,85)       32195.0209            28023   4172.020936
# 18        Spain   Male   [75,85)       30405.5714            22673   7732.571429
# 19       France   Male       85+       44517.2554            40173   4344.255425
# 20        Spain   Male       85+       38708.4286            29750   8958.428571




###### Bootstrap by age and sex ###### 

stmf3 <- stmf %>%
  mutate(upsilon = later_obs_deaths/earlier_obs_deaths) %>%
  arrange(country_code, sex, age_group)
as.data.frame(stmf3)

# Series of 10 ratios from 2009/2010 to 2018/2019 (excluding 19/20)
v <- stmf3 %>%
  # arrange(country_code, sex, age_group) %>%
  filter(epi_year!="2019/2020") %>%
  select(country_code, sex, age_group, epi_year, upsilon)
v <- pivot_wider(v, names_from = epi_year, values_from = upsilon)
as.data.frame(v)

# Observed deaths on the earlier and later segment
obs_data <- stmf3 %>%
  arrange(country_code, sex, age_group) %>%
  filter(epi_year=="2019/2020") %>%
  select(country_code, sex, age_group, epi_year,
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
    rstar[i,j] <- sample(as.numeric(v[i,4:13]), size=1, replace=T)
    
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
                          round(rowQuantiles(Risk.obs.star, 
                                             probs=c(0.025,0.975))*100,1))

# I derive the empirical percentiles for excess deaths
tab.int.risk.exp <- cbind(as.data.frame(obs_data[1:3]),
                          round(rowQuantiles(Risk.exp.star, 
                                             probs=c(0.025,0.975))*100,1))



###### TABLE S2 ######

# Excess death

stmf4 <- stmf2 %>%
  mutate(risk_obs = round((excess_deaths/later_obs_deaths)*100,1),
         risk_exp = round((excess_deaths/later_exp_deaths)*100,1))

stmf4[,c(4:6)] <- round(stmf4[,c(4:6)])
stmf4 <- as.data.frame(stmf4)

# Prediction Interval - quantiles

colnames(tab.int.exp)[4:5] <- c("expected_low_int","expected_upp_int")
colnames(tab.int.exc)[4:5] <- c("excess_low_int","excess_upp_int")
colnames(tab.int.risk.exp)[4:5] <- c("risk_exp_low_int","risk_exp_upp_int")
colnames(tab.int.risk.obs)[4:5] <- c("risk_obs_low_int","risk_obs_upp_int")

stmf4 <- 
  left_join(stmf4, tab.int.exp, by=c("country_code","sex","age_group")) %>%
  left_join(., tab.int.exc, by=c("country_code","sex","age_group")) %>%
  left_join(., tab.int.risk.exp, by=c("country_code","sex","age_group")) %>%
  left_join(., tab.int.risk.obs, by=c("country_code","sex","age_group"))

tab_excess <- 
  reshape2::dcast(
    reshape2::melt(stmf4, 
                   id.vars=c("age_group", "country_code", "sex")), 
  age_group+variable~country_code+sex)

setwd(dir.out)
write_xlsx(tab_excess,"TabS2_national.xlsx")




###### Bootstrap total ######

stmf5 <- stmf %>%
  group_by(country_code, epi_year) %>%
  summarize(earlier_obs_deaths = sum(earlier_obs_deaths),
            later_obs_deaths = sum(later_obs_deaths),
            upsilon = later_obs_deaths/earlier_obs_deaths)
as.data.frame(stmf5)

# Series of 10 ratios from 2009/2010 to 2018/2019 (excluding 19/20)
v <- stmf5 %>%
  filter(epi_year!="2019/2020") %>%
  select(country_code, epi_year, upsilon)
v <- pivot_wider(v, names_from = epi_year, values_from = upsilon)
as.data.frame(v)

# Observed deaths on the earlier and later segment
obs_data <- stmf5 %>%
  filter(epi_year=="2019/2020") %>%
  select(country_code, epi_year, earlier_obs_deaths, later_obs_deaths)
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
    rstar[i,j] <- sample(as.numeric(v[i,2:11]), size=1, replace=T)
    
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

# I derive the empirical percentiles for expected deaths
tab.int.exp <- cbind(as.data.frame(obs_data[1]),
                     round(rowQuantiles(ExpD.star, probs=c(0.025,0.975))))

# I derive the empirical percentiles for excess deaths
tab.int.exc <- cbind(as.data.frame(obs_data[1]),
                     round(rowQuantiles(Excess.star, probs=c(0.025,0.975))))

# I derive the empirical percentiles for excess deaths
tab.int.risk.obs <- cbind(as.data.frame(obs_data[1]),
                          rowQuantiles(Risk.obs.star, probs=c(0.025,0.975)))

# I derive the empirical percentiles for excess deaths
tab.int.risk.exp <- cbind(as.data.frame(obs_data[1]),
                          rowQuantiles(Risk.exp.star, probs=c(0.025,0.975)))




###### TABLE 1 ######

# Excess death

stmf6 <- stmf2 %>%
  group_by(country_code) %>%
  summarize(later_obs_deaths = round(sum(later_obs_deaths)),
            later_exp_deaths = round(sum(later_exp_deaths)),
            excess_deaths = round(sum(excess_deaths))) %>%
  mutate(risk_obs = round((excess_deaths/later_obs_deaths)*100,1),
         risk_exp = round((excess_deaths/later_exp_deaths)*100,1))
stmf6

# Covid deaths

# Load file downloaded from ECDC for covid deaths
ecdc <- readxl::read_xlsx("2020-21-10-ecdc.xlsx")

# Obtain Covid19 death count by country
ecdc2 <- ecdc
# Convert into date
ecdc2$dateRep <- as.Date(ecdc2$dateRep, format="%Y-%m-%d")
# Covid deaths
ecdc2 <- ecdc2 %>%
  filter(countriesAndTerritories %in% c("France", "Spain"),
         dateRep >= as.Date("2020-02-10"), 
         dateRep <= as.Date("2020-06-29")) %>%
  group_by(countriesAndTerritories) %>%
  summarize(covid_deaths = sum(deaths)) %>%
  rename(country_code = countriesAndTerritories)
ecdc2

# Prediction Interval - quantiles

# Create the final table
colnames(tab.int.exp)[2:3] <- c("expected_low_int","expected_upp_int")
colnames(tab.int.exc)[2:3] <- c("excess_low_int","excess_upp_int")
colnames(tab.int.risk.exp)[2:3] <- c("risk_exp_low_int","risk_exp_upp_int")
colnames(tab.int.risk.obs)[2:3] <- c("risk_obs_low_int","risk_obs_upp_int")

stmf6 <- 
  left_join(stmf6, ecdc2, by="country_code") %>%
  left_join(., tab.int.exp, by="country_code") %>%
  left_join(., tab.int.exc, by="country_code") %>%
  left_join(., tab.int.risk.exp, by="country_code") %>%
  left_join(., tab.int.risk.obs, by="country_code")

tab_excess <- 
  reshape2::dcast(
    reshape2::melt(stmf6, 
                   id.vars=c("country_code")),
    variable~country_code)

setwd(dir.out)
write_xlsx(tab_excess,"Tab1_national.xlsx")




###### FIGURE 3 ######

###### Figure excess

stmf2[,4:6] <- round(stmf2[,4:6])
stmf2 <- as.data.frame(stmf2)
tab_fig2 <- stmf2 %>%
  pivot_longer(later_obs_deaths:excess_deaths, 
               names_to = "values", 
               values_to = "count") %>%
  filter(values %in% c("later_exp_deaths", "excess_deaths")) %>%
  group_by(country_code, sex, age_group) %>%
  mutate(label_y = cumsum(count)) 

tab_fig2$values <- as.factor(tab_fig2$values)
levels(tab_fig2$values) <- c("Excess deaths", "Expected deaths")


fig3 <- tab_fig2 %>% 
  ggplot(aes(x=age_group, y=count, fill=values)) + 
  geom_bar(stat="identity", width=0.9) +
  ylim(-11000,80000) + 
  facet_grid(rows = vars(country_code), 
             cols = vars(sex)) + 
  geom_text(aes(label=count, y=count+ifelse(count>=0,0, -10000)), 
            position = position_stack(vjust=0.5), 
            vjust=0, color="black", size=5) +
  labs(x = "Age group", y = "Counts", fill = "") +
  theme_bw() +
  theme(
    # Axis 
    axis.title.x = element_text(size = 20, face = "bold"),
    axis.title.y = element_text(size = 20, face = "bold"),
    axis.text.x = element_text(size = 17, angle = 45, vjust = 1, hjust = 1),
    axis.text.y = element_text(size = 17),
    # Legend
    legend.title = element_text(size = 20, face = "bold"),
    legend.position = "bottom",
    legend.text=element_text(size=17),
    # Facet
    strip.text.x = element_text(size = 17),
    strip.text.y = element_text(size = 17))

setwd(dir.out)
ggsave("Fig3_national.png", plot = fig3, width = 12 , height =8)




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
  stmf1 <- stmf %>%
    mutate(upsilon = later_obs_deaths/earlier_obs_deaths) %>%
    filter(epi_year %in% epi.hist) %>%
    group_by(sex, age_group, country_code) %>%
    mutate(av_upsilon = mean(upsilon[which(epi_year!=epi.pred)]),
           sd_upsilon = sd(upsilon[which(epi_year!=epi.pred)]),
           cv_upsilon = sd_upsilon/av_upsilon,
           lab = paste("mean =", round(av_upsilon,2), ",",
                       "sd =", round(sd_upsilon,2), ",",
                       "cv =", round(cv_upsilon,4)*100, "%")) %>%
    ungroup()

  # Compute expected dx and excess dx for the predicted year
  # by sex, age_group, country_code
  stmf2 <- stmf1 %>% 
    group_by(sex, age_group, country_code) %>%
    mutate(later_exp_deaths = 
             ifelse(epi_year==epi.pred, 
                    round(earlier_obs_deaths*av_upsilon),
                    ifelse(epi_year!=epi.pred, NA, 999))) %>%
    ungroup()  
  
  # Subset to later segment data for the predicted year
  stmf2 <- stmf2 %>%
    filter(epi_year == epi.pred) %>%
    distinct(sex, age_group, country_code, .keep_all = TRUE) %>%
    select(c(country_code, sex, age_group, 
             later_obs_deaths, later_exp_deaths)) 
  
  return(stmf2)
  
  }

###### 5-years-averages

# Create a function to apply the 5-years average method on the previous
# five years of data, by age group, sex and country 

method.av <- function(epi.hist, epi.pred){

  # Compute expected dx and excess dx for the predicted year
  # by sex, age_group, country_code
  stmf2.av <- stmf %>% 
    filter(epi_year %in% epi.hist) %>%
    group_by(sex, age_group, country_code) %>%
    mutate(later_exp_deaths = 
             ifelse(epi_year==epi.pred, 
                    mean(later_obs_deaths[which(epi_year!=epi.pred)]),
                    ifelse(epi_year!=epi.pred, NA, 999))) %>%
    ungroup()  
  
  # Subset to later segment data for the predicted year
  stmf2.av <- stmf2.av %>%
    filter(epi_year == epi.pred) %>%
    distinct(sex, age_group, country_code, .keep_all = TRUE) %>%
    select(c(country_code, sex, age_group, 
             later_obs_deaths, later_exp_deaths)) 
  
  return(stmf2.av)
  
  }

###### Apply the functions to the epiyear 2009/10-2018/2019

epi.hist=c("2009/2010","2010/2011","2011/2012","2012/2013","2013/2014","2014/2015")
epi.pred="2014/2015"
pred.ratio.1415 <- method.ratio(epi.hist, epi.pred)
pred.av.1415 <- method.av(epi.hist, epi.pred)

epi.hist=c("2010/2011","2011/2012","2012/2013","2013/2014","2014/2015","2015/2016")
epi.pred="2015/2016"
pred.ratio.1516 <- method.ratio(epi.hist, epi.pred)
pred.av.1516 <- method.av(epi.hist, epi.pred)

epi.hist=c("2011/2012","2012/2013","2013/2014","2014/2015","2015/2016","2016/2017")
epi.pred="2016/2017"
pred.ratio.1617 <- method.ratio(epi.hist, epi.pred)
pred.av.1617 <- method.av(epi.hist, epi.pred)

epi.hist=c("2012/2013","2013/2014","2014/2015","2015/2016","2016/2017","2017/2018")
epi.pred="2017/2018"
pred.ratio.1718 <- method.ratio(epi.hist, epi.pred)
pred.av.1718 <- method.av(epi.hist, epi.pred)

epi.hist=c("2013/2014","2014/2015","2015/2016","2016/2017","2017/2018","2018/2019")
epi.pred="2018/2019"
pred.ratio.1819 <- method.ratio(epi.hist, epi.pred)
pred.av.1819 <- method.av(epi.hist, epi.pred)

###### Create a final table with the observed and fitted deaths

names(pred.ratio.1415)[names(pred.ratio.1415) == "later_obs_deaths"] <- "observed.1415"
names(pred.ratio.1415)[names(pred.ratio.1415) == "later_exp_deaths"] <- "expected.ratio.1415"
names(pred.av.1415)[names(pred.av.1415) == "later_exp_deaths"] <- "expected.av.1415"

names(pred.ratio.1516)[names(pred.ratio.1516) == "later_obs_deaths"] <- "observed.1516"
names(pred.ratio.1516)[names(pred.ratio.1516) == "later_exp_deaths"] <- "expected.ratio.1516"
names(pred.av.1516)[names(pred.av.1516) == "later_exp_deaths"] <- "expected.av.1516"

names(pred.ratio.1617)[names(pred.ratio.1617) == "later_obs_deaths"] <- "observed.1617"
names(pred.ratio.1617)[names(pred.ratio.1617) == "later_exp_deaths"] <- "expected.ratio.1617"
names(pred.av.1617)[names(pred.av.1617) == "later_exp_deaths"] <- "expected.av.1617"

names(pred.ratio.1718)[names(pred.ratio.1718) == "later_obs_deaths"] <- "observed.1718"
names(pred.ratio.1718)[names(pred.ratio.1718) == "later_exp_deaths"] <- "expected.ratio.1718"
names(pred.av.1718)[names(pred.av.1718) == "later_exp_deaths"] <- "expected.av.1718"

names(pred.ratio.1819)[names(pred.ratio.1819) == "later_obs_deaths"] <- "observed.1819"
names(pred.ratio.1819)[names(pred.ratio.1819) == "later_exp_deaths"] <- "expected.ratio.1819"
names(pred.av.1819)[names(pred.av.1819) == "later_exp_deaths"] <- "expected.av.1819"

# Create the final table, for every year I save
# - the observed values, 
# - the fitted values with the later/earlier method, 
# - the fitted values with the 5-years-average method

pred <- cbind(pred.ratio.1415[1:5], pred.av.1415[5],
              pred.ratio.1516[4:5], pred.av.1516[5],
              pred.ratio.1617[4:5], pred.av.1617[5],
              pred.ratio.1718[4:5], pred.av.1718[5],
              pred.ratio.1819[4:5], pred.av.1819[5])

###### Compute the root mean squared error
rmse <- pred %>%
  mutate(rmse.ratio = 
           sqrt( (1/5)*( (expected.ratio.1415-observed.1415)^2 +
                           (expected.ratio.1516-observed.1516)^2 +
                           (expected.ratio.1617-observed.1617)^2 +
                           (expected.ratio.1718-observed.1718)^2 +
                           (expected.ratio.1819-observed.1819)^2 ) ),
         rmse.av =
           sqrt( (1/5)*( (expected.av.1415-observed.1415)^2 +
                           (expected.av.1516-observed.1516)^2 +
                           (expected.av.1617-observed.1617)^2 +
                           (expected.av.1718-observed.1718)^2 +
                           (expected.av.1819-observed.1819)^2 ) )) %>%
  select(country_code, sex, age_group, rmse.ratio, rmse.av)

#    country_code    sex age_group rmse.ratio    rmse.av
# 1        France Female    [0,15)   32.23422   45.23813
# 2         Spain Female    [0,15)   21.97689   37.38855
# 3        France Female   [15,65)  273.47631  343.05821
# 4         Spain Female   [15,65)   67.76821  114.58615
# 5        France Female   [65,75)  291.54804 1340.75162
# 6         Spain Female   [65,75)  230.43805  310.34740
# 7        France Female   [75,85) 1141.41851 1765.98968
# 8         Spain Female   [75,85)  745.62194 1696.04727
# 9        France Female       85+ 5222.08483 6087.38093
# 10        Spain Female       85+ 2358.84840 4425.17436
# 11       France   Male    [0,15)   24.14997   19.92183
# 12        Spain   Male    [0,15)   21.81097   48.57539
# 13       France   Male   [15,65)  371.86281 1527.04924
# 14        Spain   Male   [15,65)  288.48828  486.83781
# 15       France   Male   [65,75)  634.51056 2129.66161
# 16        Spain   Male   [65,75)  344.76005  612.47968
# 17       France   Male   [75,85) 1374.52768 1636.76857
# 18        Spain   Male   [75,85)  964.45014 1532.81403
# 19       France   Male       85+ 2521.01055 4002.32311
# 20        Spain   Male       85+ 1543.32994 3384.22825

###### Compute the mean absolute percentage error
mape <- pred %>%
  mutate(mape.ratio = 
           (100/7) * (abs((observed.1415-expected.ratio.1415)/observed.1415) +
                        abs((observed.1516-expected.ratio.1516)/observed.1415) +
                        abs((observed.1617-expected.ratio.1617)/observed.1617) +
                        abs((observed.1718-expected.ratio.1718)/observed.1718) +
                        abs((observed.1819-expected.ratio.1819)/observed.1819)),
         mape.av = 
           (100/7) * (abs((observed.1415-expected.av.1415)/observed.1415) +
                        abs((observed.1516-expected.av.1516)/observed.1415) +
                        abs((observed.1617-expected.av.1617)/observed.1617) +
                        abs((observed.1718-expected.av.1718)/observed.1718) +
                        abs((observed.1819-expected.av.1819)/observed.1819))) %>%
  select(country_code, sex, age_group, mape.ratio, mape.av)

mean(mape$mape.ratio)
# [1] 2.247696
mean(mape$mape.av)
# [1] 5.017901




###### TABLE 2 ######

rmse <- rmse %>%
  pivot_longer(rmse.ratio:rmse.av, 
               names_to = "method", 
               values_to = "rmse")
rmse$rmse <- round(rmse$rmse)

tab.rmse <- 
  reshape2::dcast(
    reshape2::melt(rmse, 
                   id.vars=c("age_group", "country_code", "sex",
                             "method")),
    age_group+variable~country_code+sex+method) %>%
  select(-"variable") %>%
  relocate(age_group, France_Female_rmse.ratio, France_Female_rmse.av,
           France_Male_rmse.ratio, France_Male_rmse.av,
           Spain_Female_rmse.ratio, Spain_Female_rmse.av,
           Spain_Male_rmse.ratio, Spain_Male_rmse.av)

setwd(dir.out)
write_xlsx(tab.rmse,"Tab2_national.xlsx")







