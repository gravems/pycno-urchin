#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#                                                                                ##
# Urchin-Pycno fear feeding trials                                               ##
# Data are current as of 2020-09-15                                              ##
# Data source: Ross Whippo - UO/OIMB                                             ##
# R code prepared by Ross Whippo                                                 ##
# Last updated 2020-09-15                                                        ##
#                                                                                ##
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# SUMMARY:


# Required Files (check that script is loading latest version):
# urchin_fear_feeding_pycno-trial-1.csv

# Associated Scripts:
# NA

# TO DO 

# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# TABLE OF CONTENTS                                                            ####
#                                                                                 +
# RECENT CHANGES TO SCRIPT                                                        +
# LOAD PACKAGES                                                                   +
# READ IN AND PREPARE DATA                                                        +
# MANIPULATE DATA                                                                 +
#                                                                                 +
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# RECENT CHANGES TO SCRIPT                                                     ####
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# 2020-09-15 Script created

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# LOAD PACKAGES                                                                ####
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

library(tidyverse)
library(viridis)

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# READ IN AND PREPARE DATA                                                     ####
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

urchin_fear_pycno <- read_csv('Data/urchin_fear_feeding_pycno-trial-1.csv')

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# MANIPULATE DATA - VISUALIZATIONS                                             ####
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# mean confetti consumed across all urchins per treatment
per_urchin_consumed_total <- urchin_fear_pycno %>%
  group_by(bin, treatment) %>%
  summarise(sum(consumed))

names(per_urchin_consumed_total)[names(per_urchin_consumed_total)=="sum(consumed)"] <- "total_consumed"

ggplot(per_urchin_consumed_total, aes(x = treatment, y = total_consumed, fill = treatment)) +
  geom_boxplot() +
  scale_fill_viridis(discrete = TRUE, begin = 0.2, end = 0.9) +
  theme_minimal()

# amount consumed per time

urchin_timeseries <- urchin_fear_pycno %>%
  unite("datetime", date:time, sep = " ") 

urchin_timeseries$datetime <- as.POSIXlt(urchin_timeseries$datetime)
ggplot(urchin_timeseries, aes(x = as.POSIXct(datetime), y = consumed, fill = treatment)) +
  geom_col() +
  scale_fill_viridis(discrete = TRUE, begin = 0.2, end = 0.9) +
  facet_wrap(~treatment) 

# cumulative total consumed by time period
ggplot(urchin_timeseries %>%
         group_by(treatment) %>%
         mutate(cc = cumsum(consumed)),
       aes(x = as.POSIXct(datetime), y = cc, color = factor(treatment))) + 
  scale_color_viridis(discrete = TRUE, begin = 0.3, end = 0.9) +
  geom_line(size = 1) +
  geom_point() +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = -45))

# cumulative total consumed by time period per urchin
ggplot(urchin_timeseries %>%
         group_by(bin) %>%
         mutate(cc = cumsum(consumed)),
       aes(x = as.POSIXct(datetime), y = cc, color = treatment)) + 
  scale_color_viridis(discrete = TRUE, begin = 0.3, end = 0.9) +
  geom_point() +
  theme_minimal() +
  geom_smooth(method = lm) +
  scale_x_datetime(name = "Date") +
  scale_y_continuous(name = "Cumulative Confetti Consumed") +
  theme(axis.text.x = element_text(angle = -45)) 
  
  # total confetti eaten per treatment
urchin_fear_pycno %>%
  group_by(treatment) %>%
  summarise(sum(consumed))






############### SUBSECTION HERE

####
#<<<<<<<<<<<<<<<<<<<<<<<<<<END OF SCRIPT>>>>>>>>>>>>>>>>>>>>>>>>#

# SCRATCH PAD ####