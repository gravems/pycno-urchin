#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#                                                                                ##
# Urchin-Pycno fear feeding trials                                               ##
# Data are current as of 2020-09-18                                              ##
# Data source: Ross Whippo - UO/OIMB                                             ##
# R code prepared by Ross Whippo                                                 ##
# Last updated 2020-09-18                                                        ##
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

# combine trial and bin to prevent combining urchins across trials
urchin_fear_pycno <- unite(urchin_fear_pycno, 'ID', trial, bin, sep = "_", remove = FALSE)

per_urchin_consumed_total <- urchin_fear_pycno %>%
  group_by(ID, pycno) %>%
  summarise(sum(consumed))

names(per_urchin_consumed_total)[names(per_urchin_consumed_total)=="sum(consumed)"] <- "total_consumed"


ggplot(per_urchin_consumed_total, aes(x = pycno, y = total_consumed, fill = pycno)) +
  geom_boxplot() +
  scale_fill_viridis(discrete = TRUE, begin = 0.2, end = 0.9) +
  theme_minimal()

# amount consumed per timepoint

urchin_timeseries <- urchin_fear_pycno %>%
  unite("datetime", date:time, sep = " ") 

urchin_timeseries$datetime <- as.POSIXlt(urchin_timeseries$datetime)

ggplot(urchin_timeseries, aes(x = as.POSIXct(datetime), y = consumed, fill = pycno)) +
  geom_col() +
  scale_fill_viridis(discrete = TRUE, begin = 0.2, end = 0.9) +
  facet_wrap(~pycno) 

# cumulative total consumed by time period (only works for one trial)

test <- urchin_timeseries %>%
  group_by(pycno, ID) %>%
  mutate(cc = cumsum(consumed))

# change unknown diameters to 61

urchin_timeseries$diameter <- urchin_timeseries$diameter %>%
  recode("nd" = "61")

ggplot(urchin_timeseries %>%
         group_by(pycno, ID) %>%
         mutate(cc = cumsum(consumed)),
       aes(x = timepoint, y = cc, color = factor(pycno))) + 
  scale_color_viridis(discrete = TRUE, begin = 0.3, end = 0.8) +
  geom_point(aes(size = as.numeric(urchin_timeseries$diameter)), alpha = 0.8) +
  geom_line(aes(group = ID), alpha = 0.25) +
  geom_smooth(method = "lm") +
  theme_minimal() +
  scale_y_continuous(name = "Cumulative Confetti Consumed")

# mean number of confetti consumed per time point per treatment

ggplot(urchin_timeseries, aes(x = pycno, y = consumed, fill = pycno)) +
  scale_fill_viridis(discrete = TRUE, begin = 0.3, end = 0.9) +
  geom_boxplot() +
  facet_grid(.~timepoint)
    theme_minimal() 



# total confetti eaten per treatment
urchin_fear_pycno %>%
  group_by(treatment) %>%
  summarise(sum(consumed))






############### SUBSECTION HERE

####
#<<<<<<<<<<<<<<<<<<<<<<<<<<END OF SCRIPT>>>>>>>>>>>>>>>>>>>>>>>>#

# SCRATCH PAD ####