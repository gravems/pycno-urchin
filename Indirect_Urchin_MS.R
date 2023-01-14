#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#                                                                                ##
# Indirect Urchin MS Figures                                                     ##
# Data are current as of 2023-01-13                                              ##
# Data source: Ross Whippo                                                       ##
# R code prepared by Ross Whippo                                                 ##
# Last updated 2023-01-13                                                        ##
#                                                                                ##
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# SUMMARY:
# Script to create figures for manuscript of purple urchin-pycnopodia indirect
# interactions. Experiments run in summer of 2020 and 2021 at the Friday Harbor
# laboratories by Ross Whippo, PhD candidate at University of Oregon. 

# Required Files (check that script is loading latest version):
# urchin_fear_feeding_pycno-trial-1.csv
# trials2021_QAQC.csv
# All files in Data>2021>Tracker Data


# Associated Scripts:
# trials2021_dataset_QAQC.R

# TO DO 

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
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

# 2023-01-13 Script Created

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# LOAD PACKAGES                                                                ####
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

library(tidyverse) # data manipulation, tidying
library(viridis) # color-blind-friendly palette

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# READ IN AND PREPARE DATA                                                     ####
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

trials2020_Q <- read_csv("Data/2020/trials2020_QAQC.csv", 
                         col_types = cols(trial = col_character(), 
                                          bin = col_character(), diameter = col_double()))

trials2021_Q <- read_csv("Data/2021/trials2021_QAQC.csv", 
                         col_types = cols(algalTreat = col_factor(levels = c("nereo", "control")), 
                                          date = col_character(),  location = col_character(), 
                                          movement = col_character(), 
                                          pycnoTreat = col_factor(levels = c("pycno", "control")), 
                                          tank = col_factor(levels = c("TF1", "TF2")), 
                                          timeBegin = col_character(), timeEnd = col_character(), 
                                          urchinGroup = col_factor(levels = c("starved","fed")),
                                          location = col_factor(levels = c("c", "m", "f"))))


#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# MANIPULATE DATA                                                              ####
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

############### 2020 urchin feeding

# combine trial and bin to prevent combining urchins across trials
urchin_fear_pycno <-
  unite(trials2020_Q,
        'ID',
        trial,
        bin,
        sep = "_",
        remove = FALSE)

per_urchin_consumed_total <- urchin_fear_pycno %>%
  group_by(trial, bin, tank, ID, pycno) %>%
  summarise(`total consumed` = sum(consumed)) %>%
  ungroup()

# total consumed per urchin across all trials with standard error
per_urchin_consumed_total %>%
  mutate(`total consumed` = `total consumed` * 0.339) %>%
  mutate(Treatment = ifelse(pycno == 'yes', "Pycno", "Control")) %>%
  ggplot(aes(x = Treatment, y = `total consumed`, color = Treatment)) + 
  geom_point(col = "grey", size = 3) +
  scale_color_viridis(discrete = TRUE,
                      begin = 0.3,
                      end = 0.7,
                      option = "magma") +
  stat_summary(
    geom = "point",
    fun = "mean",
    size = 6,
    shape = 19
  ) +
  geom_errorbar(stat="summary", fun.data="mean_se", size = 1) +
  ylim(0, 30) +
  labs(y = "Kelp consumed (g)", x = "Treatment") +
  theme_minimal(base_size = 15) +
  theme(axis.title.y = element_text(vjust = 3)) +
  theme(legend.position = "none") +
  annotate("text", label = "p = 0.015", x = 2.3, y = 29, size = 5)

# timeseries per timepoint figure

urchin_timeseries <- urchin_fear_pycno %>%
  unite("datetime", date:time, sep = " ")

urchin_timeseries$datetime <-
  as.POSIXlt(urchin_timeseries$datetime)

# with scaled time points
urchin_timeseries$Hours <- urchin_timeseries$timepoint %>%
  recode(
    '0' = 0,
    '1' = 3,
    '2' = 9,
    '3' = 21,
    '4' = 27,
    '5' = 33,
    '6' = 45,
    '7' = 51,
    '8' = 57,
    '9' = 69
  )

# THIS IS THE CUMULATIVE CONFETTI FIGURE:

ggplot(
  urchin_timeseries %>%
    group_by(pycno, ID) %>%
    mutate(cc = cumsum(consumed)),
  aes(
    x = Hours, # as.POSIXct(datetime),
    y = cc,
    color = pycno
  )
) +
  scale_color_viridis(discrete = TRUE,
                      begin = 0.5,
                      end = 0.9,
                      option = "magma") +
  geom_point(size = 5) +
  geom_line(aes(group = ID), alpha = 0.25) +
  geom_smooth(method = "lm", size = 2) +
  scale_y_continuous(name = "Pieces of Kelp Consumed") +
  theme_light()

############### 2021 urchin movement



####
#<<<<<<<<<<<<<<<<<<<<<<<<<<END OF SCRIPT>>>>>>>>>>>>>>>>>>>>>>>>#

# SCRATCH PAD ####