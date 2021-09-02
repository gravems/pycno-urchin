#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#                                                                                ##
# Pycno-Urchin Dataset 2021 Model                                                ##
# Script created 2021-09-01                                                      ##
# Last updated 2021-09-01                                                        ##
# Data source: Ross Whippo                                                       ##
# R code prepared by Ross Whippo                                                 ##
#                                                                                ##
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# SUMMARY:
# A script to build a statistical model for 2021 pycno-urchin dataset.


# Required Files (check that script is loading latest version):
# trials2021_QAQC.csv

# Associated Scripts:
# trials2021_dataset_QAQC.R
# trials2021_figExplore.R

# TO DO 

# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# TABLE OF CONTENTS                                                            ####
#                                                                                 +
# RECENT CHANGES TO SCRIPT                                                        +
# LOAD PACKAGES                                                                   +
# READ IN AND PREPARE DATA                                                        +
# MODEL 1                                                                           +
#                                                                                 +
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# RECENT CHANGES TO SCRIPT                                                     ####
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# 2021-09-01 Script created

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# LOAD PACKAGES                                                                ####
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

library(tidyverse)
library(viridis)
library(lme4)

# set plot theme
theme_set(theme_classic())

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# READ IN DATA                                                                 ####
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

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

model1_dat <- trials2021_Q

# create proportion columns for each behavior

model1_dat <- model1_dat %>%
  select(trial, tank, `signalRate_ml-min`, beginTemp_C, endTemp_C, beginSal_ppt, endSal_ppt, urchinGroup, pycnoTreat, algalTreat, urchinDiam_mm, movement, location, spines, interaction) %>%
  filter(movement != "st") %>%
  add_column(move = 1) %>%
  group_by(trial, urchinGroup, pycnoTreat, algalTreat, move) %>%
  count(move) %>%
  mutate(time_moving = n/60)


############### SUBSECTION HERE

####
#<<<<<<<<<<<<<<<<<<<<<<<<<<END OF SCRIPT>>>>>>>>>>>>>>>>>>>>>>>>#

# SCRATCH PAD ####