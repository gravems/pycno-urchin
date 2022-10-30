#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#                                                                                ##
# Urchin-Pycno tracker data analysis                                             ##
# Data are current as of 2022-05-24                                              ##
# Data source: Ross Whippo, Ethan Porter-Hughes - UO/OIMB                        ##
# R code prepared by Ross Whippo                                                 ##
# Last updated 2022-10-30                                                        ##
#                                                                                ##
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# SUMMARY:
# Script to analyse Tracker software coordinate data taken from gopro video of 
# sea urchin response to pycnopodia cue. Trials are 1 hour long. 


# Required Files (check that script is loading latest version):
# All files in Data>2021>Tracker Data

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

# 2022-10-30 Script created

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# LOAD PACKAGES                                                                ####
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

library(tidyverse)
library(viridis)
library(adehabitatLT)

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# READ IN AND PREPARE DATA                                                     ####
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

X001 <- read_csv("Data/2021/Tracker Data/001_PycAlg_S01_20210815.csv", 
                 skip = 1)
X002 <- read_csv("Data/2021/Tracker Data/002_ConAlg_S02_20210816.csv", 
                 skip = 1)
X003 <- read_csv("Data/2021/Tracker Data/003_PycCon_S03_20210816.csv", 
                 skip = 1)


#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# MANIPULATE DATA - VISUALIZATIONS                                             ####
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

ggplot(X001, aes(x = x, y = y)) +
  geom_path(lineend = "butt", linejoin = "round", linemitre = 1) +
  theme_minimal() +
  coord_cartesian(xlim = c(-60, 60), ylim = c(-60, 60))

ggplot(X002, aes(x = x, y = y)) +
  geom_path(lineend = "butt", linejoin = "round", linemitre = 1) +
  theme_minimal() +
  coord_cartesian(xlim = c(-60, 60), ylim = c(-60, 60))

ggplot(X003, aes(x = x, y = y)) +
  geom_path(lineend = "butt", linejoin = "round", linemitre = 1) +
  theme_minimal() +
  coord_cartesian(xlim = c(-60, 60), ylim = c(-60, 60))
  
  
  ############### 
  
  ####
  #<<<<<<<<<<<<<<<<<<<<<<<<<<END OF SCRIPT>>>>>>>>>>>>>>>>>>>>>>>>#

# SCRATCH PAD ####

         