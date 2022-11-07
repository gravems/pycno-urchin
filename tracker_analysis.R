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
X001$id <- "PycAlg_S01"

X002 <- read_csv("Data/2021/Tracker Data/002_ConAlg_S02_20210816.csv", 
                 skip = 1)
X002$L <- NA
X002$id <- "ConAlg_S02"

X003 <- read_csv("Data/2021/Tracker Data/003_PycCon_S03_20210816.csv", 
                 skip = 1)
X003$id <- "PycCon_S03"

X004 <- read_csv("Data/2021/Tracker Data/004__ConCon_F01_20210816.csv", 
                 skip = 1)
X004$id <- "ConCon_F01"

X005 <- read_csv("Data/2021/Tracker Data/005_PycAlg_F02_20210817.csv", 
                 skip = 1)
X006 <- read_csv("Data/2021/Tracker Data/006_PycAlg_S04_20210816.csv", 
                 skip = 1)
X007 <- read_csv("Data/2021/Tracker Data/007_ConAlg_F03_20210817.csv", 
                 skip = 1)
X008 <- read_csv("Data/2021/Tracker Data/008_ConAlg_S05_20210817.csv", 
                 skip = 1)
X009 <- read_csv("Data/2021/Tracker Data/009_ConCon_F04_20210818.csv", 
                 skip = 1)
X010 <- read_csv("Data/2021/Tracker Data/010_ConCon_F05_20210818.csv", 
                 skip = 1)
X011 <- read_csv("Data/2021/Tracker Data/011_PycAlg_S06_20210818.csv", 
                 skip = 1)
X012 <- read_csv("Data/2021/Tracker Data/012_ConCon_S07_20210818.csv", 
                 skip = 1)
X013 <- read_csv("Data/2021/Tracker Data/013_ConCon_S08_20210819.csv", 
                 skip = 1)
X014 <- read_csv("Data/2021/Tracker Data/014_ConAlg_F06_20210819.csv", 
                 skip = 1)
X015 <- read_csv("Data/2021/Tracker Data/015_ConAlg_F07_20210819.csv", 
                 skip = 1)
X016 <- read_csv("Data/2021/Tracker Data/016_PycCon_F08_20210819.csv", 
                 skip = 1)
X017 <- read_csv("Data/2021/Tracker Data/017_ConAlg_S09_20210820.csv", 
                 skip = 1)
X018 <- read_csv("Data/2021/Tracker Data/018_PycCon_F09_20210820.csv", 
                 skip = 1)
X019 <- read_csv("Data/2021/Tracker Data/019_PycAlg_F10_20210820.csv", 
                 skip = 1)
X020 <- read_csv("Data/2021/Tracker Data/020_PycAlg_F11_20210820.csv", 
                 skip = 1)
X021 <- read_csv("Data/2021/Tracker Data/021_PycCon_F12_20210821.csv", 
                 skip = 1)
X022 <- read_csv("Data/2021/Tracker Data/022_PycCon_S10_20210821.csv", 
                 skip = 1)
X023 <- read_csv("Data/2021/Tracker Data/023_ConCon_S11_20210821.csv", 
                 skip = 1)
X024 <- read_csv("Data/2021/Tracker Data/024_PycCon_S12_20210821.csv", 
                 skip = 1)
X025 <- read_csv("Data/2021/Tracker Data/025_PycAlg_S13_20210822.csv", 
                 skip = 1)
X026 <- read_csv("Data/2021/Tracker Data/026_ConAlg_F13_20210822.csv", 
                 skip = 1)
X027 <- read_csv("Data/2021/Tracker Data/027_PycAlg_S14_20210823.csv", 
                 skip = 1)
X028 <- read_csv("Data/2021/Tracker Data/028_ConAlg_S14_20210823.csv", 
                 skip = 1)
X029 <- read_csv("Data/2021/Tracker Data/029_ConAlg_S16_20210824.csv", 
                 skip = 1)
X030 <- read_csv("Data/2021/Tracker Data/030_ConAlg_F14_20210824.csv", 
                 skip = 1)
X031 <- read_csv("Data/2021/Tracker Data/031_ConCon_S17_20210825.csv", 
                 skip = 1)
X032 <- read_csv("Data/2021/Tracker Data/032_ConCon_F15_20210825.csv", 
                 skip = 1)
X033 <- read_csv("Data/2021/Tracker Data/033_PycCon_F16_20210826.csv", 
                 skip = 1)
X034 <- read_csv("Data/2021/Tracker Data/034_ConCon_F17_20210826.csv", 
                 skip = 1)
X035 <- read_csv("Data/2021/Tracker Data/035_PycAlg_S18_20210827.csv", 
                 skip = 1)
X036 <- read_csv("Data/2021/Tracker Data/036_PycCon_F18_20210827.csv", 
                 skip = 1)
X037 <- read_csv("Data/2021/Tracker Data/037_PycCon_F19_20210828.csv", 
                 skip = 1)
X038 <- read_csv("Data/2021/Tracker Data/038_ConCon_F20_20210828.csv", 
                 skip = 1)
X039 <- read_csv("Data/2021/Tracker Data/039_ConCon_F20_20210828.csv", 
                 skip = 1)
X040 <- read_csv("Data/2021/Tracker Data/040_ConAlg_S20_20210828.csv", 
                 skip = 1)
X041 <- read_csv("Data/2021/Tracker Data/041_ConCon_S21_20210829.csv", 
                 skip = 1)
X042 <- read_csv("Data/2021/Tracker Data/042_PycCon_S22_20210829.csv", 
                 skip = 1)
X043 <- read_csv("Data/2021/Tracker Data/043_PycAlg_F21_20210829.csv", 
                 skip = 1)
X044 <- read_csv("Data/2021/Tracker Data/044_PycAlg_F22_20210829.csv", 
                 skip = 1)
X045 <- read_csv("Data/2021/Tracker Data/045_PycAlg_F23_20210830.csv", 
                 skip = 1)
X046 <- read_csv("Data/2021/Tracker Data/046_PycCon_S23_20210830.csv", 
                 skip = 1)
X047 <- read_csv("Data/2021/Tracker Data/047_PycCon_S24_20210830.csv", 
                 skip = 1)
X048 <- read_csv("Data/2021/Tracker Data/048_ConAlg_F24_20210830.csv", 
                 skip = 1)
X049 <- read_csv("Data/2021/Tracker Data/049_ConAlg_S25_20210904.csv", 
                 skip = 1)

All_move <- bind_rows(X001, X002, X003, X004)

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# MANIPULATE DATA - CARTESIAN PLOT OF MOVEMENT                                 ####
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
  
ggplot(X004, aes(x = x, y = y)) +
  geom_path(lineend = "butt", linejoin = "round", linemitre = 1) +
  theme_minimal() +
  coord_cartesian(xlim = c(-60, 60), ylim = c(-60, 60))

ggplot(X005, aes(x = x, y = y)) +
  geom_path(lineend = "butt", linejoin = "round", linemitre = 1) +
  theme_minimal() +
  coord_cartesian(xlim = c(-60, 60), ylim = c(-60, 60))

ggplot(X006, aes(x = x, y = y)) +
  geom_path(lineend = "butt", linejoin = "round", linemitre = 1) +
  theme_minimal() +
  coord_cartesian(xlim = c(-60, 60), ylim = c(-60, 60))

ggplot(X007, aes(x = x, y = y)) +
  geom_path(lineend = "butt", linejoin = "round", linemitre = 1) +
  theme_minimal() +
  coord_cartesian(xlim = c(-60, 60), ylim = c(-60, 60))

ggplot(X008, aes(x = x, y = y)) +
  geom_path(lineend = "butt", linejoin = "round", linemitre = 1) +
  theme_minimal() +
  coord_cartesian(xlim = c(-60, 60), ylim = c(-60, 60))

ggplot(X009, aes(x = x, y = y)) +
  geom_path(lineend = "butt", linejoin = "round", linemitre = 1) +
  theme_minimal() +
  coord_cartesian(xlim = c(-60, 60), ylim = c(-60, 60))

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# MANIPULATE DATA - create adehabitatLT objects                                ####
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

M001 <- as.ltraj(xy = All_move[, c('x','y')], typeII = FALSE, id = All_move$id)
plot(M001)

  ############### 
  
  ####
  #<<<<<<<<<<<<<<<<<<<<<<<<<<END OF SCRIPT>>>>>>>>>>>>>>>>>>>>>>>>#

# SCRATCH PAD ####

         