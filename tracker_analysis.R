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

##
X001 <- read_csv("Data/2021/Tracker Data/001_PycAlg_S01_20210815.csv", 
                col_types = cols(x = col_number(), y = col_number(),          X1 = col_number()), skip = 1)
X001$id <- "PycAlg_S01"
X001$distance <- NA
X001$distance[2:nrow(X001)] <- sqrt((X001$x[2:nrow(X001)] - X001$x[1:nrow(X001)-1]) ^ 2 + 
                                        (X001$y[2:nrow(X001)] - X001$y[1:nrow(X001)-1]) ^ 2)
time <- X001[nrow(X001), 1]/60
distance <- X001 %>%
  summarise(distance = sum(distance, na.rm = TRUE))
Alg <- "Algae"
Pyc <- "Pycno"
Urchin <- "Starved"
ID <- "S01"
Tracker_summary <- tibble(time, distance, Alg, Pyc, Urchin, ID)

##
X002 <- read_csv("Data/2021/Tracker Data/002_ConAlg_S02_20210816.csv", 
                col_types = cols(x = col_number(), y = col_number(),          X1 = col_number()), skip = 1)
X002$L <- NA
X002$id <- "ConAlg_S02"
X002$distance <- NA
X002$distance[2:nrow(X002)] <- sqrt((X002$x[2:nrow(X002)] - X002$x[1:nrow(X002)-1]) ^ 2 + 
                                      (X002$y[2:nrow(X002)] - X002$y[1:nrow(X002)-1]) ^ 2)
time <- X002[nrow(X002), 1]/60
distance <- X002 %>%
  summarise(distance = sum(distance, na.rm = TRUE))
Alg <- "Algae"
Pyc <- "No Pycno"
Urchin <- "Starved"
ID <- "S02"
summary <- tibble(time, distance, Alg, Pyc, Urchin, ID)
Tracker_summary <- bind_rows(Tracker_summary, summary)

##
X003 <- read_csv("Data/2021/Tracker Data/003_PycCon_S03_20210816.csv", 
                col_types = cols(x = col_number(), y = col_number(),          X1 = col_number()), skip = 1)
X003$id <- "PycCon_S03"
X003$distance <- NA
X003$distance[2:nrow(X003)] <- sqrt((X003$x[2:nrow(X003)] - X003$x[1:nrow(X003)-1]) ^ 2 + 
                                      (X003$y[2:nrow(X003)] - X003$y[1:nrow(X003)-1]) ^ 2)
time <- X003[nrow(X003), 1]/60
distance <- X003 %>%
  summarise(distance = sum(distance, na.rm = TRUE))
Alg <- "Algae"
Pyc <- "No Pycno"
Urchin <- "Starved"
ID <- "S03"
summary <- tibble(time, distance, Alg, Pyc, Urchin, ID)
Tracker_summary <- bind_rows(Tracker_summary, summary)

##
X004 <- read_csv("Data/2021/Tracker Data/004__ConCon_F01_20210816.csv", 
                col_types = cols(x = col_number(), y = col_number(),          X1 = col_number()), skip = 1)
X004$id <- "ConCon_F01"
X004$distance <- NA
X004$distance[2:nrow(X004)] <- sqrt((X004$x[2:nrow(X004)] - X004$x[1:nrow(X004)-1]) ^ 2 + 
                                      (X004$y[2:nrow(X004)] - X004$y[1:nrow(X004)-1]) ^ 2)
time <- X004[nrow(X004), 1]/60
distance <- X004 %>%
  summarise(distance = sum(distance, na.rm = TRUE))
Alg <- "No Algae"
Pyc <- "No Pycno"
Urchin <- "Fed"
ID <- "F01"
summary <- tibble(time, distance, Alg, Pyc, Urchin, ID)
Tracker_summary <- bind_rows(Tracker_summary, summary)

##
X005 <- read_csv("Data/2021/Tracker Data/005_PycAlg_F02_20210817.csv", 
                col_types = cols(x = col_number(), y = col_number(),          X1 = col_number()), skip = 1)
X005$distance <- NA
X005$distance[2:nrow(X005)] <- sqrt((X005$x[2:nrow(X005)] - X005$x[1:nrow(X005)-1]) ^ 2 + 
                                      (X005$y[2:nrow(X005)] - X005$y[1:nrow(X005)-1]) ^ 2)
time <- X005[nrow(X005), 1]/60
distance <- X005 %>%
  summarise(distance = sum(distance, na.rm = TRUE))
Alg <- "Algae"
Pyc <- "Pycno"
Urchin <- "Fed"
ID <- "F02"
summary <- tibble(time, distance, Alg, Pyc, Urchin, ID)
Tracker_summary <- bind_rows(Tracker_summary, summary)

##
X006 <- read_csv("Data/2021/Tracker Data/006_PycAlg_S04_20210816.csv", 
                col_types = cols(x = col_number(), y = col_number(),          X1 = col_number()), skip = 1)
X006$distance <- NA
X006$distance[2:nrow(X006)] <- sqrt((X006$x[2:nrow(X006)] - X006$x[1:nrow(X006)-1]) ^ 2 + 
                                      (X006$y[2:nrow(X006)] - X006$y[1:nrow(X006)-1]) ^ 2)
time <- X006[nrow(X006), 1]/60
distance <- X006 %>%
  summarise(distance = sum(distance, na.rm = TRUE))
Alg <- "Algae"
Pyc <- "Pycno"
Urchin <- "Starved"
ID <- "S04"
summary <- tibble(time, distance, Alg, Pyc, Urchin, ID)
Tracker_summary <- bind_rows(Tracker_summary, summary)

##
X007 <- read_csv("Data/2021/Tracker Data/007_ConAlg_F03_20210817.csv", 
                col_types = cols(x = col_number(), y = col_number(),          X1 = col_number()), skip = 1)
X007$distance <- NA
X007$distance[2:nrow(X007)] <- sqrt((X007$x[2:nrow(X007)] - X007$x[1:nrow(X007)-1]) ^ 2 + 
                                      (X007$y[2:nrow(X007)] - X007$y[1:nrow(X007)-1]) ^ 2)
time <- X007[nrow(X007), 1]/60
distance <- X007 %>%
  summarise(distance = sum(distance, na.rm = TRUE))
Alg <- "Algae"
Pyc <- "No Pycno"
Urchin <- "Fed"
ID <- "F03"
summary <- tibble(time, distance, Alg, Pyc, Urchin, ID)
Tracker_summary <- bind_rows(Tracker_summary, summary)

##
X008 <- read_csv("Data/2021/Tracker Data/008_ConAlg_S05_20210817.csv", 
                col_types = cols(x = col_number(), y = col_number(),          X1 = col_number()), skip = 1)
X008$distance <- NA
X008$distance[2:nrow(X008)] <- sqrt((X008$x[2:nrow(X008)] - X008$x[1:nrow(X008)-1]) ^ 2 + 
                                      (X008$y[2:nrow(X008)] - X008$y[1:nrow(X008)-1]) ^ 2)
time <- X008[nrow(X008), 1]/60
distance <- X008 %>%
  summarise(distance = sum(distance, na.rm = TRUE))
Alg <- "Algae"
Pyc <- "No Pycno"
Urchin <- "Starved"
ID <- "S05"
summary <- tibble(time, distance, Alg, Pyc, Urchin, ID)
Tracker_summary <- bind_rows(Tracker_summary, summary)

##
X009 <- read_csv("Data/2021/Tracker Data/009_ConCon_F04_20210818.csv", 
                col_types = cols(x = col_number(), y = col_number(),          X1 = col_number()), skip = 1)
X009$distance <- NA
X009$distance[2:nrow(X009)] <- sqrt((X009$x[2:nrow(X009)] - X009$x[1:nrow(X009)-1]) ^ 2 + 
                                      (X009$y[2:nrow(X009)] - X009$y[1:nrow(X009)-1]) ^ 2)
time <- X009[nrow(X009), 1]/60
distance <- X009 %>%
  summarise(distance = sum(distance, na.rm = TRUE))
Alg <- "No Algae"
Pyc <- "No Pycno"
Urchin <- "Fed"
ID <- "F04"
summary <- tibble(time, distance, Alg, Pyc, Urchin, ID)
Tracker_summary <- bind_rows(Tracker_summary, summary)

##
X010 <- read_csv("Data/2021/Tracker Data/010_ConCon_F05_20210818.csv", 
                col_types = cols(x = col_number(), y = col_number(),          X1 = col_number()), skip = 1)
X010$distance <- NA
X010$distance[2:nrow(X010)] <- sqrt((X010$x[2:nrow(X010)] - X010$x[1:nrow(X010)-1]) ^ 2 + 
                                      (X010$y[2:nrow(X010)] - X010$y[1:nrow(X010)-1]) ^ 2)
time <- X010[nrow(X010), 1]/60
distance <- X010 %>%
  summarise(distance = sum(distance, na.rm = TRUE))
Alg <- "No Algae"
Pyc <- "No Pycno"
Urchin <- "Fed"
ID <- "F05"
summary <- tibble(time, distance, Alg, Pyc, Urchin, ID)
Tracker_summary <- bind_rows(Tracker_summary, summary)

##
X011 <- read_csv("Data/2021/Tracker Data/011_PycAlg_S06_20210818.csv", 
                col_types = cols(x = col_number(), y = col_number(),          X1 = col_number()), skip = 1)
X011$distance <- NA
X011$distance[2:nrow(X011)] <- sqrt((X011$x[2:nrow(X011)] - X011$x[1:nrow(X011)-1]) ^ 2 + 
                                      (X011$y[2:nrow(X011)] - X011$y[1:nrow(X011)-1]) ^ 2)
time <- X011[nrow(X011), 1]/60
distance <- X011 %>%
  summarise(distance = sum(distance, na.rm = TRUE))
Alg <- "Algae"
Pyc <- "Pycno"
Urchin <- "Starved"
ID <- "S06"
summary <- tibble(time, distance, Alg, Pyc, Urchin, ID)
Tracker_summary <- bind_rows(Tracker_summary, summary)

##
X012 <- read_csv("Data/2021/Tracker Data/012_ConCon_S07_20210818.csv", 
                col_types = cols(x = col_number(), y = col_number(),          X1 = col_number()), skip = 1)
X012$distance <- NA
X012$distance[2:nrow(X012)] <- sqrt((X012$x[2:nrow(X012)] - X012$x[1:nrow(X012)-1]) ^ 2 + 
                                      (X012$y[2:nrow(X012)] - X012$y[1:nrow(X012)-1]) ^ 2)
time <- X012[nrow(X012), 1]/60
distance <- X012 %>%
  summarise(distance = sum(distance, na.rm = TRUE))
Alg <- "No Algae"
Pyc <- "No Pycno"
Urchin <- "Starved"
ID <- "S07"
summary <- tibble(time, distance, Alg, Pyc, Urchin, ID)
Tracker_summary <- bind_rows(Tracker_summary, summary)

##
X013 <- read_csv("Data/2021/Tracker Data/013_ConCon_S08_20210819.csv", 
                col_types = cols(x = col_number(), y = col_number(),          X1 = col_number()), skip = 1)
X013$distance <- NA
X013$distance[2:nrow(X013)] <- sqrt((X013$x[2:nrow(X013)] - X013$x[1:nrow(X013)-1]) ^ 2 + 
                                      (X013$y[2:nrow(X013)] - X013$y[1:nrow(X013)-1]) ^ 2)
time <- X013[nrow(X013), 1]/60
distance <- X013 %>%
  summarise(distance = sum(distance, na.rm = TRUE))
Alg <- "No Algae"
Pyc <- "No Pycno"
Urchin <- "Starved"
ID <- "S08"
summary <- tibble(time, distance, Alg, Pyc, Urchin, ID)
Tracker_summary <- bind_rows(Tracker_summary, summary)

##
X014 <- read_csv("Data/2021/Tracker Data/014_ConAlg_F06_20210819.csv", 
                col_types = cols(x = col_number(), y = col_number(),          X1 = col_number()), skip = 1)
X014$distance <- NA
X014$distance[2:nrow(X014)] <- sqrt((X014$x[2:nrow(X014)] - X014$x[1:nrow(X014)-1]) ^ 2 + 
                                      (X014$y[2:nrow(X014)] - X014$y[1:nrow(X014)-1]) ^ 2)
time <- X014[nrow(X014), 1]/60
distance <- X014 %>%
  summarise(distance = sum(distance, na.rm = TRUE))
Alg <- "Algae"
Pyc <- "No Pycno"
Urchin <- "Fed"
ID <- "F06"
summary <- tibble(time, distance, Alg, Pyc, Urchin, ID)
Tracker_summary <- bind_rows(Tracker_summary, summary)

##
X015 <- read_csv("Data/2021/Tracker Data/015_ConAlg_F07_20210819.csv", 
                col_types = cols(x = col_number(), y = col_number(),          X1 = col_number()), skip = 1)
X015$distance <- NA
X015$distance[2:nrow(X015)] <- sqrt((X015$x[2:nrow(X015)] - X015$x[1:nrow(X015)-1]) ^ 2 + 
                                      (X015$y[2:nrow(X015)] - X015$y[1:nrow(X015)-1]) ^ 2)
time <- X015[nrow(X015), 1]/60
distance <- X015 %>%
  summarise(distance = sum(distance, na.rm = TRUE))
Alg <- "Algae"
Pyc <- "No Pycno"
Urchin <- "Fed"
ID <- "F07"
summary <- tibble(time, distance, Alg, Pyc, Urchin, ID)
Tracker_summary <- bind_rows(Tracker_summary, summary)

##
X016 <- read_csv("Data/2021/Tracker Data/016_PycCon_F08_20210819.csv", 
                col_types = cols(x = col_number(), y = col_number(),          X1 = col_number()), skip = 1)
X016$distance <- NA
X016$distance[2:nrow(X016)] <- sqrt((X016$x[2:nrow(X016)] - X016$x[1:nrow(X016)-1]) ^ 2 + 
                                      (X016$y[2:nrow(X016)] - X016$y[1:nrow(X016)-1]) ^ 2)
time <- X016[nrow(X016), 1]/60
distance <- X016 %>%
  summarise(distance = sum(distance, na.rm = TRUE))
Alg <- "No Algae"
Pyc <- "Pycno"
Urchin <- "Fed"
ID <- "F08"
summary <- tibble(time, distance, Alg, Pyc, Urchin, ID)
Tracker_summary <- bind_rows(Tracker_summary, summary)

##
X017 <- read_csv("Data/2021/Tracker Data/017_ConAlg_S09_20210820.csv", 
                col_types = cols(x = col_number(), y = col_number(),          X1 = col_number()), skip = 1)
X017$distance <- NA
X017$distance[2:nrow(X017)] <- sqrt((X017$x[2:nrow(X017)] - X017$x[1:nrow(X017)-1]) ^ 2 + 
                                      (X017$y[2:nrow(X017)] - X017$y[1:nrow(X017)-1]) ^ 2)
time <- X017[nrow(X017), 1]/60
distance <- X017 %>%
  summarise(distance = sum(distance, na.rm = TRUE))
Alg <- "Algae"
Pyc <- "No Pycno"
Urchin <- "Starved"
ID <- "S09"
summary <- tibble(time, distance, Alg, Pyc, Urchin, ID)
Tracker_summary <- bind_rows(Tracker_summary, summary)

##
X018 <- read_csv("Data/2021/Tracker Data/018_PycCon_F09_20210820.csv", 
                col_types = cols(x = col_number(), y = col_number(),          X1 = col_number()), skip = 1)
X018$distance <- NA
X018$distance[2:nrow(X018)] <- sqrt((X018$x[2:nrow(X018)] - X018$x[1:nrow(X018)-1]) ^ 2 + 
                                      (X018$y[2:nrow(X018)] - X018$y[1:nrow(X018)-1]) ^ 2)
time <- X018[nrow(X018), 1]/60
distance <- X018 %>%
  summarise(distance = sum(distance, na.rm = TRUE))
Alg <- "No Algae"
Pyc <- "Pycno"
Urchin <- "Fed"
ID <- "F09"
summary <- tibble(time, distance, Alg, Pyc, Urchin, ID)
Tracker_summary <- bind_rows(Tracker_summary, summary)

##
X019 <- read_csv("Data/2021/Tracker Data/019_PycAlg_F10_20210820.csv", 
                col_types = cols(x = col_number(), y = col_number(),          X1 = col_number()), skip = 1)
X019$distance <- NA
X019$distance[2:nrow(X019)] <- sqrt((X019$x[2:nrow(X019)] - X019$x[1:nrow(X019)-1]) ^ 2 + 
                                      (X019$y[2:nrow(X019)] - X019$y[1:nrow(X019)-1]) ^ 2)
time <- X019[nrow(X019), 1]/60
distance <- X019 %>%
  summarise(distance = sum(distance, na.rm = TRUE))
Alg <- "Algae"
Pyc <- "Pycno"
Urchin <- "Fed"
ID <- "F10"
summary <- tibble(time, distance, Alg, Pyc, Urchin, ID)
Tracker_summary <- bind_rows(Tracker_summary, summary)

##
X020 <- read_csv("Data/2021/Tracker Data/020_PycAlg_F11_20210820.csv", 
                col_types = cols(x = col_number(), y = col_number(),          X1 = col_number()), skip = 1)
X020$distance <- NA
X020$distance[2:nrow(X020)] <- sqrt((X020$x[2:nrow(X020)] - X020$x[1:nrow(X020)-1]) ^ 2 + 
                                      (X020$y[2:nrow(X020)] - X020$y[1:nrow(X020)-1]) ^ 2)
time <- X020[nrow(X020), 1]/60
distance <- X020 %>%
  summarise(distance = sum(distance, na.rm = TRUE))
Alg <- "Algae"
Pyc <- "Pycno"
Urchin <- "Fed"
ID <- "F11"
summary <- tibble(time, distance, Alg, Pyc, Urchin, ID)
Tracker_summary <- bind_rows(Tracker_summary, summary)

##
X021 <- read_csv("Data/2021/Tracker Data/021_PycCon_F12_20210821.csv", 
                col_types = cols(x = col_number(), y = col_number(),          X1 = col_number()), skip = 1)
X021$distance <- NA
X021$distance[2:nrow(X021)] <- sqrt((X021$x[2:nrow(X021)] - X021$x[1:nrow(X021)-1]) ^ 2 + 
                                      (X021$y[2:nrow(X021)] - X021$y[1:nrow(X021)-1]) ^ 2)
time <- X021[nrow(X021), 1]/60
distance <- X021 %>%
  summarise(distance = sum(distance, na.rm = TRUE))
Alg <- "No Algae"
Pyc <- "Pycno"
Urchin <- "Fed"
ID <- "F12"
summary <- tibble(time, distance, Alg, Pyc, Urchin, ID)
Tracker_summary <- bind_rows(Tracker_summary, summary)

##
X022 <- read_csv("Data/2021/Tracker Data/022_PycCon_S10_20210821.csv", 
                col_types = cols(x = col_number(), y = col_number(),          X1 = col_number()), skip = 1)
X022$distance <- NA
X022$distance[2:nrow(X022)] <- sqrt((X022$x[2:nrow(X022)] - X022$x[1:nrow(X022)-1]) ^ 2 + 
                                      (X022$y[2:nrow(X022)] - X022$y[1:nrow(X022)-1]) ^ 2)
time <- X022[nrow(X022), 1]/60
distance <- X022 %>%
  summarise(distance = sum(distance, na.rm = TRUE))
Alg <- "No Algae"
Pyc <- "Pycno"
Urchin <- "Starved"
ID <- "S10"
summary <- tibble(time, distance, Alg, Pyc, Urchin, ID)
Tracker_summary <- bind_rows(Tracker_summary, summary)

##
X023 <- read_csv("Data/2021/Tracker Data/023_ConCon_S11_20210821.csv", 
                col_types = cols(x = col_number(), y = col_number(),          X1 = col_number()), skip = 1)
X023$distance <- NA
X023$distance[2:nrow(X023)] <- sqrt((X023$x[2:nrow(X023)] - X023$x[1:nrow(X023)-1]) ^ 2 + 
                                      (X023$y[2:nrow(X023)] - X023$y[1:nrow(X023)-1]) ^ 2)
time <- X023[nrow(X023), 1]/60
distance <- X023 %>%
  summarise(distance = sum(distance, na.rm = TRUE))
Alg <- "No Algae"
Pyc <- "No Pycno"
Urchin <- "Starved"
ID <- "S11"
summary <- tibble(time, distance, Alg, Pyc, Urchin, ID)
Tracker_summary <- bind_rows(Tracker_summary, summary)

##
X024 <- read_csv("Data/2021/Tracker Data/024_PycCon_S12_20210821.csv", 
                col_types = cols(x = col_number(), y = col_number(),          X1 = col_number()), skip = 1)
X024$distance <- NA
X024$distance[2:nrow(X024)] <- sqrt((X024$x[2:nrow(X024)] - X024$x[1:nrow(X024)-1]) ^ 2 + 
                                      (X024$y[2:nrow(X024)] - X024$y[1:nrow(X024)-1]) ^ 2)
time <- X024[nrow(X024), 1]/60
distance <- X024 %>%
  summarise(distance = sum(distance, na.rm = TRUE))
Alg <- "No Algae"
Pyc <- "Pycno"
Urchin <- "Starved"
ID <- "S12"
summary <- tibble(time, distance, Alg, Pyc, Urchin, ID)
Tracker_summary <- bind_rows(Tracker_summary, summary)

##
X025 <- read_csv("Data/2021/Tracker Data/025_PycAlg_S13_20210822.csv", 
                col_types = cols(x = col_number(), y = col_number(),          X1 = col_number()), skip = 1)
X025$distance <- NA
X025$distance[2:nrow(X025)] <- sqrt((X025$x[2:nrow(X025)] - X025$x[1:nrow(X025)-1]) ^ 2 + 
                                      (X025$y[2:nrow(X025)] - X025$y[1:nrow(X025)-1]) ^ 2)
time <- X025[nrow(X025), 1]/60
distance <- X025 %>%
  summarise(distance = sum(distance, na.rm = TRUE))
Alg <- "Algae"
Pyc <- "Pycno"
Urchin <- "Starved"
ID <- "S13"
summary <- tibble(time, distance, Alg, Pyc, Urchin, ID)
Tracker_summary <- bind_rows(Tracker_summary, summary)

##
X026 <- read_csv("Data/2021/Tracker Data/026_ConAlg_F13_20210822.csv", 
                col_types = cols(x = col_number(), y = col_number(),          X1 = col_number()), skip = 1)
X026$distance <- NA
X026$distance[2:nrow(X026)] <- sqrt((X026$x[2:nrow(X026)] - X026$x[1:nrow(X026)-1]) ^ 2 + 
                                      (X026$y[2:nrow(X026)] - X026$y[1:nrow(X026)-1]) ^ 2)
time <- X026[nrow(X026), 1]/60
distance <- X026 %>%
  summarise(distance = sum(distance, na.rm = TRUE))
Alg <- "Algae"
Pyc <- "No Pycno"
Urchin <- "Fed"
ID <- "F13"
summary <- tibble(time, distance, Alg, Pyc, Urchin, ID)
Tracker_summary <- bind_rows(Tracker_summary, summary)

##
X027 <- read_csv("Data/2021/Tracker Data/027_PycAlg_S14_20210823.csv", 
                col_types = cols(x = col_number(), y = col_number(),          X1 = col_number()), skip = 1)
X027$distance <- NA
X027$distance[2:nrow(X027)] <- sqrt((X027$x[2:nrow(X027)] - X027$x[1:nrow(X027)-1]) ^ 2 + 
                                      (X027$y[2:nrow(X027)] - X027$y[1:nrow(X027)-1]) ^ 2)
time <- X027[nrow(X027), 1]/60
distance <- X027 %>%
  summarise(distance = sum(distance, na.rm = TRUE))
Alg <- "Algae"
Pyc <- "Pycno"
Urchin <- "Starved"
ID <- "S14"
summary <- tibble(time, distance, Alg, Pyc, Urchin, ID)
Tracker_summary <- bind_rows(Tracker_summary, summary)

##
X028 <- read_csv("Data/2021/Tracker Data/028_ConAlg_S14_20210823.csv", 
                col_types = cols(x = col_number(), y = col_number(),          X1 = col_number()), skip = 1)
X028$distance <- NA
X028$distance[2:nrow(X028)] <- sqrt((X028$x[2:nrow(X028)] - X028$x[1:nrow(X028)-1]) ^ 2 + 
                                      (X028$y[2:nrow(X028)] - X028$y[1:nrow(X028)-1]) ^ 2)
time <- X028[nrow(X028), 1]/60
distance <- X028 %>%
  summarise(distance = sum(distance, na.rm = TRUE))
Alg <- "Algae"
Pyc <- "No Pycno"
Urchin <- "Starved"
ID <- "S15"
summary <- tibble(time, distance, Alg, Pyc, Urchin, ID)
Tracker_summary <- bind_rows(Tracker_summary, summary)

##
X029 <- read_csv("Data/2021/Tracker Data/029_ConAlg_S16_20210824.csv", 
                col_types = cols(x = col_number(), y = col_number(),          X1 = col_number()), skip = 1)
X029$distance <- NA
X029$distance[2:nrow(X029)] <- sqrt((X029$x[2:nrow(X029)] - X029$x[1:nrow(X029)-1]) ^ 2 + 
                                      (X029$y[2:nrow(X029)] - X029$y[1:nrow(X029)-1]) ^ 2)
time <- X029[nrow(X029), 1]/60
distance <- X029 %>%
  summarise(distance = sum(distance, na.rm = TRUE))
Alg <- "Algae"
Pyc <- "No Pycno"
Urchin <- "Starved"
ID <- "S16"
summary <- tibble(time, distance, Alg, Pyc, Urchin, ID)
Tracker_summary <- bind_rows(Tracker_summary, summary)

##
X030 <- read_csv("Data/2021/Tracker Data/030_ConAlg_F14_20210824.csv", 
                col_types = cols(x = col_number(), y = col_number(),          X1 = col_number()), skip = 1)
X030$distance <- NA
X030$distance[2:nrow(X030)] <- sqrt((X030$x[2:nrow(X030)] - X030$x[1:nrow(X030)-1]) ^ 2 + 
                                      (X030$y[2:nrow(X030)] - X030$y[1:nrow(X030)-1]) ^ 2)
time <- X030[nrow(X030), 1]/60
distance <- X030 %>%
  summarise(distance = sum(distance, na.rm = TRUE))
Alg <- "Algae"
Pyc <- "No Pycno"
Urchin <- "Fed"
ID <- "F14"
summary <- tibble(time, distance, Alg, Pyc, Urchin, ID)
Tracker_summary <- bind_rows(Tracker_summary, summary)

##
X031 <- read_csv("Data/2021/Tracker Data/031_ConCon_S17_20210825.csv", 
                col_types = cols(x = col_number(), y = col_number(),          X1 = col_number()), skip = 1)
X031$distance <- NA
X031$distance[2:nrow(X031)] <- sqrt((X031$x[2:nrow(X031)] - X031$x[1:nrow(X031)-1]) ^ 2 + 
                                      (X031$y[2:nrow(X031)] - X031$y[1:nrow(X031)-1]) ^ 2)
time <- X031[nrow(X031), 1]/60
distance <- X031 %>%
  summarise(distance = sum(distance, na.rm = TRUE))
Alg <- "No Algae"
Pyc <- "No Pycno"
Urchin <- "Starved"
ID <- "S17"
summary <- tibble(time, distance, Alg, Pyc, Urchin, ID)
Tracker_summary <- bind_rows(Tracker_summary, summary)

#
X032 <- read_csv("Data/2021/Tracker Data/032_ConCon_F15_20210825.csv", 
                col_types = cols(x = col_number(), y = col_number(),          X1 = col_number()), skip = 1)
X032$distance <- NA
X032$distance[2:nrow(X032)] <- sqrt((X032$x[2:nrow(X032)] - X032$x[1:nrow(X032)-1]) ^ 2 + 
                                      (X032$y[2:nrow(X032)] - X032$y[1:nrow(X032)-1]) ^ 2)
time <- X032[nrow(X032), 1]/60
distance <- X032 %>%
  summarise(distance = sum(distance, na.rm = TRUE))
Alg <- "No Algae"
Pyc <- "No Pycno"
Urchin <- "Fed"
ID <- "F15"
summary <- tibble(time, distance, Alg, Pyc, Urchin, ID)
Tracker_summary <- bind_rows(Tracker_summary, summary)

##
X033 <- read_csv("Data/2021/Tracker Data/033_PycCon_F16_20210826.csv", 
                col_types = cols(x = col_number(), y = col_number(),          X1 = col_number()), skip = 1)
X033$distance <- NA
X033$distance[2:nrow(X033)] <- sqrt((X033$x[2:nrow(X033)] - X033$x[1:nrow(X033)-1]) ^ 2 + 
                                      (X033$y[2:nrow(X033)] - X033$y[1:nrow(X033)-1]) ^ 2)
time <- X033[nrow(X033), 1]/60
distance <- X033 %>%
  summarise(distance = sum(distance, na.rm = TRUE))
Alg <- "No Algae"
Pyc <- "Pycno"
Urchin <- "Fed"
ID <- "F16"
summary <- tibble(time, distance, Alg, Pyc, Urchin, ID)
Tracker_summary <- bind_rows(Tracker_summary, summary)

##
X034 <- read_csv("Data/2021/Tracker Data/034_ConCon_F17_20210826.csv", 
                col_types = cols(x = col_number(), y = col_number(),          X1 = col_number()), skip = 1)
X034$distance <- NA
X034$distance[2:nrow(X034)] <- sqrt((X034$x[2:nrow(X034)] - X034$x[1:nrow(X034)-1]) ^ 2 + 
                                      (X034$y[2:nrow(X034)] - X034$y[1:nrow(X034)-1]) ^ 2)
time <- X034[nrow(X034), 1]/60
distance <- X034 %>%
  summarise(distance = sum(distance, na.rm = TRUE))
Alg <- "No Algae"
Pyc <- "No Pycno"
Urchin <- "Fed"
ID <- "F17"
summary <- tibble(time, distance, Alg, Pyc, Urchin, ID)
Tracker_summary <- bind_rows(Tracker_summary, summary)


##
X035 <- read_csv("Data/2021/Tracker Data/035_PycAlg_S18_20210827.csv", 
                col_types = cols(x = col_number(), y = col_number(),          X1 = col_number()), skip = 1)
X035$distance <- NA
X035$distance[2:nrow(X035)] <- sqrt((X035$x[2:nrow(X035)] - X035$x[1:nrow(X035)-1]) ^ 2 + 
                                      (X035$y[2:nrow(X035)] - X035$y[1:nrow(X035)-1]) ^ 2)
time <- X035[nrow(X035), 1]/60
distance <- X035 %>%
  summarise(distance = sum(distance, na.rm = TRUE))
Alg <- "Algae"
Pyc <- "Pycno"
Urchin <- "Starved"
ID <- "S18"
summary <- tibble(time, distance, Alg, Pyc, Urchin, ID)
Tracker_summary <- bind_rows(Tracker_summary, summary)

##
X036 <- read_csv("Data/2021/Tracker Data/036_PycCon_F18_20210827.csv", 
                 col_types = cols(x = col_number(), y = col_number(), 
                                  X1 = col_number()), skip = 1)
X036$distance <- NA
X036$distance[2:nrow(X036)] <- sqrt((X036$x[2:nrow(X036)] - X036$x[1:nrow(X036)-1]) ^ 2 + 
                                      (X036$y[2:nrow(X036)] - X036$y[1:nrow(X036)-1]) ^ 2)
time <- X036[nrow(X036), 1]/60
distance <- X036 %>%
  summarise(distance = sum(distance, na.rm = TRUE))
Alg <- "No Algae"
Pyc <- "Pycno"
Urchin <- "Fed"
ID <- "F18"
summary <- tibble(time, distance, Alg, Pyc, Urchin, ID)
Tracker_summary <- bind_rows(Tracker_summary, summary)

##
X037 <- read_csv("Data/2021/Tracker Data/037_PycCon_F19_20210828.csv", 
                 col_types = cols(x = col_number(), y = col_number(), 
                                  X1 = col_number()), skip = 1)
X037$distance <- NA
X037$distance[2:nrow(X037)] <- sqrt((X037$x[2:nrow(X037)] - X037$x[1:nrow(X037)-1]) ^ 2 + 
                                      (X037$y[2:nrow(X037)] - X037$y[1:nrow(X037)-1]) ^ 2)
time <- X037[nrow(X037), 1]/60
distance <- X037 %>%
  summarise(distance = sum(distance, na.rm = TRUE))
Alg <- "No Algae"
Pyc <- "Pycno"
Urchin <- "Fed"
ID <- "F19"
summary <- tibble(time, distance, Alg, Pyc, Urchin, ID)
Tracker_summary <- bind_rows(Tracker_summary, summary)

##
X038 <- read_csv("Data/2021/Tracker Data/038_ConCon_F20_20210828.csv", 
                 col_types = cols(x = col_number(), y = col_number(), 
                                  X1 = col_number()), skip = 1)
X038$distance <- NA
X038$distance[2:nrow(X038)] <- sqrt((X038$x[2:nrow(X038)] - X038$x[1:nrow(X038)-1]) ^ 2 + 
                                      (X038$y[2:nrow(X038)] - X038$y[1:nrow(X038)-1]) ^ 2)
time <- X038[nrow(X038), 1]/60
distance <- X038 %>%
  summarise(distance = sum(distance, na.rm = TRUE))
Alg <- "No Algae"
Pyc <- "No Pycno"
Urchin <- "Fed"
ID <- "F20"
summary <- tibble(time, distance, Alg, Pyc, Urchin, ID)
Tracker_summary <- bind_rows(Tracker_summary, summary)

##
X039 <- read_csv("Data/2021/Tracker Data/039_ConCon_F20_20210828.csv", 
                 col_types = cols(x = col_number(), y = col_number(), 
                                  X1 = col_number()), skip = 1)
X039$distance <- NA
X039$distance[2:nrow(X039)] <- sqrt((X039$x[2:nrow(X039)] - X039$x[1:nrow(X039)-1]) ^ 2 + 
                                      (X039$y[2:nrow(X039)] - X039$y[1:nrow(X039)-1]) ^ 2)
time <- X039[nrow(X039), 1]/60
distance <- X039 %>%
  summarise(distance = sum(distance, na.rm = TRUE))
Alg <- "No Algae"
Pyc <- "No Pycno"
Urchin <- "Starved"
ID <- "S19"
summary <- tibble(time, distance, Alg, Pyc, Urchin, ID)
Tracker_summary <- bind_rows(Tracker_summary, summary)

##
X040 <- read_csv("Data/2021/Tracker Data/040_ConAlg_S20_20210828.csv", 
                 col_types = cols(x = col_number(), y = col_number(), 
                                  X1 = col_number()), skip = 1)
X040$distance <- NA
X040$distance[2:nrow(X040)] <- sqrt((X040$x[2:nrow(X040)] - X040$x[1:nrow(X040)-1]) ^ 2 + 
                                      (X040$y[2:nrow(X040)] - X040$y[1:nrow(X040)-1]) ^ 2)
time <- X040[nrow(X040), 1]/60
distance <- X040 %>%
  summarise(distance = sum(distance, na.rm = TRUE))
Alg <- "Algae"
Pyc <- "No Pycno"
Urchin <- "Starved"
ID <- "S20"
summary <- tibble(time, distance, Alg, Pyc, Urchin, ID)
Tracker_summary <- bind_rows(Tracker_summary, summary)

##
X041 <- read_csv("Data/2021/Tracker Data/041_ConCon_S21_20210829.csv", 
                 col_types = cols(x = col_number(), y = col_number(), 
                                  X1 = col_number()), skip = 1)
X041$distance <- NA
X041$distance[2:nrow(X041)] <- sqrt((X041$x[2:nrow(X041)] - X041$x[1:nrow(X041)-1]) ^ 2 + 
                                      (X041$y[2:nrow(X041)] - X041$y[1:nrow(X041)-1]) ^ 2)
time <- X041[nrow(X041), 1]/60
distance <- X041 %>%
  summarise(distance = sum(distance, na.rm = TRUE))
Alg <- "No Algae"
Pyc <- "No Pycno"
Urchin <- "Starved"
ID <- "S21"
summary <- tibble(time, distance, Alg, Pyc, Urchin, ID)
Tracker_summary <- bind_rows(Tracker_summary, summary)

##
X042 <- read_csv("Data/2021/Tracker Data/042_PycCon_S22_20210829.csv", 
                 col_types = cols(x = col_number(), y = col_number(), 
                                  X1 = col_number()), skip = 1)
X042$distance <- NA
X042$distance[2:nrow(X042)] <- sqrt((X042$x[2:nrow(X042)] - X042$x[1:nrow(X042)-1]) ^ 2 + 
                                      (X042$y[2:nrow(X042)] - X042$y[1:nrow(X042)-1]) ^ 2)
time <- X042[nrow(X042), 1]/60
distance <- X042 %>%
  summarise(distance = sum(distance, na.rm = TRUE))
Alg <- "No Algae"
Pyc <- "Pycno"
Urchin <- "Starved"
ID <- "S22"
summary <- tibble(time, distance, Alg, Pyc, Urchin, ID)
Tracker_summary <- bind_rows(Tracker_summary, summary)

##
X043 <- read_csv("Data/2021/Tracker Data/043_PycAlg_F21_20210829.csv", 
                 col_types = cols(x = col_number(), y = col_number(), 
                                  X1 = col_number()), skip = 1)
X043$distance <- NA
X043$distance[2:nrow(X043)] <- sqrt((X043$x[2:nrow(X043)] - X043$x[1:nrow(X043)-1]) ^ 2 + 
                                      (X043$y[2:nrow(X043)] - X043$y[1:nrow(X043)-1]) ^ 2)
time <- X043[nrow(X043), 1]/60
distance <- X043 %>%
  summarise(distance = sum(distance, na.rm = TRUE))
Alg <- "Algae"
Pyc <- "Pycno"
Urchin <- "Fed"
ID <- "F21"
summary <- tibble(time, distance, Alg, Pyc, Urchin, ID)
Tracker_summary <- bind_rows(Tracker_summary, summary)

##
X044 <- read_csv("Data/2021/Tracker Data/044_PycAlg_F22_20210829.csv", 
                 col_types = cols(x = col_number(), y = col_number(), 
                                  X1 = col_number()), skip = 1)
X044$distance <- NA
X044$distance[2:nrow(X044)] <- sqrt((X044$x[2:nrow(X044)] - X044$x[1:nrow(X044)-1]) ^ 2 + 
                                      (X044$y[2:nrow(X044)] - X044$y[1:nrow(X044)-1]) ^ 2)
time <- X044[nrow(X044), 1]/60
distance <- X044 %>%
  summarise(distance = sum(distance, na.rm = TRUE))
Alg <- "Algae"
Pyc <- "Pycno"
Urchin <- "Fed"
ID <- "F22"
summary <- tibble(time, distance, Alg, Pyc, Urchin, ID)
Tracker_summary <- bind_rows(Tracker_summary, summary)


X045 <- read_csv("Data/2021/Tracker Data/045_PycAlg_F23_20210830.csv", 
                 col_types = cols(x = col_number(), y = col_number(), 
                                  X1 = col_number()), skip = 1)
X045$distance <- NA
X045$distance[2:nrow(X045)] <- sqrt((X045$x[2:nrow(X045)] - X045$x[1:nrow(X045)-1]) ^ 2 + 
                                      (X045$y[2:nrow(X045)] - X045$y[1:nrow(X045)-1]) ^ 2)
time <- X045[nrow(X045), 1]/60
distance <- X045 %>%
  summarise(distance = sum(distance, na.rm = TRUE))
Alg <- "Algae"
Pyc <- "Pycno"
Urchin <- "Fed"
ID <- "F23"
summary <- tibble(time, distance, Alg, Pyc, Urchin, ID)
Tracker_summary <- bind_rows(Tracker_summary, summary)

##
X046 <- read_csv("Data/2021/Tracker Data/046_PycCon_S23_20210830.csv", 
                 col_types = cols(x = col_number(), y = col_number(), 
                                  X1 = col_number()), skip = 1)
X046$distance <- NA
X046$distance[2:nrow(X046)] <- sqrt((X046$x[2:nrow(X046)] - X046$x[1:nrow(X046)-1]) ^ 2 + 
                                      (X046$y[2:nrow(X046)] - X046$y[1:nrow(X046)-1]) ^ 2)
time <- X046[nrow(X046), 1]/60
distance <- X046 %>%
  summarise(distance = sum(distance, na.rm = TRUE))
Alg <- "No Algae"
Pyc <- "Pycno"
Urchin <- "Starved"
ID <- "S23"
summary <- tibble(time, distance, Alg, Pyc, Urchin, ID)
Tracker_summary <- bind_rows(Tracker_summary, summary)

##
X047 <- read_csv("Data/2021/Tracker Data/047_PycCon_S24_20210830.csv", 
                 col_types = cols(x = col_number(), y = col_number(), 
                                  X1 = col_number()), skip = 1)
X047$distance <- NA
X047$distance[2:nrow(X047)] <- sqrt((X047$x[2:nrow(X047)] - X047$x[1:nrow(X047)-1]) ^ 2 + 
                                      (X047$y[2:nrow(X047)] - X047$y[1:nrow(X047)-1]) ^ 2)
time <- X047[nrow(X047), 1]/60
distance <- X047 %>%
  summarise(distance = sum(distance, na.rm = TRUE))
Alg <- "No Algae"
Pyc <- "Pycno"
Urchin <- "Starved"
ID <- "S24"
summary <- tibble(time, distance, Alg, Pyc, Urchin, ID)
Tracker_summary <- bind_rows(Tracker_summary, summary)

##
X048 <- read_csv("Data/2021/Tracker Data/048_ConAlg_F24_20210830.csv", 
                 col_types = cols(x = col_number(), y = col_number(), 
                                  X1 = col_number()), skip = 1)
X048$distance <- NA
X048$distance[2:nrow(X048)] <- sqrt((X048$x[2:nrow(X048)] - X048$x[1:nrow(X048)-1]) ^ 2 + 
                                      (X048$y[2:nrow(X048)] - X048$y[1:nrow(X048)-1]) ^ 2)
time <- X048[nrow(X048), 1]/60
distance <- X048 %>%
  summarise(distance = sum(distance, na.rm = TRUE))
Alg <- "Algae"
Pyc <- "No Pycno"
Urchin <- "Fed"
ID <- "F24"
summary <- tibble(time, distance, Alg, Pyc, Urchin, ID)
Tracker_summary <- bind_rows(Tracker_summary, summary)

##
X049 <- read_csv("Data/2021/Tracker Data/049_ConAlg_S25_20210904.csv", 
                 col_types = cols(x = col_number(), y = col_number(), 
                                  X1 = col_number()), skip = 1)
X049$distance <- NA
X049$distance[2:nrow(X049)] <- sqrt((X049$x[2:nrow(X049)] - X049$x[1:nrow(X049)-1]) ^ 2 + 
                                      (X049$y[2:nrow(X049)] - X049$y[1:nrow(X049)-1]) ^ 2)
time <- X049[nrow(X049), 1]/60
distance <- X049 %>%
  summarise(distance = sum(distance, na.rm = TRUE))
Alg <- "Algae"
Pyc <- "No Pycno"
Urchin <- "Starved"
ID <- "S25"
summary <- tibble(time, distance, Alg, Pyc, Urchin, ID)
Tracker_summary <- bind_rows(Tracker_summary, summary)

# remove 20 min replicate
Tracker_summary <- Tracker_summary %>%
  filter(ID != "S02")


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

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# MANIPULATE DATA - create adehabitatLT objects                                ####
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Figures and stats for total distance travelled

Tracker_summary %>%
  unite("Treatment", Alg:Urchin, sep = " + ") %>%
  mutate(Treatment =  factor(Treatment, levels = c("No Algae + No Pycno + Starved",
                                                   "Algae + No Pycno + Starved",
                                                   "No Algae + Pycno + Starved",
                                                   "Algae + Pycno + Starved",
                                                   "No Algae + No Pycno + Fed",
                                                   "Algae + No Pycno + Fed",
                                                   "No Algae + Pycno + Fed",
                                                   "Algae + Pycno + Fed"))) %>%
  ggplot(aes(x = distance, y = Treatment)) +
  stat_summary(
    geom = "point",
    fun = "mean",
    size = 4,
    shape = 19
  ) +
  geom_errorbar(stat="summary", fun.data="mean_se", size = 1) +
  theme_minimal(base_size = 15) +
  theme(legend.position = "top") 

  ############### 
  
  ####
  #<<<<<<<<<<<<<<<<<<<<<<<<<<END OF SCRIPT>>>>>>>>>>>>>>>>>>>>>>>>#

# SCRATCH PAD ####

         