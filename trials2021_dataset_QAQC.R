#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#                                                                                ##
# Pycno-Urchin Dataset 2021 QAQC                                                 ##
# Script created 2021-08-19                                                      ##
# Last updated 2021-09-04                                                        ##
# Data source: Ross Whippo                                                       ##
# R code prepared by Ross Whippo                                                 ##
# Last updated 2021-08-31                                                        ##
#                                                                                ##
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# SUMMARY:
# A script to QAQC and parse the 2021 pycno-urchin dataset.


# Required Files (check that script is loading latest version):
# trials2021.csv

# Associated Scripts:
# trials2021_figExplore.R

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

# 2021-08-19 Script created

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# LOAD PACKAGES                                                                ####
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

library(tidyverse)

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# READ IN DATA                                                                 ####
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

trials2021 <- read_csv("Data/2021/trials2021.csv", 
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

# fill in nd data from 017 with video observations

trials2021$distance_cm <- trials2021$distance_cm %>%
  recode("nd" = "0")
trials2021$movement <- trials2021$movement %>%
  recode("nd" = "st")
trials2021$location <- trials2021$location %>%
  recode("nd" = "c")
trials2021$spines <- trials2021$spines %>%
  recode("nd" = "st")
trials2021$tubefeet <- trials2021$tubefeet %>%
  recode("nd" = "ex")
trials2021$interaction <- trials2021$interaction %>%
  recode("nd" = "i")

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# EXPORT DATA                                                                  ####
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

write_csv(trials2021, "Data/2021/trials2021_QAQC.csv")

############### SUBSECTION HERE

####
#<<<<<<<<<<<<<<<<<<<<<<<<<<END OF SCRIPT>>>>>>>>>>>>>>>>>>>>>>>>#

# SCRATCH PAD ####