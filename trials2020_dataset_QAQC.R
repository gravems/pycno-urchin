#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#                                                                                ##
# Pycno-Urchin Dataset 2020 QAQC                                                 ##
# Script created 2023-01-13                                                      ##
# Last updated 2023-01-13                                                        ##
# Data source: Ross Whippo                                                       ##
# R code prepared by Ross Whippo                                                 ##
# Last updated 2023-01-13                                                        ##
#                                                                                ##
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# SUMMARY:
# A script to QAQC and parse the 2020 pycno-urchin dataset.


# Required Files (check that script is loading latest version):
# urchin_fear_feeding_pycno-trial-1.csv

# Associated Scripts:
# Indirect_Urchin_MS.R

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

# 2023-01-13 Script created

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# LOAD PACKAGES                                                                ####
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

library(tidyverse)

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# READ IN DATA                                                                 ####
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

trials2020 <- read_csv("Data/2020/urchin_fear_feeding_pycno-trial-1.csv", 
                       col_types = cols(date = col_character(), 
                                        time = col_time(format = "%H:%M"), 
                                        trial = col_factor(levels = c("1", "2", "3", "4", "5")), 
                                        timepoint = col_integer(), 
                                        diameter = col_double(), 
                                        pycno = col_factor(levels = c("yes", "no"))))



#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# MANIPULATE DATA                                                              ####
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# combine trial and bin to prevent combining urchins across trials
trials2020 <- unite(urchin_fear_pycno,
        'ID', trial, bin, sep = "_", remove = FALSE)

# create column to ID 'bucket' empty treatments
trials2020 <- trials2020 %>%
  mutate(bucket = treatment)

# change 'bucket' to 'empty' in treatment column
trials2020 <- trials2020 %>%
  mutate(treatment = replace(treatment, treatment == 'bucket', 'empty'))

# add ND diameter to 61 (from notes?)
trials2020 <- trials2020 %>%
  mutate(diameter = replace_na(diameter, 61))

  

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# EXPORT DATA                                                                  ####
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

write_csv(trials2020, "Data/2020/trials2020_QAQC.csv")

############### SUBSECTION HERE

####
#<<<<<<<<<<<<<<<<<<<<<<<<<<END OF SCRIPT>>>>>>>>>>>>>>>>>>>>>>>>#

# SCRATCH PAD ####
