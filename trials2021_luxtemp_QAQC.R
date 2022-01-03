#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#                                                                                ##
# Pycno-Urchin Lux Temp data      `  `                                           ##
# Script created 2021-09-02                                                      ##
# Last updated 2021-09-02                                                        ##
# Data source: Ross Whippo                                                       ##
# R code prepared by Ross Whippo                                                 ##
#                                                                                ##
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# SUMMARY:
# A script to QAQC and parse the 2021 pycno-urchin lux and temp dataset.


# Required Files (check that script is loading latest version):
# 001%202021-08-15%2015-42-41%20-0700.csv
# 002%202021-08-16%2010-03-40%20-0700.csv
# 003%202021-08-16%2014-51-11%20-0700.csv
# 004%202021-08-16%2017-18-06%20-0700.csv
# 005%202021-08-17%2010-28-02%20-0700.csv
# 006%202021-08-17%2013-11-23%20-0700.csv
# 007%202021-08-17%2015-38-50%20-0700.csv
# 008%202021-08-17%2017-33-46%20-0700.csv
# 009%202021-08-18%2009-35-17%20-0700.csv
# 010%202021-08-18%2011-40-44%20-0700.csv
# 011x%202021-08-18%2013-56-18%20-0700.csv
# 012%202021-08-18%2015-47-53%20-0700.csv
# 013%202021-08-19%2009-24-12%20-0700.csv
# 014%202021-08-19%2011-23-07%20-0700.csv
# 015%202021-08-19%2013-20-34%20-0700.csv
# 016%202021-08-19%2015-11-45%20-0700.csv
# 017%202021-08-20%2008-46-37%20-0700.csv
# 018%202021-08-20%2010-44-25%20-0700.csv
# 019%202021-08-20%2013-05-40%20-0700.csv
# 020%202021-08-20%2015-08-07%20-0700.csv
# 021%202021-08-21%2009-06-28%20-0700.csv
# 022%202021-08-21%2011-07-03%20-0700.csv
# 023%202021-08-21%2012-59-53%20-0700.csv
# 024%202021-08-21%2014-51-47%20-0700.csv
# 025%202021-08-22%2009-30-50%20-0700.csv
# 026%202021-08-22%2011-46-27%20-0700.csv
# 027%202021-08-23%2009-29-01%20-0700.csv
# 028%202021-08-23%2011-30-35%20-0700.csv
# 029%202021-08-24%2009-04-30%20-0700.csv
# 030%202021-08-24%2010-55-28%20-0700.csv
# 031%202021-08-25%2008-56-14%20-0700.csv
# 032%202021-08-25%2011-18-49%20-0700.csv
# 033%202021-08-26%2008-34-11%20-0700.csv
# 034%202021-08-26%2010-11-43%20-0700.csv
# 035%202021-08-27%2009-08-39%20-0700.csv
# 036%202021-08-27%2011-06-51%20-0700.csv
# 037%202021-08-28%2009-28-22%20-0700.csv
# 038%202021-08-28%2011-18-39%20-0700.csv
# 039%202021-08-28%2013-45-18%20-0700.csv
# 040%202021-08-28%2016-20-25%20-0700.csv
# 041%202021-08-29%2009-36-02%20-0700.csv
# 042%202021-08-29%2011-58-23%20-0700.csv
# 043%202021-08-29%2013-50-29%20-0700.csv
# 044%202021-08-29%2016-03-49%20-0700.csv
# 045%202021-08-30%2009-23-27%20-0700.csv
# 046%202021-08-30%2011-17-58%20-0700.csv
# 047%202021-08-30%2013-07-32%20-0700.csv
# 048%202021-08-30%2015-22-15%20-0700.csv

# trials2021_QAQC.csv


# Associated Scripts:
# 

# TO DO 
# 2021-09-04 Still need to remove "urchin on top of HOBO lux values"

# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# TABLE OF CONTENTS                                                            ####
#                                                                                 +
# RECENT CHANGES TO SCRIPT                                                        +
# LOAD PACKAGES                                                                   +
# READ IN DATA                                                                    +
# MANIPULATE DATA                                                                 +
# EXPORT DATA                                                                     +
#                                                                                 +
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# RECENT CHANGES TO SCRIPT                                                     ####
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# 2021-09-02 Script created
# 2021-09-04 finished first draft of script

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# LOAD PACKAGES                                                                ####
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

library(tidyverse)

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# READ IN DATA                                                                 ####
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

  
  luxtemp_001 <- read_csv("Data/2021/001 2021-08-15 15-42-41 -0700.csv", 
                                            col_types = cols(`Button Down` = col_skip(), 
                                                             `Button Up` = col_skip(), 
                                                             `Date Time, GMT -0700` = col_character(), 
                                                             EOF = col_skip(), 
                                                             `Host Connect` = col_skip(), 
                                                             Stopped = col_skip()), skip = 2)
  luxtemp_001$trial <- "001"
  
  
  luxtemp_002 <- read_csv("Data/2021/002 2021-08-16 10-03-40 -0700.csv", 
                          col_types = cols(`Button Down` = col_skip(), 
                                           `Button Up` = col_skip(), 
                                           `Date Time, GMT -0700` = col_character(), 
                                           EOF = col_skip(), 
                                           `Host Connect` = col_skip(), 
                                           Stopped = col_skip()), skip = 2)
  luxtemp_002$trial <- "002"
  
  luxtemp_003 <- read_csv("Data/2021/003 2021-08-16 14-51-11 -0700.csv", 
                          col_types = cols(`Button Down` = col_skip(), 
                                           `Button Up` = col_skip(), 
                                           `Date Time, GMT -0700` = col_character(), 
                                           EOF = col_skip(), 
                                           `Host Connect` = col_skip(), 
                                           Stopped = col_skip()), skip = 2)
  luxtemp_003$trial <- "003"
  
  luxtemp_004 <- read_csv("Data/2021/004 2021-08-16 17-18-06 -0700.csv", 
                          col_types = cols(`Button Down` = col_skip(), 
                                           `Button Up` = col_skip(), 
                                           `Date Time, GMT -0700` = col_character(), 
                                           EOF = col_skip(), 
                                           `Host Connect` = col_skip(), 
                                           Stopped = col_skip()), skip = 2)
  luxtemp_004$trial <- "004"
  
  luxtemp_005 <- read_csv("Data/2021/005 2021-08-17 10-28-02 -0700.csv", 
                          col_types = cols(`Button Down` = col_skip(), 
                                           `Button Up` = col_skip(), 
                                           `Date Time, GMT -0700` = col_character(), 
                                           EOF = col_skip(), 
                                           `Host Connect` = col_skip(), 
                                           Stopped = col_skip()), skip = 2)
  luxtemp_005$trial <- "005"
  
  luxtemp_006 <- read_csv("Data/2021/006 2021-08-17 13-11-23 -0700.csv", 
                          col_types = cols(`Button Down` = col_skip(), 
                                           `Button Up` = col_skip(), 
                                           `Date Time, GMT -0700` = col_character(), 
                                           EOF = col_skip(), 
                                           `Host Connect` = col_skip(), 
                                           Stopped = col_skip()), skip = 2)
  luxtemp_006$trial <- "006"
  
  luxtemp_007 <- read_csv("Data/2021/007 2021-08-17 15-38-50 -0700.csv", 
                          col_types = cols(`Button Down` = col_skip(), 
                                           `Button Up` = col_skip(), 
                                           `Date Time, GMT -0700` = col_character(), 
                                           EOF = col_skip(), 
                                           `Host Connect` = col_skip(), 
                                           Stopped = col_skip()), skip = 2)
  luxtemp_007$trial <- "007"
  
  luxtemp_008 <- read_csv("Data/2021/008 2021-08-17 17-33-46 -0700.csv", 
                          col_types = cols(`Button Down` = col_skip(), 
                                           `Button Up` = col_skip(), 
                                           `Date Time, GMT -0700` = col_character(), 
                                           EOF = col_skip(), 
                                           `Host Connect` = col_skip(), 
                                           Stopped = col_skip()), skip = 2)
  luxtemp_008$trial <- "008"

luxtemp_009 <- read_csv("Data/2021/009 2021-08-18 09-35-17 -0700.csv", 
                        col_types = cols(`Button Down` = col_skip(), 
                                         `Button Up` = col_skip(), 
                                         `Date Time, GMT -0700` = col_character(), 
                                         EOF = col_skip(), 
                                         `Host Connect` = col_skip(), 
                                         Stopped = col_skip()), skip = 2)
luxtemp_009$trial <- "009"

luxtemp_010 <- read_csv("Data/2021/010 2021-08-18 11-40-44 -0700.csv", 
                        col_types = cols(`Button Down` = col_skip(), 
                                         `Button Up` = col_skip(), 
                                         `Date Time, GMT -0700` = col_character(), 
                                         EOF = col_skip(), 
                                         `Host Connect` = col_skip(), 
                                         Stopped = col_skip()), skip = 2)
luxtemp_010$trial <- "010"

luxtemp_011x <- read_csv("Data/2021/011x 2021-08-18 13-56-18 -0700.csv", 
                         col_types = cols(`Button Down` = col_skip(), 
                                          `Button Up` = col_skip(), 
                                          `Date Time, GMT -0700` = col_character(), 
                                          EOF = col_skip(), 
                                          `Host Connect` = col_skip(), 
                                          Stopped = col_skip()), skip = 2)
luxtemp_011x$trial <- "011"

luxtemp_012 <- read_csv("Data/2021/012 2021-08-18 15-47-53 -0700.csv", 
                        col_types = cols(`Button Down` = col_skip(), 
                                         `Button Up` = col_skip(), 
                                         `Date Time, GMT -0700` = col_character(), 
                                         EOF = col_skip(), 
                                         `Host Connect` = col_skip(), 
                                         Stopped = col_skip()), skip = 2)
luxtemp_012$trial <- "012"

luxtemp_013 <- read_csv("Data/2021/013 2021-08-19 09-24-12 -0700.csv", 
                        col_types = cols(`Button Down` = col_skip(), 
                                         `Button Up` = col_skip(), 
                                         `Date Time, GMT -0700` = col_character(), 
                                         EOF = col_skip(), 
                                         `Host Connect` = col_skip(), 
                                         Stopped = col_skip()), skip = 2)
luxtemp_013$trial <- "013"

luxtemp_014 <- read_csv("Data/2021/014 2021-08-19 11-23-07 -0700.csv", 
                        col_types = cols(`Button Down` = col_skip(), 
                                         `Button Up` = col_skip(), 
                                         `Date Time, GMT -0700` = col_character(), 
                                         EOF = col_skip(), 
                                         `Host Connect` = col_skip(), 
                                         Stopped = col_skip()), skip = 2)
luxtemp_014$trial <- "014"

luxtemp_015 <- read_csv("Data/2021/015 2021-08-19 13-20-34 -0700.csv", 
                       col_types = cols(`Button Down` = col_skip(), 
                                        `Button Up` = col_skip(), 
                                        `Date Time, GMT -0700` = col_character(), 
                                        EOF = col_skip(), 
                                        `Host Connect` = col_skip(), 
                                        Stopped = col_skip()), skip = 2)
luxtemp_015$trial <- "015"

luxtemp_016 <- read_csv("Data/2021/016 2021-08-19 15-11-45 -0700.csv", 
                        col_types = cols(`Button Down` = col_skip(), 
                                         `Button Up` = col_skip(), 
                                         `Date Time, GMT -0700` = col_character(), 
                                         EOF = col_skip(), 
                                         `Host Connect` = col_skip(), 
                                         Stopped = col_skip()), skip = 2)
luxtemp_016$trial <- "016"

luxtemp_017 <- read_csv("Data/2021/017 2021-08-20 08-46-37 -0700.csv", 
                        col_types = cols(`Button Down` = col_skip(), 
                                         `Button Up` = col_skip(), 
                                         `Date Time, GMT -0700` = col_character(), 
                                         EOF = col_skip(), 
                                         `Host Connect` = col_skip(), 
                                         Stopped = col_skip()), skip = 2)
luxtemp_017$trial <- "017"

luxtemp_018 <- read_csv("Data/2021/018 2021-08-20 10-44-25 -0700.csv", 
                        col_types = cols(`Button Down` = col_skip(), 
                                         `Button Up` = col_skip(), 
                                         `Date Time, GMT -0700` = col_character(), 
                                         EOF = col_skip(), 
                                         `Host Connect` = col_skip(), 
                                         Stopped = col_skip()), skip = 2)
luxtemp_018$trial <- "018"

luxtemp_019 <- read_csv("Data/2021/019 2021-08-20 13-05-40 -0700.csv", 
                        col_types = cols(`Button Down` = col_skip(), 
                                         `Button Up` = col_skip(), 
                                         `Date Time, GMT -0700` = col_character(), 
                                         EOF = col_skip(), 
                                         `Host Connect` = col_skip(), 
                                         Stopped = col_skip()), skip = 2)
luxtemp_019$trial <- "019"

luxtemp_020 <- read_csv("Data/2021/020 2021-08-20 15-08-07 -0700.csv", 
                        col_types = cols(`Button Down` = col_skip(), 
                                         `Button Up` = col_skip(), 
                                         `Date Time, GMT -0700` = col_character(), 
                                         EOF = col_skip(), 
                                         `Host Connect` = col_skip(), 
                                         Stopped = col_skip()), skip = 2)
luxtemp_020$trial <- "020"

luxtemp_021 <- read_csv("Data/2021/021 2021-08-21 09-06-28 -0700.csv", 
                        col_types = cols(`Button Down` = col_skip(), 
                                         `Button Up` = col_skip(), 
                                         `Date Time, GMT -0700` = col_character(), 
                                         EOF = col_skip(), 
                                         `Host Connect` = col_skip(), 
                                         Stopped = col_skip()), skip = 2)
luxtemp_021$trial <- "021"

luxtemp_022 <- read_csv("Data/2021/022 2021-08-21 11-07-03 -0700.csv", 
                        col_types = cols(`Button Down` = col_skip(), 
                                         `Button Up` = col_skip(), 
                                         `Date Time, GMT -0700` = col_character(), 
                                         EOF = col_skip(), 
                                         `Host Connect` = col_skip(), 
                                         Stopped = col_skip()), skip = 2)
luxtemp_022$trial <- "022"

luxtemp_023 <- read_csv("Data/2021/023 2021-08-21 12-59-53 -0700.csv", 
                        col_types = cols(`Button Down` = col_skip(), 
                                         `Button Up` = col_skip(), 
                                         `Date Time, GMT -0700` = col_character(), 
                                         EOF = col_skip(), 
                                         `Host Connect` = col_skip(), 
                                         Stopped = col_skip()), skip = 2)
luxtemp_023$trial <- "023"

luxtemp_024 <- read_csv("Data/2021/024 2021-08-21 14-51-47 -0700.csv", 
                        col_types = cols(`Button Down` = col_skip(), 
                                         `Button Up` = col_skip(), 
                                         `Date Time, GMT -0700` = col_character(), 
                                         EOF = col_skip(), 
                                         `Host Connect` = col_skip(), 
                                         Stopped = col_skip()), skip = 2)
luxtemp_024$trial <- "024"

luxtemp_025 <- read_csv("Data/2021/025 2021-08-22 09-30-50 -0700.csv", 
                        col_types = cols(`Button Down` = col_skip(), 
                                         `Button Up` = col_skip(), 
                                         `Date Time, GMT -0700` = col_character(), 
                                         EOF = col_skip(), 
                                         `Host Connect` = col_skip(), 
                                         Stopped = col_skip()), skip = 2)
luxtemp_025$trial <- "025"

luxtemp_026 <- read_csv("Data/2021/026 2021-08-22 11-46-27 -0700.csv", 
                        col_types = cols(`Button Down` = col_skip(), 
                                         `Button Up` = col_skip(), 
                                         `Date Time, GMT -0700` = col_character(), 
                                         EOF = col_skip(), 
                                         `Host Connect` = col_skip(), 
                                         Stopped = col_skip()), skip = 2)
luxtemp_026$trial <- "026"

luxtemp_027 <- read_csv("Data/2021/027 2021-08-23 09-29-01 -0700.csv", 
                        col_types = cols(`Button Down` = col_skip(), 
                                         `Button Up` = col_skip(), 
                                         `Date Time, GMT -0700` = col_character(), 
                                         EOF = col_skip(), 
                                         `Host Connect` = col_skip(), 
                                         Stopped = col_skip()), skip = 2)
luxtemp_027$trial <- "027"

luxtemp_028 <- read_csv("Data/2021/028 2021-08-23 11-30-35 -0700.csv", 
                        col_types = cols(`Button Down` = col_skip(), 
                                         `Button Up` = col_skip(), 
                                         `Date Time, GMT -0700` = col_character(), 
                                         EOF = col_skip(), 
                                         `Host Connect` = col_skip(), 
                                         Stopped = col_skip()), skip = 2)
luxtemp_028$trial <- "028"

luxtemp_029 <- read_csv("Data/2021/029 2021-08-24 09-04-30 -0700.csv", 
                        col_types = cols(`Button Down` = col_skip(), 
                                         `Button Up` = col_skip(), 
                                         `Date Time, GMT -0700` = col_character(), 
                                         EOF = col_skip(), 
                                         `Host Connect` = col_skip(), 
                                         Stopped = col_skip()), skip = 2)
luxtemp_029$trial <- "029"

luxtemp_030 <- read_csv("Data/2021/030 2021-08-24 10-55-28 -0700.csv", 
                        col_types = cols(`Button Down` = col_skip(), 
                                         `Button Up` = col_skip(), 
                                         `Date Time, GMT -0700` = col_character(), 
                                         EOF = col_skip(), 
                                         `Host Connect` = col_skip(), 
                                         Stopped = col_skip()), skip = 2)
luxtemp_030$trial <- "030"

luxtemp_031 <- read_csv("Data/2021/031 2021-08-25 08-56-14 -0700.csv", 
                        col_types = cols(`Button Down` = col_skip(), 
                                         `Button Up` = col_skip(), 
                                         `Date Time, GMT -0700` = col_character(), 
                                         EOF = col_skip(), 
                                         `Host Connect` = col_skip(), 
                                         Stopped = col_skip()), skip = 2)
luxtemp_031$trial <- "031"

luxtemp_032 <- read_csv("Data/2021/032 2021-08-25 11-18-49 -0700.csv", 
                        col_types = cols(`Button Down` = col_skip(), 
                                         `Button Up` = col_skip(), 
                                         `Date Time, GMT -0700` = col_character(), 
                                         EOF = col_skip(), 
                                         `Host Connect` = col_skip(), 
                                         Stopped = col_skip()), skip = 2)
luxtemp_032$trial <- "032"
luxtemp_032 <- luxtemp_032 %>%
  drop_na()
  
luxtemp_033 <- read_csv("Data/2021/033 2021-08-26 08-34-11 -0700.csv", 
                        col_types = cols(`Button Down` = col_skip(), 
                                         `Button Up` = col_skip(), 
                                         `Date Time, GMT -0700` = col_character(), 
                                         EOF = col_skip(), 
                                         `Host Connect` = col_skip(), 
                                         Stopped = col_skip()), skip = 2)
luxtemp_033$trial <- "033"

luxtemp_034 <- read_csv("Data/2021/034 2021-08-26 10-11-43 -0700.csv", 
                        col_types = cols(`Button Down` = col_skip(), 
                                         `Button Up` = col_skip(), 
                                         `Date Time, GMT -0700` = col_character(), 
                                         EOF = col_skip(), 
                                         `Host Connect` = col_skip(), 
                                         Stopped = col_skip()), skip = 2)
luxtemp_034$trial <- "034"

luxtemp_035 <- read_csv("Data/2021/035 2021-08-27 09-08-39 -0700.csv", 
                        col_types = cols(`Button Down` = col_skip(), 
                                         `Button Up` = col_skip(), 
                                         `Date Time, GMT -0700` = col_character(), 
                                         EOF = col_skip(), 
                                         `Host Connect` = col_skip(), 
                                         Stopped = col_skip()), skip = 2)
luxtemp_035$trial <- "035"

luxtemp_036 <- read_csv("Data/2021/036 2021-08-27 11-06-51 -0700.csv", 
                        col_types = cols(`Button Down` = col_skip(), 
                                         `Button Up` = col_skip(), 
                                         `Date Time, GMT -0700` = col_character(), 
                                         EOF = col_skip(), 
                                         `Host Connect` = col_skip(), 
                                         Stopped = col_skip()), skip = 2)
luxtemp_036$trial <- "036"

luxtemp_037 <- read_csv("Data/2021/037 2021-08-28 09-28-22 -0700.csv", 
                        col_types = cols(`Button Down` = col_skip(), 
                                         `Button Up` = col_skip(), 
                                         `Date Time, GMT -0700` = col_character(), 
                                         EOF = col_skip(), 
                                         `Host Connect` = col_skip(), 
                                         Stopped = col_skip()), skip = 2)
luxtemp_037$trial <- "037"

luxtemp_038 <- read_csv("Data/2021/038 2021-08-28 11-18-39 -0700.csv", 
                        col_types = cols(`Button Down` = col_skip(), 
                                         `Button Up` = col_skip(), 
                                         `Date Time, GMT -0700` = col_character(), 
                                         EOF = col_skip(), 
                                         `Host Connect` = col_skip(), 
                                         Stopped = col_skip()), skip = 2)
luxtemp_038$trial <- "038"

luxtemp_039 <- read_csv("Data/2021/039 2021-08-28 13-45-18 -0700.csv", 
                        col_types = cols(`Button Down` = col_skip(), 
                                         `Button Up` = col_skip(), 
                                         `Date Time, GMT -0700` = col_character(), 
                                         EOF = col_skip(), 
                                         `Host Connect` = col_skip(), 
                                         Stopped = col_skip()), skip = 2)
luxtemp_039$trial <- "039"

luxtemp_040 <- read_csv("Data/2021/040 2021-08-28 16-20-25 -0700.csv", 
                        col_types = cols(`Button Down` = col_skip(), 
                                         `Button Up` = col_skip(), 
                                         `Date Time, GMT -0700` = col_character(), 
                                         EOF = col_skip(), 
                                         `Host Connect` = col_skip(), 
                                         Stopped = col_skip()), skip = 2)
luxtemp_040$trial <- "040"

luxtemp_041 <- read_csv("Data/2021/041 2021-08-29 09-36-02 -0700.csv", 
                        col_types = cols(`Button Down` = col_skip(), 
                                         `Button Up` = col_skip(), 
                                         `Date Time, GMT -0700` = col_character(), 
                                         EOF = col_skip(), 
                                         `Host Connect` = col_skip(), 
                                         Stopped = col_skip()), skip = 2)
luxtemp_041$trial <- "041"

luxtemp_042 <- read_csv("Data/2021/042 2021-08-29 11-58-23 -0700.csv", 
                        col_types = cols(`Button Down` = col_skip(), 
                                         `Button Up` = col_skip(), 
                                         `Date Time, GMT -0700` = col_character(), 
                                         EOF = col_skip(), 
                                         `Host Connect` = col_skip(), 
                                         Stopped = col_skip()), skip = 2)
luxtemp_042$trial <- "042"

luxtemp_043 <- read_csv("Data/2021/043 2021-08-29 13-50-29 -0700.csv", 
                        col_types = cols(`Button Down` = col_skip(), 
                                         `Button Up` = col_skip(), 
                                         `Date Time, GMT -0700` = col_character(), 
                                         EOF = col_skip(), 
                                         `Host Connect` = col_skip(), 
                                         Stopped = col_skip()), skip = 2)
luxtemp_043$trial <- "043"

luxtemp_044 <- read_csv("Data/2021/044 2021-08-29 16-03-49 -0700.csv", 
                        col_types = cols(`Button Down` = col_skip(), 
                                         `Button Up` = col_skip(), 
                                         `Date Time, GMT -0700` = col_character(), 
                                         EOF = col_skip(), 
                                         `Host Connect` = col_skip(), 
                                         Stopped = col_skip()), skip = 2)
luxtemp_044$trial <- "044"

luxtemp_045 <- read_csv("Data/2021/045 2021-08-30 09-23-27 -0700.csv", 
                        col_types = cols(`Button Down` = col_skip(), 
                                         `Button Up` = col_skip(), 
                                         `Date Time, GMT -0700` = col_character(), 
                                         EOF = col_skip(), 
                                         `Host Connect` = col_skip(), 
                                         Stopped = col_skip()), skip = 2)
luxtemp_045$trial <- "045"

luxtemp_046 <- read_csv("Data/2021/046 2021-08-30 11-17-58 -0700.csv", 
                        col_types = cols(`Button Down` = col_skip(), 
                                         `Button Up` = col_skip(), 
                                         `Date Time, GMT -0700` = col_character(), 
                                         EOF = col_skip(), 
                                         `Host Connect` = col_skip(), 
                                         Stopped = col_skip()), skip = 2)
luxtemp_046$trial <- "046"

luxtemp_047 <- read_csv("Data/2021/047 2021-08-30 13-07-32 -0700.csv", 
                        col_types = cols(`Button Down` = col_skip(), 
                                         `Button Up` = col_skip(), 
                                         `Date Time, GMT -0700` = col_character(), 
                                         EOF = col_skip(), 
                                         `Host Connect` = col_skip(), 
                                         Stopped = col_skip()), skip = 2)
luxtemp_047$trial <- "047"

luxtemp_048 <- read_csv("Data/2021/048 2021-08-30 15-22-15 -0700.csv", 
                        col_types = cols(`Button Down` = col_skip(), 
                                         `Button Up` = col_skip(), 
                                         `Date Time, GMT -0700` = col_character(), 
                                         EOF = col_skip(), 
                                         `Host Connect` = col_skip(), 
                                         Stopped = col_skip()), skip = 2)
luxtemp_048$trial <- "048"

luxtemp_049 <- read_csv("Data/2021/049 2021-09-04 11-53-53 -0700.csv", 
                        col_types = cols(`Button Down` = col_skip(), 
                                         `Button Up` = col_skip(), 
                                         `Date Time, GMT -0700` = col_character(), 
                                         EOF = col_skip(), 
                                         `Host Connect` = col_skip(), 
                                         Stopped = col_skip()), skip = 2)
luxtemp_049$trial <- "049"

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# MANIPULATE DATA                                                              ####
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

all_luxtemp <- bind_rows(list(luxtemp_001, 
                           luxtemp_002,
                           luxtemp_003,
                           luxtemp_004,
                           luxtemp_005,
                           luxtemp_006,
                           luxtemp_007,
                           luxtemp_008,
                           luxtemp_009,
                           luxtemp_010,
                           luxtemp_011x,
                           luxtemp_012,
                           luxtemp_013,
                           luxtemp_014,
                           luxtemp_015,
                           luxtemp_016,
                           luxtemp_017,
                           luxtemp_018,
                           luxtemp_019,
                           luxtemp_020,
                           luxtemp_021,
                           luxtemp_022,
                           luxtemp_023,
                           luxtemp_024,
                           luxtemp_025,
                           luxtemp_026,
                           luxtemp_027,
                           luxtemp_028,
                           luxtemp_029,
                           luxtemp_030,
                           luxtemp_031,
                           luxtemp_032,
                           luxtemp_033,
                           luxtemp_034,
                           luxtemp_035,
                           luxtemp_036,
                           luxtemp_037,
                           luxtemp_038,
                           luxtemp_039,
                           luxtemp_040,
                           luxtemp_041,
                           luxtemp_042,
                           luxtemp_043,
                           luxtemp_044,
                           luxtemp_045,
                           luxtemp_046,
                           luxtemp_047,
                           luxtemp_048,
                           luxtemp_049))

# separate times from dates in new column
all_luxtemp_joined <- all_luxtemp %>%
  separate(`Date Time, GMT -0700`, sep = " ",
           into = c("date", "time"))
# remove seconds
all_luxtemp_joined$time <- str_sub(all_luxtemp_joined$time, start = 1L, end = -4L)

# import trials times to parse out non-trial readings

trial_times <- read_csv("Data/2021/trials2021_QAQC.csv",
                        col_types = cols(`timeBegin` = col_character(), 
                                         `timeEnd` = col_character()) )
trial_times <- trial_times %>%
  select(trial, timeBegin, timeEnd) %>%
  distinct(trial, timeBegin, timeEnd)

# join trials times with lux temp readings
time_luxtemp <- all_luxtemp_joined %>%
  left_join(trial_times, by = "trial")

# subset to readings only taken during trials
subset_luxtemp <- time_luxtemp %>%
  group_by(trial) %>%
  filter(time >= timeBegin, time <= timeEnd)

# identify mean, variation, max, for each trial)
luxtemp_summary <- subset_luxtemp %>%
  group_by(trial) %>%
  summarise(mean(`Temp, (*C)`),
            sd(`Temp, (*C)`),
            mean(`Intensity, ( lux)`),
            sd(`Intensity, ( lux)`),
            max(`Intensity, ( lux)`))

luxtemp_summary <- luxtemp_summary %>%
  rename("mean_temp_C" = "mean(`Temp, (*C)`)",
         "sd_temp_C" = "sd(`Temp, (*C)`)",
         "mean_lux" = "mean(`Intensity, ( lux)`)",
         "sd_lux" = "sd(`Intensity, ( lux)`)",
         "max_lux" = "max(`Intensity, ( lux)`)")

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# EXPORT DATA                                                                  ####
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

write_csv(luxtemp_summary, "Data/2021/luxtemp_QAQC.csv")

############### SUBSECTION HERE

####
#<<<<<<<<<<<<<<<<<<<<<<<<<<END OF SCRIPT>>>>>>>>>>>>>>>>>>>>>>>>#

# SCRATCH PAD ####