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


# Associated Scripts:
# 

# TO DO 

# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# TABLE OF CONTENTS                                                            ####
#                                                                                 +
# RECENT CHANGES TO SCRIPT                                                        +
# LOAD PACKAGES                                                                   +
# READ IN                                                                         +
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


luxtemp_001 <- read_csv("Data/2021/001 2021-08-15 15-42-41 -0700.csv", 
                                          col_types = cols(`Button Down` = col_skip(), 
                                                           `Button Up` = col_skip(), 
                                                           `Date Time, GMT -0700` = col_character(), 
                                                           EOF = col_skip(), 
                                                           `Host Connect` = col_skip(), 
                                                           Stopped = col_skip()), skip = 2)


luxtemp_002 <- read_csv("Data/2021/002 2021-08-16 10-03-40 -0700.csv", 
                        col_types = cols(`Button Down` = col_skip(), 
                                         `Button Up` = col_skip(), 
                                         `Date Time, GMT -0700` = col_character(), 
                                         EOF = col_skip(), 
                                         `Host Connect` = col_skip(), 
                                         Stopped = col_skip()), skip = 2)

luxtemp_003 <- read_csv("Data/2021/003 2021-08-16 14-51-11 -0700.csv", 
                        col_types = cols(`Button Down` = col_skip(), 
                                         `Button Up` = col_skip(), 
                                         `Date Time, GMT -0700` = col_character(), 
                                         EOF = col_skip(), 
                                         `Host Connect` = col_skip(), 
                                         Stopped = col_skip()), skip = 2)

luxtemp_004 <- read_csv("Data/2021/004 2021-08-16 17-18-06 -0700.csv", 
                        col_types = cols(`Button Down` = col_skip(), 
                                         `Button Up` = col_skip(), 
                                         `Date Time, GMT -0700` = col_character(), 
                                         EOF = col_skip(), 
                                         `Host Connect` = col_skip(), 
                                         Stopped = col_skip()), skip = 2)

luxtemp_005 <- read_csv("Data/2021/005 2021-08-17 10-28-02 -0700.csv", 
                        col_types = cols(`Button Down` = col_skip(), 
                                         `Button Up` = col_skip(), 
                                         `Date Time, GMT -0700` = col_character(), 
                                         EOF = col_skip(), 
                                         `Host Connect` = col_skip(), 
                                         Stopped = col_skip()), skip = 2)

luxtemp_006 <- read_csv("Data/2021/006 2021-08-17 13-11-23 -0700.csv", 
                        col_types = cols(`Button Down` = col_skip(), 
                                         `Button Up` = col_skip(), 
                                         `Date Time, GMT -0700` = col_character(), 
                                         EOF = col_skip(), 
                                         `Host Connect` = col_skip(), 
                                         Stopped = col_skip()), skip = 2)

luxtemp_007 <- read_csv("Data/2021/007 2021-08-17 15-38-50 -0700.csv", 
                        col_types = cols(`Button Down` = col_skip(), 
                                         `Button Up` = col_skip(), 
                                         `Date Time, GMT -0700` = col_character(), 
                                         EOF = col_skip(), 
                                         `Host Connect` = col_skip(), 
                                         Stopped = col_skip()), skip = 2)

luxtemp_008 <- read_csv("Data/2021/008 2021-08-17 17-33-46 -0700.csv", 
                        col_types = cols(`Button Down` = col_skip(), 
                                         `Button Up` = col_skip(), 
                                         `Date Time, GMT -0700` = col_character(), 
                                         EOF = col_skip(), 
                                         `Host Connect` = col_skip(), 
                                         Stopped = col_skip()), skip = 2)

luxtemp_009
luxtemp_010
luxtemp_011x
luxtemp_012
luxtemp_013
luxtemp_014
luxtemp_015
luxtemp_016
luxtemp_017
luxtemp_018
luxtemp_019
luxtemp_020
luxtemp_021
luxtemp_022
luxtemp_023
luxtemp_024
luxtemp_025
luxtemp_026
luxtemp_027
luxtemp_028
luxtemp_029
luxtemp_030
luxtemp_031
luxtemp_032
luxtemp_033
luxtemp_034
luxtemp_035
luxtemp_036
luxtemp_037
luxtemp_038
luxtemp_039
luxtemp_040
luxtemp_041
luxtemp_042
luxtemp_043
luxtemp_044
luxtemp_045
luxtemp_046
luxtemp_047
luxtemp_048

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# MANIPULATE DATA                                                              ####
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++



#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# EXPORT DATA                                                                  ####
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# write_csv(trials2021, "Data/2021/trials2021_QAQC.csv")

############### SUBSECTION HERE

####
#<<<<<<<<<<<<<<<<<<<<<<<<<<END OF SCRIPT>>>>>>>>>>>>>>>>>>>>>>>>#

# SCRATCH PAD ####