#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#                                                                                ##
# Pycno-Urchin Dataset 2021 QAQC                                                 ##
# Data are current as of 2021-08-19                                              ##
# Data source: Ross Whippo                                                       ##
# R code prepared by Ross Whippo                                                 ##
# Last updated 2021-08-19                                                        ##
#                                                                                ##
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# SUMMARY:
# A script to QAQC and parse the 2021 pycno-urchin dataset.


# Required Files (check that script is loading latest version):
# trials2021.csv

# Associated Scripts:
# FILE.R

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
library(viridis)

# set plot theme
theme_set(theme_classic())

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




# is there a difference in mean test diameter between groups?

trials2021 %>%
  ggplot(aes(x = urchinGroup, y = urchinDiam_mm, fill = urchinGroup)) +
  geom_boxplot() +
  scale_fill_viridis(discrete = TRUE)

t.test(urchinDiam_mm ~ urchinGroup, data = trials2021)
  
# Output:

# Welch Two Sample t-test

# data:  urchinDiam_mm by urchinGroup
# t = -8.7964, df = 1161.6, p-value < 2.2e-16
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#   -3.567221 -2.266112
# sample estimates:
# mean in group starved     mean in group fed 
# 71.16667              74.08333 

# yes, by 4mm

# is there a difference in time spent moving among all treatments?

movement <- trials2021 %>%
  select(trial, urchinGroup, pycnoTreat, algalTreat, movement) %>%
  filter(movement != "st") %>%
  add_column(move = 1) %>%
  group_by(trial, urchinGroup, pycnoTreat, algalTreat, move) %>%
  count(move) %>%
  mutate(time_moving = n/60)

movement %>%
  ggplot(aes(x = urchinGroup, y = time_moving, fill = urchinGroup)) +
  geom_boxplot() +
  scale_fill_viridis(discrete = TRUE, begin = 0.3, end = 0.8) +
  facet_grid(pycnoTreat ~ algalTreat)

# do treatments spend different amounts of time interacting with the signal?

interaction <- trials2021 %>%
  select(trial, urchinGroup, pycnoTreat, algalTreat, interaction) %>%
  filter(interaction != "ni") %>%
  add_column(interact = 1) %>%
  group_by(trial, urchinGroup, pycnoTreat, algalTreat, interact) %>%
  count(interact) %>%
  mutate(time_interacting = n/60)

interaction %>%
  ggplot(aes(x = urchinGroup, y = time_interacting, fill = urchinGroup)) +
  geom_boxplot() +
  scale_fill_viridis(discrete = TRUE, begin = 0.3, end = 0.8) +
  facet_grid(pycnoTreat ~ algalTreat)

# do urchin spend different amounts of time in distance zones among treatments?

location <- trials2021 %>%
  select(trial, urchinGroup, pycnoTreat, algalTreat, location) %>%
  add_column(locate = 1) %>%
  group_by(trial, urchinGroup, pycnoTreat, algalTreat, location) %>%
  count(locate) %>%
  mutate(time_spent = n/60)
location$location <- factor(location$location, levels = c("c", "m", "f"))
  
location %>%
  ggplot(aes(x = location, y = time_spent, fill = urchinGroup)) +
  geom_boxplot() +
  scale_fill_viridis(discrete = TRUE, begin = 0.3, end = 0.8) +
  facet_grid(pycnoTreat ~ algalTreat)



############### SUBSECTION HERE

####
#<<<<<<<<<<<<<<<<<<<<<<<<<<END OF SCRIPT>>>>>>>>>>>>>>>>>>>>>>>>#

# SCRATCH PAD ####