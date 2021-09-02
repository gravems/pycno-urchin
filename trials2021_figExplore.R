#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#                                                                                ##
# Pycno-Urchin Dataset 2021 Figure Exploration                                   ##
# Script created 2021-08-31                                                      ##
# Last updated 2021-08-31                                                        ##
# Data source: Ross Whippo                                                       ##
# R code prepared by Ross Whippo                                                 ##
#                                                                                ##
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# SUMMARY:
# A script to QAQC and parse the 2021 pycno-urchin dataset.


# Required Files (check that script is loading latest version):
# trials2021_QAQC.csv

# Associated Scripts:
# trials2021_dataset_QAQC.R
# trials2021_model1.R

# TO DO 

# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# TABLE OF CONTENTS                                                            ####
#                                                                                 +
# RECENT CHANGES TO SCRIPT                                                        +
# LOAD PACKAGES                                                                   +
# READ IN DATA                                                                    +
# MANIPULATE DATA                                                                 +
#                                                                                 +
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# RECENT CHANGES TO SCRIPT                                                     ####
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# 2021-08-31 Script created

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

# is there a difference in mean test diameter between groups?

diams <- trials2021_Q %>%
  select(trial, urchinGroup, urchinDiam_mm) %>%
  distinct(trial, urchinGroup, urchinDiam_mm)

diams %>%
  ggplot(aes(x = urchinGroup, y = urchinDiam_mm, fill = urchinGroup)) +
  geom_boxplot() +
  scale_fill_viridis(discrete = TRUE)

t.test(urchinDiam_mm ~ urchinGroup, data = diams)

# Welch Two Sample t-test

# data:  urchinDiam_mm by urchinGroup
# t = -1.5407, df = 43.008, p-value = 0.1307
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#   -5.6760725  0.7594058
# sample estimates:
#   mean in group starved     mean in group fed 
# 71.20833              73.66667 

# NO, 2.46mm average difference

# is there a difference in time spent moving among all treatments?

movement <- trials2021_Q %>%
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

interaction <- trials2021_Q %>%
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

location <- trials2021_Q %>%
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

# is there a difference in movement + location?

move_loc <- trials2021_Q %>%
  select(trial, urchinGroup, pycnoTreat, algalTreat, movement, location) %>%
  mutate(moves = case_when(movement == "ma" ~ "moving",
                           movement == "mp" ~ "moving",
                           movement == "mt" ~ "moving",
                           movement == "st" ~ "still")) %>%
  unite(mov_loc, moves, location, sep = "_") %>%
  add_column(place = 1) %>%
  group_by(trial, urchinGroup, pycnoTreat, algalTreat, mov_loc) %>%
  count(place) %>%
  mutate(time_spent = n/60) 
move_loc$mov_loc <- factor(move_loc$mov_loc, levels = c("moving_c", "still_c", "moving_m", "still_m", "moving_f", "still_f"))

move_loc %>%
  ggplot(aes(x = mov_loc, y = time_spent, fill = urchinGroup)) +
  geom_boxplot() +
  scale_fill_viridis(discrete = TRUE, begin = 0.3, end = 0.8) +
  facet_wrap(pycnoTreat ~ algalTreat)


############### SUBSECTION HERE

####
#<<<<<<<<<<<<<<<<<<<<<<<<<<END OF SCRIPT>>>>>>>>>>>>>>>>>>>>>>>>#

# SCRATCH PAD ####