#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#                                                                                ##
# Pycno-Urchin Dataset 2021 Figure Exploration                                   ##
# Script created 2021-08-31                                                      ##
# Last updated 2021-09-04                                                        ##
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
# t = -1.7371, df = 44.656, p-value = 0.08927
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#  -5.9320892  0.4387559
# sample estimates:
#  mean in group starved     mean in group fed 
# 70.92000              73.66667 

# NO, 2.75mm average difference 

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

# FHL Talk Figure (starved urchins always moved less)

movement %>%
  mutate(pycnoTreat = ifelse(pycnoTreat == 'pycno', "Pycno", "No Pycno")) %>%
  mutate(algalTreat = ifelse(algalTreat == 'nereo', "Algae", "No Algae")) %>%
  mutate(`Urchin Treatment` = urchinGroup) %>%
  unite("Treatment", pycnoTreat:algalTreat, sep = " + ") %>%
  mutate(`Percent of time moving` = time_moving*100) %>%
  ggplot(aes(x = Treatment, y = `Percent of time moving`, color = `Urchin Treatment`)) +
  scale_color_viridis(discrete = TRUE,
                      begin = 0.3,
                      end = 0.7,
                      option = "viridis") +
    stat_summary(
      geom = "point",
      fun = "mean",
      size = 4,
      shape = 19
    ) +
    geom_errorbar(stat="summary", fun.data="mean_se", size = 1) +
  scale_y_continuous(limits = c(0,100)) +
  theme_minimal(base_size = 15) +
  theme(legend.position = "top") 


# WSN Figure, urchin movement (used w/ treatment table icons)
movement %>%
  mutate(pycnoTreat = ifelse(pycnoTreat == 'pycno', "Pycno", "No Pycno")) %>%
  mutate(algalTreat = ifelse(algalTreat == 'nereo', "Algae", "No Algae")) %>%
  mutate(`Urchin Treatment` = urchinGroup) %>%
  unite("Treatment", urchinGroup:algalTreat, sep = " + ") %>%
  mutate(Treatment = as.factor(Treatment)) %>%
  mutate(Treatment =  factor(Treatment, levels = c("starved + No Pycno + No Algae",
                                                   "starved + No Pycno + Algae",
                                                   "starved + Pycno + No Algae",
                                                   "starved + Pycno + Algae",
                                                   "fed + No Pycno + No Algae",
                                                   "fed + No Pycno + Algae",
                                                   "fed + Pycno + No Algae",
                                                   "fed + Pycno + Algae"))) %>%
  mutate(`Percent of time moving` = time_moving*100) %>%
  ggplot(aes(x = `Percent of time moving`, y = Treatment)) + 
  stat_summary(
    geom = "point",
    fun = "mean",
    size = 4,
    shape = 19
  ) +
  geom_errorbar(stat="summary", fun.data="mean_se", size = 1) +
  theme_minimal()
  


urchin_timeseries %>%
  group_by(pycno, ID) %>%
  mutate(cc = cumsum(consumed) * 0.339) %>%
  mutate(Treatment = ifelse(pycno == 'yes', "Pycno", "Control")) %>%
  ggplot(aes(x = hours, y = cc, color = Treatment)) +
  scale_color_viridis(discrete = TRUE,
                      begin = 0.3,
                      end = 0.7,
                      option = "magma") +
  geom_line(aes(group = ID), alpha = 0.3) +
  stat_summary(
    geom = "point",
    fun = "mean",
    size = 4,
    shape = 19
  ) +
  geom_errorbar(stat="summary", fun.data="mean_se", size = 1) +
  scale_y_continuous(name = "Amount of kelp consumed (g)") + 
  theme_minimal(base_size = 15) +
  theme(legend.position = "top") 




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

# FHL Talk Figure

interaction %>%
  mutate(pycnoTreat = ifelse(pycnoTreat == 'pycno', "Pycno", "No Pycno")) %>%
  mutate(algalTreat = ifelse(algalTreat == 'nereo', "Algae", "No Algae")) %>%
  mutate(`Urchin Treatment` = urchinGroup) %>%
  unite("Treatment", pycnoTreat:algalTreat, sep = " + ") %>%
  filter(Treatment %in% c("No Pycno + Algae", "Pycno + Algae")) %>%
  mutate(`Percent of time interacting` = time_interacting*100) %>%
  ggplot(aes(x = Treatment, y = `Percent of time interacting`, color = `Urchin Treatment`)) +
  scale_color_viridis(discrete = TRUE,
                      begin = 0.3,
                      end = 0.7,
                      option = "viridis") +
  stat_summary(
    geom = "point",
    fun = "mean",
    size = 4,
    shape = 19
  ) +
  geom_errorbar(stat="summary", fun.data="mean_se", size = 1) +
  scale_y_continuous(limits = c(0,100)) +
  theme_minimal(base_size = 15) +
  theme(legend.position = "top") 

# WSN percent interaction figure
interaction %>%
  mutate(pycnoTreat = ifelse(pycnoTreat == 'pycno', "Pycno", "No Pycno")) %>%
  mutate(algalTreat = ifelse(algalTreat == 'nereo', "Algae", "No Algae")) %>%
  mutate(`Urchin Treatment` = urchinGroup) %>%
  filter(algalTreat == "Algae") %>%
  unite("Treatment", urchinGroup:algalTreat, sep = " + ") %>%
  mutate(`Percent of time interacting` = time_interacting*100) %>%
  mutate(Treatment =  factor(Treatment, levels = c("starved + No Pycno + Algae",
                                                   "starved + Pycno + Algae",
                                                   "fed + No Pycno + Algae",
                                                   "fed + Pycno + Algae"))) %>%
  ggplot(aes(x = `Percent of time interacting`, y = Treatment)) +
 stat_summary(
    geom = "point",
    fun = "mean",
    size = 4,
    shape = 19
  ) +
  geom_errorbar(stat="summary", fun.data="mean_se", size = 1) +
  scale_x_continuous(limits = c(0,100)) +
  theme_minimal(base_size = 15) +
  theme(legend.position = "top") 


# percent time urchins spent interacting
interact_percent <- interaction %>%
  unite(treats, algalTreat, pycnoTreat, sep = "_") %>%
  filter(treats == 'nereo_pycno') %>%
  group_by(urchinGroup) %>%
  summarise(mean(time_interacting)) 
  

movement %>%
  mutate(pycnoTreat = ifelse(pycnoTreat == 'pycno', "Pycno", "No Pycno")) %>%
  mutate(algalTreat = ifelse(algalTreat == 'nereo', "Algae", "No Algae")) %>%
  mutate(`Urchin Treatment` = urchinGroup) %>%
  unite("Treatment", pycnoTreat:algalTreat, sep = " + ") %>%
  mutate(`Percent of time moving` = time_moving*100) %>%
  ggplot(aes(x = Treatment, y = `Percent of time moving`, color = `Urchin Treatment`)) +
  scale_color_viridis(discrete = TRUE,
                      begin = 0.3,
                      end = 0.7,
                      option = "viridis") +
  stat_summary(
    geom = "point",
    fun = "mean",
    size = 4,
    shape = 19
  ) +
  geom_errorbar(stat="summary", fun.data="mean_se", size = 1) +
  scale_y_continuous(limits = c(0,100)) +
  theme_minimal(base_size = 15) +
  theme(legend.position = "top") 


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

# What does movment look like over the course of an hour for each treatment?

time_move <- trials2021_Q %>%
  select(trial, urchinGroup, pycnoTreat, algalTreat, timePoint_min, distance_cm) %>%
  group_by(trial) %>%
  unite(treatment, pycnoTreat, algalTreat, sep = "_") %>%
  mutate(treatment = factor(treatment))


time_move %>%
  ggplot(aes(x = timePoint_min, y = distance_cm, fill = treatment, group = treatment)) +
  geom_point() +
  geom_smooth(method = "loess") +
  scale_fill_viridis(discrete = TRUE) +
  facet_wrap(.~ urchinGroup)

# direct comparison to Belleza et al 2021 Figure 2

NMIOdata <- trials2021_Q %>%
  select(trial, urchinGroup, pycnoTreat, algalTreat, movement, location, interaction) %>%
#  mutate(still = movement) %>%
#  mutate(recode(still, 'st' = 1,
#                'ma' = 0,
#                'mt' = 0,
#                'mp' = 0), 
#         .keep = 'unused') %>%
#  rename('still' = 'recode(still, st = 1, ma = 0, mt = 0, mp = 0)') %>%
#  mutate(still = as.character(still)) %>%
  pivot_longer(cols = c(movement, location, interaction),
               names_to = "behavior",
               values_to = "value") %>%
  mutate(recode(value, 'i' = 1,
         'ma' = 1,
         'mt' = 1,
         'mp' = 1,
         'c' = 1,
         'm' = 1,
         'ni' = 0,
         'f' = 0,
         'st' = 0)) %>%
        # '0' = 0,
        # '1' = 1)) %>%
  rename('count' = 'recode(...)') %>%
  group_by(trial, urchinGroup, pycnoTreat, algalTreat, behavior) %>%
  summarise(sum(`count`)) %>%
  rename('count' = 'sum(count)') %>%
  unite('treatment', pycnoTreat, algalTreat, sep = "-")
  
# IRM Figure after existing movement literature

ggplot(NMIOdata, aes(x = behavior, y = count, fill = urchinGroup)) +
  geom_boxplot() +
  theme_linedraw() +
  scale_fill_viridis(discrete = TRUE, begin = 0.2, end = 0.8) +
  facet_grid(.~treatment) +
  labs(y = "minutes") + 
  scale_x_discrete(labels = c("interacting", "ROI", "moving"))

# reorganized IRM figure from grad student feedback

# New facet label names for treatment variable
treat_labs <- c(
  'control-control' = "control",
  'control-nereo' = "nereo",
  'pycno-control' = "pycno",
  'pycno-nereo' = "nereo + pycno")

# standard error function 
MinMeanSEMMax <- function(x) {
  v <- c(min(x), mean(x) - sd(x)/sqrt(length(x)), mean(x), mean(x) + sd(x)/sqrt(length(x)), max(x))
  names(v) <- c("ymin", "lower", "middle", "upper", "ymax")
  v
}

ggplot(NMIOdata, aes(y = count, fill = urchinGroup)) +
  geom_boxplot() +
  theme_linedraw() +
  theme(axis.ticks.x = element_blank(),
        axis.text.x = element_blank()) +
  scale_fill_viridis(discrete = TRUE, begin = 0.2, end = 0.8) +
  facet_grid(behavior~treatment, labeller = labeller(treatment = treat_labs)) +
  labs(y = "percent time") + 
  scale_y_continuous(breaks = seq(0, 60, len = 3), labels = function(x) paste0((x/60)*100, "%")) 
  

# numerical values

# algae only interaction times

NMIOdata %>%
  filter(behavior == "interaction") %>%
  filter(treatment == "control-nereo") %>%
  group_by(urchinGroup) %>%
  summarise(mean(count), median(count), sd(count))
NMIOdata %>%
  filter(behavior == "interaction") %>%
  filter(treatment == "control-nereo") %>%
  group_by(urchinGroup) %>%
  summarise(sd(count)/sqrt(length(count)))

# algae pycno interaction times

NMIOdata %>%
  filter(behavior == "interaction") %>%
  filter(treatment == "pycno-nereo") %>%
  group_by(urchinGroup) %>%
  summarise(mean(count), median(count), sd(count))
NMIOdata %>%
  filter(behavior == "interaction") %>%
  filter(treatment == "pycno-nereo") %>%
  group_by(urchinGroup) %>%
  summarise(sd(count)/sqrt(length(count)))
  
# time spent in ROI algae only

NMIOdata %>%
  filter(behavior == "location") %>%
  filter(treatment == "control-nereo") %>%
  group_by(urchinGroup) %>%
  summarise(mean(count), median(count), sd(count))
NMIOdata %>%
  filter(behavior == "location") %>%
  filter(treatment == "control-nereo") %>%
  group_by(urchinGroup) %>%
  summarise(sd(count)/sqrt(length(count)))

# time spent in ROI algae and pycno

NMIOdata %>%
  filter(behavior == "location") %>%
  filter(treatment == "pycno-nereo") %>%
  group_by(urchinGroup) %>%
  summarise(mean(count), median(count), sd(count))
NMIOdata %>%
  filter(behavior == "location") %>%
  filter(treatment == "pycno-nereo") %>%
  group_by(urchinGroup) %>%
  summarise(sd(count)/sqrt(length(count)))

# time spent moving algae and pycno

NMIOdata %>%
  filter(behavior == "movement") %>%
  filter(treatment == "pycno-nereo") %>%
  group_by(urchinGroup) %>%
  summarise(mean(count), median(count), sd(count))
NMIOdata %>%
  filter(behavior == "movement") %>%
  filter(treatment == "pycno-nereo") %>%
  group_by(urchinGroup) %>%
  summarise(sd(count)/sqrt(length(count)))


  ############### SUBSECTION HERE

####
#<<<<<<<<<<<<<<<<<<<<<<<<<<END OF SCRIPT>>>>>>>>>>>>>>>>>>>>>>>>#

# SCRATCH PAD ####

test <- movement %>%
  mutate(pycnoTreat = ifelse(pycnoTreat == 'pycno', "Pycno", "No Pycno")) %>%
  mutate(algalTreat = ifelse(algalTreat == 'nereo', "Algae", "No Algae")) %>%
  mutate(`Urchin Treatment` = urchinGroup) %>%
  unite("Treatment", urchinGroup:algalTreat, sep = " + ") %>%
  mutate(Treatment = as.factor(Treatment)) %>%
  mutate(Treatment =  factor(Treatment, levels = c("fed + Pycno + Algae",
                                         "fed + Pycno + No Algae",
                                         "fed + No Pycno + Algae",
                                         "fed + No Pycno + No Algae",
                                         "starved + Pycno + Algae",
                                         "starved + Pycno + No Algae",
                                         "starved + No Pycno + Algae",
                                         "starved + No Pycno + No Algae")))


str(test)
