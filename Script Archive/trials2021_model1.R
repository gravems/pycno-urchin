#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#                                                                                ##
# Pycno-Urchin Dataset 2021 Model                                                ##
# Script created 2021-09-01                                                      ##
# Last updated 2022-03-03                                                        ##
# Data source: Ross Whippo                                                       ##
# R code prepared by Ross Whippo                                                 ##
#                                                                                ##
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# SUMMARY:
# A script to build a statistical model for 2021 pycno-urchin dataset.


# Required Files (check that script is loading latest version):
# trials2021_QAQC.csv

# Associated Scripts:
# trials2021_dataset_QAQC.R
# trials2021_figExplore.R

# TO DO 

# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# TABLE OF CONTENTS                                                            ####
#                                                                                 +
# RECENT CHANGES TO SCRIPT                                                        +
# LOAD PACKAGES                                                                   +
# READ IN AND PREPARE DATA                                                        +
# MODEL 1                                                                         +
#                                                                                 +
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# RECENT CHANGES TO SCRIPT                                                     ####
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# 2021-09-01 Script created
# 2022-02-10 started analyses suggested by Sarah Gravem
# 2022-03-02 Added models and figure for interaction

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# LOAD PACKAGES                                                                ####
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

library(tidyverse)
library(viridis)
library(lme4)
library(ggpubr)
library(systemfonts)

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

algalconsumption <- read_csv("Data/2021/algal_consumption.csv") # area of algae consumed

confetti_weights <- read_csv("Data/2020/confetti_weights.csv") # last year confetti weights

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# MANIPULATE DATA                                                              ####
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# NOTES: run all models with tank & date as random factors first, if they are not significant,
# remove them
# break up data into 5 minute increments, create column of time (5, 10, 15, etc),
# to make it a continuous variable.
# models to be made:
#   1. proportion of time interacting - binomial glm
#   2. proportion of time spines moving - binomial glm
#   3. average distance from signal - (lmer for rand effects) linear if normal, use Poisson dist
#   4. cumulative distance traveled - (lmer for rand effects) linear if normal, use Poisson dist
#   5. movement categories: 0 = mt, 0.5 = mp, 0.5 = st, 1 = ma - binomial glm
#   6. amount eaten (descriptive visualizations)

model_1_dat <- trials2021_Q
model_1_dat <- model_1_dat %>%
  select(trial, date, tank, urchinGroup, pycnoTreat, algalTreat, interaction) %>%
  group_by(trial, date, tank, urchinGroup, pycnoTreat, algalTreat) %>%
  mutate(interacting = case_when(interaction == "ni" ~ 0,
                           interaction == "i" ~ 1)) 
model_1_dat_60 <- model_1_dat %>%
  summarise(interacting = mean(interacting))
model_1_datetank_60 <- glm(interacting ~ date + tank, family = binomial, data = model_1_dat_60)
summary(model_1_datetank_60)
model_1_60 <- glm(interacting ~ urchinGroup + trial + pycnoTreat*algalTreat, family = binomial, data = model_1_dat_60)
summary(model_1_60)

# is there a correlation between tank and one of the other factors?
# Create a table with the needed variables.
tank_dat = table(model_1_dat_60$urchinGroup, model_1_dat_60$tank) 
print(tank_dat)
# Perform the Chi-Square test.
print(chisq.test(tank_dat))
tank_dat = table(model_1_dat_60$pycnoTreat, model_1_dat_60$tank)
print(tank_dat)
tank_dat = table(model_1_dat_60$algalTreat, model_1_dat_60$tank)
print(tank_dat)


# 5 minute increments INTERACTION

model_1_dat <- trials2021_Q
model_1_dat <- model_1_dat %>%
  select(trial, date, tank, urchinGroup, pycnoTreat, algalTreat, interaction) %>%
  group_by(trial, date, tank, urchinGroup, pycnoTreat, algalTreat) %>%
  mutate(interacting = case_when(interaction == "ni" ~ 0,
                                 interaction == "i" ~ 1)) 

model_1_dat_5 <- model_1_dat %>%
  mutate(minutes = c(rep(5, 5), rep(10, 5), rep(15, 5), rep(20, 5), rep(25, 5), rep(30, 5), rep(35, 5), rep(40, 5), rep(45, 5), rep(50, 5), rep(55, 5), rep(60, 5))) %>%
  group_by(trial, date, tank, urchinGroup, pycnoTreat, algalTreat, minutes) %>%
  summarise(interacting = mean(interacting))
model_1_datetank_5 <- glm(interacting ~ date + tank, family = binomial, data = model_1_dat_5)
summary(model_1_datetank_5)
model_1_5 <- glm(interacting ~ urchinGroup + pycnoTreat + algalTreat + tank, family = binomial, data = model_1_dat_5)
summary(model_1_5)

# output to new csv for other scripts to access:
write_csv(model_1_dat_5, "model_1_dat_5.csv")

# figure of time spent interaction based on all the factors

theme_set(theme_light(base_size = 18))

model_1_plotdat_5 <- model_1_dat_5 %>%
  unite(Treatment, pycnoTreat, algalTreat, sep = "/" ) %>%
  mutate(Treatment = case_when(Treatment == "pycno/control" ~ "Pycno",
                            Treatment == "control/nereo" ~ "Nereo",
                            Treatment == "control/control" ~ "Control",
                            Treatment == "pycno/nereo" ~ "Pycno/Nereo")) 
treatMeans <- model_1_plotdat_5 %>%
  group_by(Treatment, urchinGroup) %>%
  summarise(treatMean = mean(interacting))

model_1_plotdat_5 <- model_1_plotdat_5 %>%
  left_join(treatMeans)
  
theme_set(theme_light(base_size = 18))

control_avg <- 
  model_1_plotdat_5 %>%
  ungroup() %>%
  filter(Treatment == "Control") %>%
  summarize(avg = mean(interacting)) %>%
  pull(avg)


plot_interact <- model_1_plotdat_5 %>%
  ggplot(aes(y = interacting, x = Treatment, color = urchinGroup)) +
  geom_hline(aes(yintercept = control_avg), color = "gray70", size = 0.8, linetype = "dashed") +
  geom_jitter(position = position_jitter(seed = 227, width = 0.2), size = 2, alpha = 0.20) +
  stat_summary(fun = mean, geom = "point", size = 6.5, shape = 21, fill = "black", position = position_jitter(seed = 227, width = 0.2)) +
  stat_summary(fun = mean, geom = "point", size = 5, shape = 16, position = position_jitter(seed = 227, width = 0.2)) +
  scale_y_continuous(
    limits = c(-0.1, 1.1), expand = c(0.005, 0.005)) +
  coord_flip() +
  scale_color_viridis(discrete = TRUE, begin = 0.3, end = 0.9) +
  labs(color = "Urchin Group") +
  labs(y = "Proportion Time Interacting", x = NULL) +
  theme(
    axis.title = element_text(size = 16),
   axis.text.x = element_text(size = 12),
    panel.grid = element_blank()
  ) 
  


# 5 minute increments MOVING

model_1_dat <- trials2021_Q
model_1_dat <- model_1_dat %>%
  select(trial, date, tank, urchinGroup, pycnoTreat, algalTreat, movement) %>%
  group_by(trial, date, tank, urchinGroup, pycnoTreat, algalTreat) %>%
  mutate(movement = case_when(movement == "st" ~ 0,
                                 movement == "ma" ~ 1,
                                 movement == "mt" ~ 1,
                                 movement == "mp" ~ 1)) 

model_1_dat_5 <- model_1_dat %>%
  mutate(minutes = c(rep(5, 5), rep(10, 5), rep(15, 5), rep(20, 5), rep(25, 5), rep(30, 5), rep(35, 5), rep(40, 5), rep(45, 5), rep(50, 5), rep(55, 5), rep(60, 5))) %>%
  group_by(trial, date, tank, urchinGroup, pycnoTreat, algalTreat, minutes) %>%
  summarise(movement = mean(movement))
model_1_datetank_5 <- glm(movement ~ date + tank, family = binomial, data = model_1_dat_5)
summary(model_1_datetank_5)
model_1_5 <- glm(movement ~ urchinGroup + pycnoTreat + algalTreat + tank, family = binomial, data = model_1_dat_5)
summary(model_1_5)

# figure of time spent moving based on all the factors

theme_set(theme_light(base_size = 18))

model_1_plotdat_5 <- model_1_dat_5 %>%
  unite(Treatment, pycnoTreat, algalTreat, sep = "/" ) %>%
  mutate(Treatment = case_when(Treatment == "pycno/control" ~ "Pycno",
                               Treatment == "control/nereo" ~ "Nereo",
                               Treatment == "control/control" ~ "Control",
                               Treatment == "pycno/nereo" ~ "Pycno/Nereo")) 
treatMeans <- model_1_plotdat_5 %>%
  group_by(Treatment, urchinGroup) %>%
  summarise(treatMean = mean(movement))

model_1_plotdat_5 <- model_1_plotdat_5 %>%
  left_join(treatMeans)

theme_set(theme_light(base_size = 18))

control_avg <- 
  model_1_plotdat_5 %>%
  ungroup() %>%
  filter(Treatment == "Control") %>%
  summarize(avg = mean(movement)) %>%
  pull(avg)


plot_move <- model_1_plotdat_5 %>%
  ggplot(aes(y = movement, x = Treatment, color = urchinGroup)) +
  geom_hline(aes(yintercept = control_avg), color = "gray70", size = 0.6) +
  geom_jitter(position = position_jitter(seed = 227, width = 0.2), size = 2, alpha = 0.20) +
  stat_summary(fun = mean, geom = "point", size = 6.5, shape = 21, fill = "black", position = position_jitter(seed = 227, width = 0.2)) +
  stat_summary(fun = mean, geom = "point", size = 5, shape = 16, position = position_jitter(seed = 227, width = 0.2)) +
  scale_y_continuous(
    limits = c(-0.1, 1.1), expand = c(0.005, 0.005)) +
  coord_flip() +
  scale_color_viridis(discrete = TRUE, begin = 0.3, end = 0.8, option = "H") +
  labs(color = "Urchin Group") +
  labs(y = "Proportion Time Moving", x = NULL) +
  theme(
    axis.title = element_text(size = 16),
    axis.text.x = element_text(size = 12),
    panel.grid = element_blank()
  ) 



# is there a correlation between tank and one of the other factors?
# Create a table with the needed variables.
tank_dat = table(model_1_dat_5$algalTreat, model_1_dat_5$tank) 
print(tank_dat)
# Perform the Chi-Square test.
print(chisq.test(tank_dat))

tank_dat = table(model_1_dat_5$urchinGroup, model_1_dat_5$tank) 
print(tank_dat)
# Perform the Chi-Square test.
print(chisq.test(tank_dat))

# 10 minute increments

model_1_dat_10 <- model_1_dat %>%
  mutate(minutes = c(rep(10, 10), rep(20, 10), rep(30, 10), rep(40, 10), rep(50, 10), rep(60, 10))) %>%
  group_by(trial, date, tank, urchinGroup, pycnoTreat, algalTreat, minutes) %>%
  summarise(interacting = mean(interacting))
model_1_datetank_10 <- glm(interacting ~ date + tank, family = binomial, data = model_1_dat_10)
summary(model_1_datetank_10)
model_1_10 <- glm(interacting ~ tank + urchinGroup + pycnoTreat + algalTreat, family = binomial, data = model_1_dat_5)
summary(model_1_10)

# is there a correlation between tank and one of the other factors?
# Create a table with the needed variables.
tank_dat = table(model_1_dat_5$algalTreat, model_1_dat_5$tank) 
print(tank_dat)
# Perform the Chi-Square test.
print(chisq.test(tank_dat))


# create proportion columns for each behavior

model1_dat <- model_dat %>%
  select(trial, tank, `signalRate_ml-min`, beginTemp_C, endTemp_C, beginSal_ppt, endSal_ppt, urchinGroup, pycnoTreat, algalTreat, urchinDiam_mm, movement, location, spines, interaction) %>%
  filter(movement != "st") %>%
  add_column(move = 1) %>%
  group_by(trial, urchinGroup, pycnoTreat, algalTreat, move) %>%
  count(move) %>%
  mutate(time_moving = n/60)

# Did urchins in each group and treatment towards, away, or neither the most?

model_2_dat_5 <- trials2021_Q %>%
  group_by(trial, date, tank, urchinGroup, pycnoTreat, algalTreat) %>%
  select(trial, date, tank, urchinGroup, pycnoTreat, algalTreat, movement) %>%
  mutate(minutes = c(rep(5, 5), rep(10, 5), rep(15, 5), rep(20, 5), rep(25, 5), rep(30, 5), rep(35, 5), rep(40, 5), rep(45, 5), rep(50, 5), rep(55, 5), rep(60, 5))) %>%
  group_by(trial, date, tank, urchinGroup, pycnoTreat, algalTreat, minutes) %>%
  mutate(moves = case_when(movement == "ma" ~ 1,
                           movement == "mp" ~ 0.5,
                           movement == "mt" ~ 0,
                           movement == "st" ~ 0.5)) %>%
  summarise(moves = mean(moves))

model_2_datetank_5 <- glm(moves ~ date + tank, family = binomial, data = model_2_dat_5)
summary(model_2_datetank_5)
#NO TANK OR DATE EFFECT

model_2_5 <- glm(moves ~ urchinGroup + pycnoTreat + algalTreat, family = binomial, data = model_2_dat_5)
summary(model_2_5)


# did urchins have a difference in average distance from the signal in the tank?

model_3_dat_5 <- trials2021_Q %>%
  group_by(trial, date, tank, urchinGroup, pycnoTreat, algalTreat) %>%
  select(trial, date, tank, urchinGroup, pycnoTreat, algalTreat, distance_cm) %>%
  mutate(minutes = c(rep(5, 5), rep(10, 5), rep(15, 5), rep(20, 5), rep(25, 5), rep(30, 5), rep(35, 5), rep(40, 5), rep(45, 5), rep(50, 5), rep(55, 5), rep(60, 5))) %>%
  group_by(trial, date, tank, urchinGroup, pycnoTreat, algalTreat, minutes) %>%
  summarise(dist = mean(distance_cm))

# testing for normality
library("ggpubr")
ggdensity(model_3_dat_5$dist, 
          main = "Density plot of dist",
          xlab = "distance_cm")
ggqqplot(model_3_dat_5$dist)
# NOT NORMAL

# transforming for normality ROOT
model_3_dat_5 <- model_3_dat_5 %>%
  mutate(dist_root = sqrt(dist +1))
# testing transformed for normality
library("ggpubr")
ggdensity(model_3_dat_5$dist_root, 
          main = "Density plot of dist",
          xlab = "distance (root+1)")
ggqqplot(model_3_dat_5$dist_root)
# NOT NORMAL

# transforming for normality LOG
model_3_dat_5 <- model_3_dat_5 %>%
  mutate(dist_log = log(dist +1))
# testing transformed for normality
library("ggpubr")
ggdensity(model_3_dat_5$dist_log, 
          main = "Density plot of dist",
          xlab = "distance (log+1)")
ggqqplot(model_3_dat_5$dist_log)
# NOT NORMAL

# transforming for normality LOG10
model_3_dat_5 <- model_3_dat_5 %>%
  mutate(dist_10 = log10(dist +1))
# testing transformed for normality
library("ggpubr")
ggdensity(model_3_dat_5$dist_10, 
          main = "Density plot of dist",
          xlab = "distance (log10+1)")
ggqqplot(model_3_dat_5$dist_10)
# NOT NORMAL

# transforming for normality CUBED ROOT
model_3_dat_5 <- model_3_dat_5 %>%
  mutate(dist_cube = 10^(log10(dist+1)/3))
# testing transformed for normality
library("ggpubr")
ggdensity(model_3_dat_5$dist_cube, 
          main = "Density plot of dist",
          xlab = "distance (cube root+1)")
ggqqplot(model_3_dat_5$dist_cube)
# NOT NORMAL

model_3_datetank_5 <- glm(dist ~ date + tank, family = binomial, data = model_2_dat_5)
summary(model_2_datetank_5)


model_2_5 <- glm(moves ~ urchinGroup + pycnoTreat + algalTreat, family = binomial, data = model_2_dat_5)
summary(model_2_5)





# Algal consumption plots

# based on field notes from 2020-09-13 - confetti diameter = 21mm

# > pi*((2.1/2)^2)
# [1] 3.463606

# > confetti_weights %>%
#  +   summarise(mean(weight_mg))
#  A tibble: 1 x 1
# `mean(weight_mg)`
# <dbl>
#   1              339.

# > 339/3.463606
# [1] 97.87487 mg per square cm

theme_set(theme_light(base_size = 18))

algae_biomass <- algalconsumption %>%
  mutate(biomass = eaten*97.87487) %>%
  mutate(pycnoTreat = case_when(pycnoTreat == "pycno" ~ "yes",
                            pycnoTreat == "control" ~ "no")) %>%
  rename(pycno = pycnoTreat)
  

algae_biomass %>%
  ggplot(aes(x = pycno, y = biomass, fill = pycno)) +
  geom_boxplot() +
  scale_fill_viridis(discrete = TRUE, begin  = 0.5, end = 0.9, option = "magma") +
  scale_y_continuous(name = "Biomass of Kelp Consumed (mg)")

algae_biomass %>%
  group_by(pycno) %>%
  summarize(mean(biomass))






############### SUBSECTION HERE

####
#<<<<<<<<<<<<<<<<<<<<<<<<<<END OF SCRIPT>>>>>>>>>>>>>>>>>>>>>>>>#

# SCRATCH PAD ####