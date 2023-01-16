#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#                                                                                ##
# Indirect Urchin MS Figures                                                     ##
# Script Created 2023-01-13                                                      ##
# Last updated 2023-01-16                                                        ##
# Data source: Ross Whippo PhD Dissertation                                      ##
# R code prepared by Ross Whippo                                                 ##
#                                                                                ##
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# SUMMARY:
# Script to create figures for manuscript of purple urchin-pycnopodia indirect
# interactions. Experiments run in summer of 2020 and 2021 at the Friday Harbor
# Laboratories by Ross Whippo, PhD candidate at University of Oregon. 

# Required Files (check that script is loading latest version):
# trials2020_QAQC.csv
# trials2021_QAQC.csv
# trials2021_trackerDist_QAQC.csv
# trials2021_trackerCoord_QAQC.csv


# Associated Scripts:
# trials2020_dataset_QAQC.R
# trials2021_dataset_QAQC.R
# tracker2021_dataset_QAQC.R

# TO DO 

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# TABLE OF CONTENTS                                                            ####
#                                                                                 +
# RECENT CHANGES TO SCRIPT                                                        +
# LOAD PACKAGES                                                                   +
# READ IN DATA                                                                    +
# 2020 URCHIN FEEDING                                                             +
# 2021 URCHIN MOVEMENT                                                            +
# 2021 TRACKER DATA                                                              +
#                                                                                 +
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# RECENT CHANGES TO SCRIPT                                                     ####
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# 2023-01-13 Script Created
# all 2020, 2021, and tracker data added

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# LOAD PACKAGES                                                                ####
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

library(tidyverse) # data manipulation, tidying
library(viridis) # color-blind-friendly palette
library(lmerTest) # lmer 
library(adehabitatLT) # tracker movement analyses

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# READ IN DATA                                                     ####
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

trials2020_Q <- read_csv("Data/2020/trials2020_QAQC.csv", 
                         col_types = cols(trial = col_character(), 
                                          bin = col_character(), diameter = col_double()))

trials2021_Q <- read_csv("Data/2021/trials2021_QAQC.csv", 
                         col_types = cols(algalTreat = col_factor(levels = c("nereo", "control")), 
                                          date = col_character(),  location = col_character(), 
                                          movement = col_character(), 
                                          pycnoTreat = col_factor(levels = c("pycno", "control")), 
                                          tank = col_factor(levels = c("TF1", "TF2")), 
                                          timeBegin = col_character(), timeEnd = col_character(), 
                                          urchinGroup = col_factor(levels = c("starved","fed")),
                                          location = col_factor(levels = c("c", "m", "f"))))

trials2021_trackerDist_QAQC <- read_csv("Data/2021/trials2021_trackerDist_QAQC.csv")

trials2021_trackerCoord_QAQC <- read_csv("Data/2021/trials2021_trackerCoord_QAQC.csv")


#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 2020 URCHIN FEEDING                                                          ####
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


# combine trial and bin to prevent combining urchins across trials
urchin_fear_pycno <-
  unite(trials2020_Q,
        'ID',
        trial,
        bin,
        sep = "_",
        remove = FALSE)

per_urchin_consumed_total <- urchin_fear_pycno %>%
  group_by(trial, bin, tank, ID, pycno) %>%
  summarise(`total consumed` = sum(consumed)) %>%
  ungroup()

# total consumed per urchin across all trials with standard error
per_urchin_consumed_total %>%
  mutate(`total consumed` = `total consumed` * 0.339) %>%
  mutate(Treatment = ifelse(pycno == 'yes', "Pycno", "Control")) %>%
  ggplot(aes(x = Treatment, y = `total consumed`, color = Treatment)) + 
  geom_point(col = "grey", size = 3) +
  scale_color_viridis(discrete = TRUE,
                      begin = 0.3,
                      end = 0.7,
                      option = "magma") +
  stat_summary(
    geom = "point",
    fun = "mean",
    size = 6,
    shape = 19
  ) +
  geom_errorbar(stat="summary", fun.data="mean_se", size = 1) +
  ylim(0, 30) +
  labs(y = "Kelp consumed (g)", x = "Treatment") +
  theme_minimal(base_size = 15) +
  theme(axis.title.y = element_text(vjust = 3)) +
  theme(legend.position = "none") +
  annotate("text", label = "p = 0.015", x = 2.3, y = 29, size = 5)

# timeseries per timepoint figure

urchin_timeseries <- urchin_fear_pycno %>%
  unite("datetime", date:time, sep = " ")

urchin_timeseries$datetime <-
  as.POSIXlt(urchin_timeseries$datetime)

# with scaled time points
urchin_timeseries$Hours <- urchin_timeseries$timepoint %>%
  recode(
    '0' = 0,
    '1' = 3,
    '2' = 9,
    '3' = 21,
    '4' = 27,
    '5' = 33,
    '6' = 45,
    '7' = 51,
    '8' = 57,
    '9' = 69
  )

urchin_timeseries_lm <- urchin_timeseries%>%
  group_by(pycno, ID) %>%
  mutate(cc = cumsum(consumed) * 0.339) %>%
  mutate(Treatment = ifelse(pycno == 'yes', "Pycno", "Control")) 

mod_2 <- lm(`cc` ~ pycno, data = urchin_timeseries_lm)
summary(mod_2)



# THIS IS THE CUMULATIVE CONFETTI FIGURE:

label1 <- paste("R^2 == ", round(summary(mod_2)$r.squared,3))

urchin_timeseries %>%
  group_by(pycno, ID) %>%
  mutate(cc = cumsum(consumed) * 0.339) %>%
  mutate(Treatment = ifelse(pycno == 'yes', "Pycno", "Control")) %>%
  ggplot(aes(x = Hours, y = cc, color = Treatment)) +
  scale_color_viridis(discrete = TRUE,
                      begin = 0.3,
                      end = 0.7,
                      option = "magma") +
  geom_line(aes(group = ID), alpha = 0.15) +
  geom_point(size = 2, position = "jitter", alpha = 0.15) +
  geom_smooth(method = "lm", size = 2) +
  scale_y_continuous(name = "Kelp consumed (g)") + 
  theme_minimal(base_size = 15) +
  theme(legend.position = "top") +
  annotate("text", label = label1, x = 10, y = 25, size = 5, parse = TRUE)


#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 2021 URCHIN MOVEMENT                                                         ####
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# moving

movement <- trials2021_Q %>%
  dplyr::select(trial, tank, urchinGroup, pycnoTreat, algalTreat, movement) %>%
  filter(movement != "st") %>%
  add_column(move = 1) %>%
  group_by(trial, urchinGroup, pycnoTreat, algalTreat, move, tank) %>%
  count(move) %>%
  mutate(time_moving = n/60)

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

#### Movement stats

summary(lmer(n ~ pycnoTreat + algalTreat + urchinGroup + (1 | tank), data = movement))

# interacting 

interaction <- trials2021_Q %>%
  dplyr::select(trial, tank, urchinGroup, pycnoTreat, algalTreat, interaction) %>%
  filter(interaction != "ni") %>%
  add_column(interact = 1) %>%
  group_by(trial, tank, urchinGroup, pycnoTreat, algalTreat, interact) %>%
  count(interact) %>%
  mutate(time_interacting = n/60)

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
  theme(legend.position = "top") +
  xlab(label = "Percent of time interacting with kelp")

# Interaction stats

# all treatments
summary(lmer(n ~ pycnoTreat + algalTreat + urchinGroup + (1 | tank), data = interaction))
# algae only
summary(lmer(n ~ pycnoTreat + urchinGroup + (1 | tank), data = filter(interaction, algalTreat == "nereo")))
# values of percent time spent interacting
interaction %>%
  group_by(urchinGroup) %>%
  summarise(mean(time_interacting))

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 2021 TRACKER DATA                                                            ####
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

tracker_all_ade <- as.ltraj(xy = trials2021_trackerCoord_QAQC[, c('x','y')], typeII = FALSE, 
                 id = trials2021_trackerCoord_QAQC$id)
plot(tracker_all_ade)

# Figures and stats for total distance traveled

trials2021_trackerDist_QAQC %>%
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
  theme(legend.position = "top") +
  xlab(label = "distance (cm)")

# MODEL FOR WSN TALK STATS

summary(lmer(distance ~ Pyc + Alg + Urchin + (1 | tank), data = trials2021_trackerDist_QAQC))

####
#<<<<<<<<<<<<<<<<<<<<<<<<<<END OF SCRIPT>>>>>>>>>>>>>>>>>>>>>>>>#

# SCRATCH PAD ####