#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#                                                                                ##
# Urchin-Pycno fear feeding trials                                               ##
# Data are current as of 2020-09-18                                              ##
# Data source: Ross Whippo - UO/OIMB                                             ##
# R code prepared by Ross Whippo                                                 ##
# Last updated 2020-09-18                                                        ##
#                                                                                ##
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# SUMMARY:


# Required Files (check that script is loading latest version):
# urchin_fear_feeding_pycno-trial-1.csv

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

# 2020-09-15 Script created

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# LOAD PACKAGES                                                                ####
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

library(tidyverse)
library(viridis)
library(nlme)
library(vegan)
library(ggpubr)

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# READ IN AND PREPARE DATA                                                     ####
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

urchin_fear_pycno <-
  read_csv('Data/urchin_fear_feeding_pycno-trial-1.csv')
# dataset with bucket treatment removed:
#urchin_fear_pycno <- urchin_fear_pycno %>%
#  filter(treatment != 'bucket')


#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# MANIPULATE DATA - VISUALIZATIONS                                             ####
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# mean confetti consumed across all urchins per treatment

# combine trial and bin to prevent combining urchins across trials
urchin_fear_pycno <-
  unite(urchin_fear_pycno,
        'ID',
        trial,
        bin,
        sep = "_",
        remove = FALSE)

per_urchin_consumed_total <- urchin_fear_pycno %>%
  group_by(ID, pycno) %>%
  summarise(sum(consumed))

names(per_urchin_consumed_total)[names(per_urchin_consumed_total) ==
                                   "sum(consumed)"] <- "total_consumed"

# urchin sizes
urchin_size <- urchin_fear_pycno %>%
  filter(timepoint == 0)
urchin_size$diameter <- as.numeric(urchin_size$diameter)
size_plot <- ggplot(
  urchin_size, aes(x = pycno, y = as.numeric(diameter), fill = pycno)) +
  geom_boxplot() +
  scale_fill_viridis(discrete = TRUE,
                     begin = 0.2,
                     end = 0.9) +
  theme_minimal()

summary(urchin_size$diameter)

# total consumed by each urchin across all trials
total_consumed_plot <- ggplot(per_urchin_consumed_total,
       aes(x = pycno, y = total_consumed, fill = pycno)) +
  geom_boxplot() +
  scale_fill_viridis(discrete = TRUE,
                     begin = 0.2,
                     end = 0.9) +
  theme_minimal()

t.test(diameter ~ pycno, data = urchin_size)

per_trial_pycno <- per_urchin_consumed_total %>%
  filter(pycno == "yes")
summary(per_trial_pycno$total_consumed)
sd(per_trial_pycno$total_consumed)

per_trial_empty <- per_urchin_consumed_total %>%
  filter(pycno == "no")
summary(per_trial_empty$total_consumed)
sd(per_trial_empty$total_consumed)

# amount consumed per timepoint
timepoint_consumed_plot <- ggplot(urchin_fear_pycno, aes(x = pycno, y = consumed, fill = pycno)) +
  geom_boxplot() +
  scale_fill_viridis(discrete = TRUE,
                     begin = 0.2,
                     end = 0.9) +
  theme_minimal()

per_timepoint_pycno <- urchin_fear_pycno %>%
  filter(pycno == "yes")
summary(per_timepoint_pycno$consumed)
sd(per_timepoint_pycno$consumed)

per_timepoint_empty <- urchin_fear_pycno %>%
  filter(pycno == "no")
summary(per_timepoint_empty$consumed)
sd(per_timepoint_empty$consumed)


urchin_timeseries <- urchin_fear_pycno %>%
  unite("datetime", date:time, sep = " ")

urchin_timeseries$datetime <-
  as.POSIXlt(urchin_timeseries$datetime)

timeseries_bar <- ggplot(urchin_timeseries, aes(
  x = as.POSIXct(datetime),
  y = consumed,
  fill = pycno
)) +
  geom_col() +
  scale_fill_viridis(discrete = TRUE,
                     begin = 0.2,
                     end = 0.9) +
  facet_wrap( ~ pycno)

# cumulative total consumed by trial

urchin_time <- urchin_timeseries %>%
  group_by(trial, pycno) %>%
  mutate(tot_consumed = sum(consumed))

total_box <- ggplot(
  urchin_timeseries %>%
    group_by(trial, pycno) %>%
    mutate(tot_consumed = sum(consumed)),
  aes(x = pycno, y = tot_consumed, fill = factor(pycno))
) +
  geom_boxplot() +
  scale_fill_viridis(discrete = TRUE,
                     begin = 0.2,
                     end = 0.9) +
  theme_minimal()

per_exp_pycno <- urchin_time %>%
  select(-datetime) %>%
  filter(pycno == "yes")
summary(per_exp_pycno$tot_consumed)
sd(per_exp_pycno$tot_consumed)

per_exp_empty <- urchin_time %>%
  select(-datetime) %>%
  filter(pycno == "no")
summary(per_exp_empty$tot_consumed)
sd(per_exp_empty$tot_consumed)


# change unknown diameters to 61

urchin_timeseries$diameter <- urchin_timeseries$diameter %>%
  recode("nd" = "61")

ggplot(
  urchin_timeseries %>%
    group_by(pycno, ID) %>%
    mutate(cc = cumsum(consumed)),
  aes(
    x = timepoint, # as.POSIXct(datetime),
    y = cc,
    color = factor(pycno)
  )
) +
  scale_color_viridis(discrete = TRUE,
                      begin = 0.3,
                      end = 0.8) +
  geom_point(aes(size = log(as.numeric(
    urchin_timeseries$diameter
  )) * 2), alpha = 0.7) +
  geom_line(aes(group = ID), alpha = 0.25) +
  geom_smooth(method = "lm") +
  theme_minimal() +
  scale_y_continuous(name = "Cumulative Confetti Consumed (linear)")

# with scaled time points
urchin_timeseries$hours <- urchin_timeseries$timepoint %>%
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

hour_line <- ggplot(
  urchin_timeseries %>%
    group_by(pycno, ID) %>%
    mutate(cc = cumsum(consumed)),
  aes(
    x = hours, # as.POSIXct(datetime),
    y = cc,
    color = factor(pycno)
  )
) +
  scale_color_viridis(discrete = TRUE,
                      begin = 0.3,
                      end = 0.8) +
  geom_point(aes(size = log(as.numeric(
    urchin_timeseries$diameter
  )) * 2), alpha = 0.7) +
  geom_line(aes(group = ID), alpha = 0.25) +
  geom_smooth(method = "lm") +
  theme_minimal() +
  scale_y_continuous(name = "Cumulative Confetti Consumed (linear)")

# mean number of confetti consumed per time point per treatment

timepoint_box <- ggplot(urchin_timeseries, aes(x = pycno, y = consumed, fill = pycno)) +
  scale_fill_viridis(discrete = TRUE,
                     begin = 0.3,
                     end = 0.9) +
  geom_boxplot() +
  facet_grid(. ~ timepoint) +
  theme_minimal()



# total confetti eaten per treatment
urchin_fear_pycno %>%
  group_by(treatment) %>%
  summarise(sum(consumed))

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# MANIPULATE DATA - STATS                                                      ####
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

urchin_fear_mix_data <- urchin_fear_pycno %>%
  filter(treatment != 'bucket') %>%
  filter(timepoint != '0')

# is total amount of confetti consumed per trial different between groups?
t.test(total_consumed ~ pycno, data = per_urchin_consumed_total)

# is aount of confetti consumed per time point different between groups (w/size)?
mix_1 <-
  lme(consumed ~ pycno, # + diameter,
      random = ~ timepoint | ID,
      data = urchin_fear_mix_data)
mix_1
anova(mix_1)

# calculate slopes of consumption
urchin_vals <-  urchin_timeseries %>%
  group_by(pycno, ID) %>%
  mutate(cc = cumsum(consumed))


############### COLLATED FIGURES

Figure_1 <- ggarrange(size_plot, timepoint_consumed_plot, total_consumed_plot, total_box, 
                         labels = c("A", "B", "C", "D"),
                         ncol = 2, nrow = 2,
                         common.legend = TRUE, legend = "right")
annotate_figure(Figure_1, bottom = text_grob("Figure 1: Calcuated A) urchin test diameter across all trials, B) kelp 'confetti' consumed per urchin per timepoint, \n C) total kelp 'confetti' consumed per urchin across each trial, and D) total kelp 'confetti' consumed across all trials.", size = 10))
# 

####
#<<<<<<<<<<<<<<<<<<<<<<<<<<END OF SCRIPT>>>>>>>>>>>>>>>>>>>>>>>>#

# SCRATCH PAD ####

#urchin_fear_pycno <- urchin_fear_pycno %>%
#  filter(treatment != 'bucket') %>%
#  filter(timepoint != '0')
