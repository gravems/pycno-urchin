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
plot(urchin_size$diameter ~ as.factor(as.character(urchin_size$ID)))
summary(as.numeric(urchin_size$diameter))

# total consumed across all trials
ggplot(per_urchin_consumed_total,
       aes(x = pycno, y = total_consumed, fill = pycno)) +
  geom_boxplot() +
  scale_fill_viridis(discrete = TRUE,
                     begin = 0.2,
                     end = 0.9) +
  theme_minimal()

# amount consumed per timepoint
ggplot(urchin_fear_pycno, aes(x = pycno, y = consumed, fill = pycno)) +
  geom_boxplot() +
  scale_fill_viridis(discrete = TRUE,
                     begin = 0.2,
                     end = 0.9) +
  theme_minimal()


urchin_timeseries <- urchin_fear_pycno %>%
  unite("datetime", date:time, sep = " ")

urchin_timeseries$datetime <-
  as.POSIXlt(urchin_timeseries$datetime)

ggplot(urchin_timeseries, aes(
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

ggplot(
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

ggplot(
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

ggplot(urchin_timeseries, aes(x = pycno, y = consumed, fill = pycno)) +
  scale_fill_viridis(discrete = TRUE,
                     begin = 0.3,
                     end = 0.9) +
  geom_boxplot() +
  facet_grid(. ~ timepoint)
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
  lme(consumed ~ pycno + diameter,
      random = ~ timepoint | ID,
      data = urchin_fear_mix_data)
mix_1
anova(mix_1)


############### SUBSECTION HERE

####
#<<<<<<<<<<<<<<<<<<<<<<<<<<END OF SCRIPT>>>>>>>>>>>>>>>>>>>>>>>>#

# SCRATCH PAD ####

urchin_fear_pycno <- urchin_fear_pycno %>%
  filter(treatment != 'bucket') %>%
  filter(timepoint != '0')
