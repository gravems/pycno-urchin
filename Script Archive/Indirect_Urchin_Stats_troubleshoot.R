#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#                                                                                ##
# Indirect Urchin MS Figures                                                     ##
# Script Created 2023-01-13                                                      ##
# Last updated 2023-01-31                                                        ##
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

# TO DO 
# Figure out a way to run stats that violate assumptions with boostrapping etc. 

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# TABLE OF CONTENTS                                                            ####
#                                                                                 +
# RECENT CHANGES TO SCRIPT                                                        +
# LOAD PACKAGES                                                                   +
# READ IN DATA                                                                    +
# 2020 URCHIN FEEDING                                                             +
# 2021 URCHIN MOVEMENT                                                            +
#                                                                                 +
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# RECENT CHANGES TO SCRIPT                                                     ####
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# 2023-01-31 Script adapted from Indirect_Urchin_MS.R
#   -reduced to feeding and movement stats
#   -notes added throughout

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# LOAD PACKAGES                                                                ####
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

library(tidyverse) # data manipulation, tidying 
library(viridis) # color-blind-friendly palette 
library(ggpubr) # additional visualizations 
library(DHARMa) # testing assumptions of stats 
library(rstatix) # extracting stats summaries 
library(PMCMRplus) # friedman test posthoc 
library(lmerTest) # lmer

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# LOAD FUNCTIONS                                                               ####
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# fuction for "%notin%
`%notin%` <- Negate(`%in%`)

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# READ IN DATA                                                                 ####
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

trials2020_Q <- read_csv("Data/2020/trials2020_QAQC.csv", 
                         col_types = cols(trial = col_character(), 
                                          bin = col_character(), 
                                          diameter = col_double()))

trials2021_Q <- read_csv("Data/2021/trials2021_QAQC.csv", 
                         col_types = cols(date = col_character(),  
                                          location = col_character(), 
                                          movement = col_character(), 
                                          timeBegin = col_character(), 
                                          timeEnd = col_character()))

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 2020 URCHIN FEEDING                                                          ####
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

str(trials2020_Q)

# Experimental structure:

# To detect indirect effects of Pycnopodia on purple urchin feeding rates, the experiment
# tracked the consumption of kelp by urchins across 5 paired replicates with two treatments
# (exposed to Pycno, not exposed to Pycno) for between 51 and 69 hours. Urchins were
# held individually in small containers within one of two larger seawater tanks that either
# had 'Pycno smell' or not. Kelp was fed to the urchins as small standardized disks
# and consumption across the trial was calculated as the number of disks they ate between
# each time point at which they were checked (every six hours during daylight, twelve hours
# between next day). Values of disks consumed are transformed into biomass below in the 
# script based on weights of 'test disks' of kelp (i.e.: 1 disk = 0.341 g kelp tissue).

# Primary questions:

# Q1
# Did the urchins consume different amounts of kelp based on their treatment?

# Q2
# Did the rate of consumption for urchins change within the trials depending on treatment
# (e.g. - did they eat less at first and then start eating more later?)

# Q3
# Was there a very short-term effect of treatment in amount consumed when urchins were
# first put into the experiment (i.e.: Did urchins exposed to Pycno eat a lot less
# right from the beginning?)


# PROBLEM 1:

# Trial 1 was only run for 51 hours, while trials 2-5 were run for 69 hours. This reduces
# the number of sampling points for trial 1 and makes an unbalanced design. For now, I 
# have truncated all trials to the 51 hour mark (i.e.: removed the last two observation
# points for trials 2-5 so they match up with trial 1). I had to do this because the 
# stats I was using required a balanced design, but if there's a way around this, that
# would be great.

####################################

####################
#      ##     ##   #
#    ##  ##    #   #
#   #     #    #   # 
#   #     #    #   #
#   #  #  #    #   #
#    #  ##     #   #
#     ### #   ###  #
#                  #
#################### 

# Addressing Q1 : total consumption differences by end of experiment between treatments

# create dataframe of trials 1-5, up to time point 7 (i.e.: hour 51), and remove timepoint
# zero which is all zeros. Summarise total amount consumed per urchin to compare 
# overall feeding rate.
per_urchin_consumed_trunc <- trials2020_Q %>%
  filter(timepoint %notin% c(0, 8, 9)) %>%
  group_by(trial, bin, tank, ID, pycno) %>%
  summarise(cc = (sum(consumed) * 0.341)) %>%
  ungroup()


# create mixed effects model testing total consumption per urchin against treatment
# with trial as a random factor
feeding_lme <- lmer(cc ~ pycno + (1|trial), data = per_urchin_consumed_trunc)

# test if assumptions of model are met
feeding_sim <- simulateResiduals(fittedModel = feeding_lme, plot = F)
plot(feeding_sim)
testDispersion(feeding_lme)

# Assumptions NOT MET (failed Levene Test for homogeneity of variance)

# Log transform data to see if that helps

# All trials truncated total consumption per urchin in grams Log for tests
per_urchin_consumed_trunc_log <- trials2020_Q %>%
  filter(timepoint %notin% c(0, 8, 9)) %>%
  group_by(trial, bin, tank, ID, pycno) %>%
  summarise(cc = log((sum(consumed) * 0.341)+1)) %>%
  ungroup()

# create mixed effects model testing total consumption per urchin against treatment
# with trial as a random factor and log-transformed response
feeding_lme <- lmer(cc ~ pycno + (1|trial), data = per_urchin_consumed_trunc_log)

# test if assumptions of model are met
feeding_sim <- simulateResiduals(fittedModel = feeding_lme, plot = F)
plot(feeding_sim)
testDispersion(feeding_lme)

# Assumptions are now met!

# view summary of test
summary(feeding_lme)
# effect of treatment is significant p < 0.02

# Calculate the actual difference in consumption between groups
per_urchin_consumed_trunc %>%
  group_by(pycno) %>%
  summarise(mean_total = mean(cc)) 
# difference between pycno and no pycno total consumption
5.67-2.86

# percentage reduction in feeding
2.81/5.67
# a 50% reduction in feeding by urchins exposed to Pycnos at 51 hours


# FIGURE - total consumed per urchin across all trials with standard error
per_urchin_consumed_trunc %>%
  mutate(Treatment = ifelse(pycno == 'yes', "Pycno", "Control")) %>%
  ggplot(aes(x = Treatment, y = cc, color = Treatment)) + 
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
  ylim(0, 23) +
  labs(y = "Kelp consumed (g)", x = "Treatment") +
  theme_minimal(base_size = 15) +
  theme(axis.title.y = element_text(vjust = 3)) +
  theme(legend.position = "none") +
  annotate("text", label = "p = 0.012", x = 2.3, y = 21, size = 5)

####################
#      ##    ###   #
#    ##  ##     #  #
#   #     #     #  # 
#   #     #    ##  #
#   #  #  #   ##   #
#    #  ##   ##    #
#     ### #  ####  #
#                  #
####################

# Q2 : differences in feeding rates over the course of each trial between treatments


# Create data frame of truncated feeding per urchin per time point. Calculate the 
# 'difference in feeding rate' between time points per urchin (e.g.: no change between 
# time point for an urchin = 0, increase in feeding since last timepoint = positive number,
# decrease in feeding = negative number). Convert 'time point' into the actual hours that 
# they represent for analyses and visualizations
timepoint_consumption_change <- trials2020_Q %>%
  filter(timepoint %notin% c(8, 9)) %>%
  mutate(g_consumed = consumed * 0.341) %>%
  group_by(ID, pycno, trial) %>%
  summarise(change = diff(g_consumed))
timepoint_consumption_change$hours <- rep(1:7, 71)
timepoint_consumption_change$hours <- timepoint_consumption_change$hours %>%
  recode(
    '1' = 3,
    '2' = 9,
    '3' = 21,
    '4' = 27,
    '5' = 33,
    '6' = 45,
    '7' = 51
  )

# Mixed effects model testing if feeding rate changes for urchins within a single
# trials, with urchin ID nested within trial as random factors
change_lme <- lmer(change ~ pycno + (1|ID:trial) + (1|trial), data = timepoint_consumption_change)

# test if assumptions of model are met
change_sim <- simulateResiduals(fittedModel = change_lme, plot = F)
plot(change_sim)
testDispersion(change_lme)
# Almost all assumptions are violated

# make plot of variances around each trial
# visualize spread of data per trial
ggboxplot(timepoint_consumption_change, x = "hours", y = "change", add = "jitter")

# Try again with log transformed data shifted by a constant to make all 
# values positive before taking the log
timepoint_consumption_change_log <- timepoint_consumption_change %>%
  mutate(change = log(change + (abs(min(change)) + 0.001)))

# Mixed effects model testing if feeding rate changes for urchins within a single
# trials, with urchin ID nested within trial as random factors log transformed
change_lme <- lmer(change ~ pycno + (1|ID:trial) + (1|trial), data = timepoint_consumption_change_log)

# test if assumptions of model are met
change_sim <- simulateResiduals(fittedModel = change_lme, plot = F)
plot(change_sim)
testDispersion(change_lme)
# Almost all assumptions are still violated

# make plot of variances around each trial for log data
# visualize spread of data per trial
ggboxplot(timepoint_consumption_change_log, x = "hours", y = "change", add = "jitter")

# only option is a Friedman Test - nonparametric repeated measure one-way ANOVA
# have to split out treatments and test separately, can't directly compare
# treatments to each other, but can detect differences in feeding rate within
# each treatment across time. 

# Friedman test (nonparametric one-way ANOVA) and Nemeny Posthoc 

# separate out pycno treatment for testing
pycno_change <- timepoint_consumption_change %>%
  filter(pycno == 'yes')

# make into dataframe for test (required by the friedman_test function)
pycno_df <- as.data.frame(pycno_change)

# view table of 'common' stats from the data
pycno_df %>%
  group_by(hours) %>%
  get_summary_stats(change, type = "common")

# visualize spread of data per trial
ggboxplot(pycno_df, x = "hours", y = "change", add = "jitter")

# run friedman test on pycno treatment
pycno_fried <- pycno_df %>% 
  friedman_test(change ~ hours | ID)
pycno_fried
# test was significant p = 0.046. There was change in feeding rates within trials

# test how strong the effect was with Kendall's W test (0 = no effect, 1 = huge effect)
pycno_df %>% 
  friedman_effsize(change ~ hours |ID)
# effect was small X = 0.0647

# what was the mean change across all trials for the Pycno treatment?
pycno_change %>%
  ungroup() %>%
  summarise(mean(change))
# mean change was positive value 0.0717 (overall increase in feeding, but unknown
# WHEN during trials this effect is strongest)

# run posthoc test to detect which time points were different from each other
frdAllPairsNemenyiTest(change ~ hours | ID, data = pycno_df)
# NONE were different in posthoc, likely b/c marginal friedman value (0.046)




# separate out no pycno for testing
nopycno_change <- timepoint_consumption_change %>%
  filter(pycno == 'no')

# make into dataframe for test (required by the friedman_test function)
nopycno_df <- as.data.frame(nopycno_change)

# view table of 'common' stats from the data
nopycno_df %>%
  group_by(hours) %>%
  get_summary_stats(change, type = "common")

# visualize spread of data per trial
ggboxplot(nopycno_df, x = "hours", y = "change", add = "jitter")

# run friedman test on pycno treatment
nopycno_fried <- nopycno_df %>% 
  friedman_test(change ~ hours | ID)
nopycno_fried
# test was significant p = 0.00116. There was change in feeding rates within trials

# test how strong the effect was with Kendall's W test (0 = no effect, 1 = huge effect)
nopycno_df %>% 
  friedman_effsize(change ~ hours |ID)
# effect was small X = 0.0970

# what was the mean change across all trials for the Pycno treatment?
nopycno_change %>%
  ungroup() %>%
  summarise(mean(change))
# mean change was positive value 0.104 (overall increase in feeding, but unknown
# WHEN during trials this effect is strongest)

# run posthoc test to detect which time points were different from each other
frdAllPairsNemenyiTest(change ~ hours | ID, data = nopycno_df)
# hour 3 was different from 4 of the 6 other trials p < 0.02


# Visualize these results in a single figure with cumulative consumption as well

# prepare to join datasets - change dataset
change_trunc <- timepoint_consumption_change %>%
  unite(urchintime, ID, hours, sep = "_", remove = FALSE)

# extract cumulative consumption at each time point per urchin to join - cumulative dataset
urchin_cum_join <- trials2020_Q %>%
  filter(timepoint != 0) %>%
  group_by(pycno, ID) %>%
  mutate(cc = cumsum(consumed * 0.341)) 
urchin_cum_join$hours <- urchin_cum_join$timepoint %>%
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
urchin_cum_join <- urchin_cum_join %>%
  unite(urchintime, ID, hours, sep = "_") %>%
  ungroup() %>%
  select(-pycno, -trial)

# join change dataset with cumulative consumption dataset
change_cum_all <-  change_trunc %>%
  left_join(urchin_cum_join, by = "urchintime")
# change treatment labels for plot
change_cum_all <- change_cum_all %>%
  mutate(pycno = case_when(pycno == "yes" ~ "Pycno present",
                           pycno == "no" ~ "Pycno not present"))

# plot of change per timepoint mean sterror, overlay with overall rate of consumption
ggplot(change_cum_all, aes(x = hours, y = change)) +
  geom_jitter(col = 'grey', size = 3, width = 1) +
  facet_grid(~pycno) +
  stat_summary(
    geom = "point",
    fun = "mean",
    size = 2,
    shape = 19
  ) +
  geom_errorbar(stat="summary", fun.data="mean_se", size = 1) +
  stat_summary(aes(y = cc), fun = mean, color = "red", geom = "line") +
  scale_y_continuous("Change in consumption (g)", sec.axis = sec_axis(~., labels = c("", "", "0", "2", "4", "6"), 
                                         name = "Mean cumulative kelp consumed (g)")) +
  scale_x_continuous(breaks = seq(0, 60, 24)) +
  xlab("Hours") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "blue") +
  theme_bw() +
  theme(axis.title.y.right = element_text(margin = margin(t = 12, r = 12, b = 12, l = 12)),
        axis.title.y.left = element_text(margin = margin(t = 12, r = 12, b = 12, l = 12))) 

####################
#      ##    ###   #
#    ##  ##     #  #
#   #     #     #  # 
#   #     #   ##   #
#   #  #  #     #  #
#    #  ##      #  #
#     ### #  ###   #
#                  #
####################

# Q3 : Did urchins eat a lot less/more at the beginning of the trial based on treatment? 
#      (kind of an inversion of Q1 which dealt with the end of the trial/overall consumption)

# Separate out the first time point (hour 3) for amount consumed across all urchins and 
# transform kelp disk to biomass
per_urchin_consumed_initial <- trials2020_Q %>%
  filter(timepoint == 1) %>%
  mutate(g_consumed = consumed * 0.341)

# Create mixed effects model to test if initial consumption was different based on treatment
# with urchin nested within trial
initial_lme <- lmer(g_consumed ~ pycno + (1|trial), data = per_urchin_consumed_initial)

# test if assumptions of model are met
initial_sim <- simulateResiduals(fittedModel = initial_lme, plot = F)
plot(initial_sim)
testDispersion(initial_sim)
# assumptions of model are not met

# try a square root transformation on data
per_urchin_consumed_initial_sqrt <- trials2020_Q %>%
  filter(timepoint == 1) %>%
  mutate(g_consumed = sqrt(consumed * 0.341))

# Create mixed effects model to test if initial consumption was different based on treatment
# with urchin nested within trial square root transformed data
initial_lme <- lmer(g_consumed ~ pycno + (1|trial), data = per_urchin_consumed_initial_sqrt)

# test if assumptions of model are met
initial_sim <- simulateResiduals(fittedModel = initial_lme, plot = F)
plot(initial_sim)
testDispersion(initial_sim)
# assumptions equal variance not met

# visualize the data to observe unequal variances
ggboxplot(per_urchin_consumed_initial, x = "trial", y = "g_consumed", color = "pycno", add = "jitter")



# PROBLEM 2:

# Ideally, I'd like to run a model that would account for all the cumulative consumption
# and/or changes in feeding rates across trials per urchin. I've included a figure
# that I made a while ago to illustrate what I mean, but I could not figure out any 
# statistical test that would work because it's essentially a repeated measures mixed-
# effects model (two-way ANOVA) with data that doesn't conform to assumptions. So far 
# as I can figure there is no non-parametric equivalent that exists. Any thoughts
# you have on this would be great. It would prevent a lot of the overly complex analyses
# I've done above from being necessary (e.g.: breaking out treatment groups and not being 
# able to compare between them.)

# here is the figure I originally made before running the stats. If you can think of a way
# to statistically test this for things like significant differences in slope, and changes
# in rates between time points, that would be awesome. Essentially non of the assumptions
# for normal repeated measures are met, plus I'm missing the last two timepoints' data 
# for the first trial. 

cumulative_figure <- trials2020_Q
cumulative_figure$hours <- cumulative_figure$timepoint %>%
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
 cumulative_figure %>%
  unite('ID',
      trial,
      bin,
      sep = "_",
      remove = FALSE) %>%
  group_by(pycno, ID) %>%
  mutate(cc = cumsum(consumed) * 0.341) %>%
  mutate(Treatment = ifelse(pycno == 'yes', "Pycno", "Control")) %>%
  ggplot(aes(x = hours, y = cc, color = Treatment)) +
  scale_color_viridis(discrete = TRUE,
                      begin = 0.3,
                      end = 0.7,
                      option = "magma") +
  geom_line(aes(group = ID), alpha = 0.15) +
  geom_point(size = 2, position = "jitter", alpha = 0.15) +
  geom_smooth(method = "lm", size = 2) +
  scale_y_continuous(name = "Kelp consumed (g)") + 
  theme_minimal(base_size = 15) +
  theme(legend.position = "top")

# caption: x = hours since experiment started, y = cumulative amount of kelp
 # consumed per urchins, solid lines are a regression of feeding rate, and
 # transparent points are individual urchins cumulative consumption color
 # coded for treatment.


#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 2021 URCHIN MOVEMENT                                                         ####
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

 str(trials2021_Q)
 
 # Experimental structure:
 
 # To detect indirect effects of Pycnopodia on purple urchin movement over short (i.e.:
 # 1 hour) time scales and to determine if urchins will risk feeding depending on if
 # they're well fed or starved, I ran an experiment testing various behaviors.
 # Urchins were split into two groups, starved and fed, and held in those conditions
 # (food withheld or given) for 7 weeks to precondition them for the experiment. 
 # For the actual experiment I had two arenas (for convenience, identical) that I 
 # would place an urchin into and watch for an hour. Like the previous experiment,
 # one of the treatments was exposure to Pycnopodia water that was plumbed from a header
 # tank into the center of the arena. There was also a no-Pycno control treatment.
 # The other factors in the experiment were the starvation state of the particular urchin
 # tested (fed, starved), and the presence or absence of a kelp cue in the middle of the
 # tank next to the outflow from the header tank. This created a fully crossed design of 
 # Pycno (present, absent), urchnin (fed, starved), and kelp (present, absent). I would place
 # the urchins in the tank and record their behavior every minute for an hour. The only
 # behaviors I'm addressing here are movement (i.e.: how much time did they spend moving around
 # versus sitting still), and how much time they spent 'interacting' with the cue 
 # sources in the center of the tank (interacting, not interacting). For the purposes
 # of the experiment, 'interacting' meant that the urchin had at least one tube foot 
 # touching the kelp/kelp control, or the actual inflow from which the Pycno or control
 # water was flowing from. 
 #
 # I also filmed all the trials with GoPro and used some software to extract xy coordinates
 # set to a scale bar and caculate total distance moved by each urchin. 
 #  
 # The idea is that movement can be a proxy for 'escape response' and interaction with
 # the signal (particularly in the treatments where kelp was present) can be a proxy
 # for 'willingness to risk predation for food'. 

 # Primary questions:
 
 # Q4
 # Did any of the treatments change amount of time urchins spent moving around?
 
 # Q5
 # Did any of the treatments change amount of time interacting with the signal?
 
 # Q6
 # Was the total distance moved different between treatments? 

 ##############################
 
 # PROBLEM 3:
 #
 # There are immediate problems with this dataset. It is not balanced (I had to 
 # rerun a trial b/c a GoPro wasn't turned on) so there is an extra trial for
 # one of the treatments. In addition, since I ran it in two separate arenas,
 # it turned out there was a 'tank effect' for the movement stats. I think
 # this was because treatments were not even distributed between the tanks,
 # but my stats to try to figure this out have been unclear. Any thoughts you 
 # have here would be helpful.

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

# kelp consumption stats

# 3.463606 cm^2 = 0.341 g
# 0.341/3.463606
# 0.09845231 per cm^2

trials2021_algal_consumption %>%
  mutate(eaten = eaten * 0.0985) %>%
  filter(eaten != 0) %>%
  t_test(eaten ~ pycnoTreat)

trials2021_algal_consumption %>%
  mutate(eaten = eaten * 0.0985) %>%
  filter(eaten != 0) %>%
  group_by(pycnoTreat) %>%
  summarise(mean(eaten))



############ STATS FROM GRAVEM

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
model_1_60 <- glm(interacting ~ urchinGroup + pycnoTreat*algalTreat, family = binomial, data = model_1_dat_60)
summary(model_1_60)

# is there a correlation between tank and one of the other factors?
# Create a table with the needed variables.
tank_dat = table(model_1_dat_60$urchinGroup, model_1_dat_60$tank) 
print(tank_dat)
# Perform the Chi-Square test.
print(chisq.test(tank_dat))
tank_dat = table(model_1_dat_60$pycnoTreat, model_1_dat_60$tank)
print(chisq.test(tank_dat))
tank_dat = table(model_1_dat_60$algalTreat, model_1_dat_60$tank)
print(chisq.test(tank_dat))


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

control_avg_interact <- 
  model_1_plotdat_5 %>%
  ungroup() %>%
  filter(Treatment == "Control") %>%
  summarize(avg = mean(interacting)) %>%
  pull(avg)


plot_interact <- model_1_plotdat_5 %>%
  ggplot(aes(y = interacting, x = Treatment, color = urchinGroup)) +
  geom_hline(aes(yintercept = control_avg_interact), color = "gray70", size = 0.8, linetype = "dashed") +
  geom_jitter(position = position_jitter(seed = 227, width = 0.2), size = 2, alpha = 0.20) +
  stat_summary(fun = mean, geom = "point", size = 6.5, shape = 21, fill = "black", position = position_jitter(seed = 227, width = 0.2)) +
  stat_summary(fun = mean, geom = "point", size = 5, shape = 16, position = position_jitter(seed = 227, width = 0.2)) +
  scale_y_continuous(
    limits = c(-0.1, 1.1), expand = c(0.005, 0.005)) +
  coord_flip() +
  scale_color_viridis(discrete = TRUE, begin = 0.3, end = 0.8, option = "H") +
  labs(color = "Urchin Group") +
  labs(y = "Proportion Time Interacting", x = NULL) +
  theme(
    axis.title = element_text(size = 16),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_blank(),
    panel.grid = element_blank()
  ) 

# difference in urchin groups interacting
trials2021_Q %>%
  group_by(urchinGroup, trial) %>%
  mutate(interaction = case_when(interaction == "ni" ~ 0,
                              interaction == "i" ~ 1)) %>%
  summarise(interaction = sum(interaction)) %>%
  ungroup() %>%
  group_by(urchinGroup) %>%
  summarise(mean(interaction))








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

control_avg_move <- 
  model_1_plotdat_5 %>%
  ungroup() %>%
  filter(Treatment == "Control") %>%
  summarize(avg = mean(movement)) %>%
  pull(avg)


plot_move <- model_1_plotdat_5 %>%
  ggplot(aes(y = movement, x = Treatment, color = urchinGroup)) +
  geom_hline(aes(yintercept = control_avg_move), color = "gray70", size = 0.8, linetype = "dashed") +
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

# difference in moving between starved and fed groups
trials2021_Q %>%
  group_by(urchinGroup, trial) %>%
  mutate(movement = case_when(movement == "st" ~ 0,
                              movement == "ma" ~ 1,
                              movement == "mt" ~ 1,
                              movement == "mp" ~ 1)) %>%
  summarise(movement = sum(movement)) %>%
  ungroup() %>%
  group_by(urchinGroup) %>%
  summarise(mean(movement))
  

# COMBINED MOVEMENT INTERACTION FIGURE
ggpubr::ggarrange(plot_move, plot_interact, 
          labels = c("a","b"), 
          label.x = c(0.24, -0.02),
          ncol = 2,
          nrow = 1,
          widths = c(1, 0.73),
          common.legend = TRUE, legend = "top")
# best size: 800 x 400

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 2021 TRACKER DATA                                                            ####
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

tracker_all_ade <- as.ltraj(xy = trials2021_trackerCoord_QAQC[, c('x','y')], typeII = FALSE, 
                 id = trials2021_trackerCoord_QAQC$id)
plot(tracker_all_ade)

ggplot(tracker_all_ade) +
  geom(point)

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


# difference in distance between fed and starved
trials2021_trackerDist_QAQC %>%
  group_by(Urchin) %>%
  summarise(mean(distance))

842/576

# MODEL FOR WSN TALK STATS

summary(lmer(distance ~ Pyc +Alg + Urchin + (1 | tank), data = trials2021_trackerDist_QAQC))


# PLOTS FOR DISTANCE

dist_data <- trials2021_trackerDist_QAQC %>%
  unite(Treatment, Pyc, Alg, sep = "/") %>%
  mutate(Treatment = case_when(Treatment == "Pycno/No Algae" ~ "Pycno",
                               Treatment == "No Pycno/Algae" ~ "Nereo",
                               Treatment == "No Pycno/No Algae" ~ "Control",
                               Treatment == "Pycno/Algae" ~ "Pycno/Nereo")) 

plot_dist <- dist_data %>%
  ggplot(aes(y = distance, x = Treatment, color = Urchin)) +
  geom_jitter(position = position_jitter(seed = 227, width = 0.2), size = 2, alpha = 0.20) +
  stat_summary(fun = mean, geom = "point", size = 6.5, shape = 21, fill = "black", position = position_jitter(seed = 007, width = 0.2)) +
  stat_summary(fun = mean, geom = "point", size = 5, shape = 16, position = position_jitter(seed = 007, width = 0.2)) +
  coord_flip() +
  scale_color_viridis(discrete = TRUE, begin = 0.3, end = 0.8, option = "H") +
  labs(color = "Urchin Group") +
  labs(y = "Distance Moved (cm)", x = NULL) +
  theme(
    axis.title = element_text(size = 16),
    axis.text.x = element_text(size = 12),
    panel.grid = element_blank()
  )

plot_dist_overall <-  
  dist_data %>%
  mutate(`Urchin Group` = Urchin) %>%
  ggplot(aes(x = `Urchin Group`, y = distance, fill = `Urchin Group`)) + 
  geom_jitter(col = "grey", size = 3, width = 0.1) +
  geom_boxplot(width = 0.25, alpha = 0.5) +
  scale_fill_viridis(discrete = TRUE, begin = 0.3, end = 0.8, option = "H") +
  labs(y = "Distance Moved (cm)", x = "Urchin Group") +
  theme_minimal(base_size = 15) +
  theme(axis.title.y = element_text(vjust = 3)) +
  annotate("text", x = 1.5, y = 1200, label = "p < 0.001")

# COMBINED MOVEMENT INTERACTION FIGURE
ggpubr::ggarrange(plot_dist, plot_dist_overall, 
                  labels = c("a","b"), 
                  ncol = 1,
                  nrow = 2,
                  widths = c(1, 0.73),
                  common.legend = TRUE, legend = "top")
# best size: 450 x 750


####
#<<<<<<<<<<<<<<<<<<<<<<<<<<END OF SCRIPT>>>>>>>>>>>>>>>>>>>>>>>>#

# SCRATCH PAD ####

