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

library(viridis) # color-blind-friendly palette
library(car) # diameter stats, unbalanced design
library(lmerTest) # lmer 
library(nlme) #lme
library(lme4) # feeding stats?
library(adehabitatLT) # tracker movement analyses
library(tidyverse) # data manipulation, tidying

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# LOAD FUNCTIONS                                                               ####
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# fuction for "%notin%
`%notin%` <- Negate(`%in%`)

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# READ IN DATA                                                     ####
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

trials2020_Q <- read_csv("Data/2020/trials2020_QAQC.csv", 
                         col_types = cols(trial = col_character(), 
                                          bin = col_character(), diameter = col_double()))

confetti_weights <- read_csv("Data/2020/confetti_weights.csv", 
                             col_types = cols(trial = col_character()))

trials2021_Q <- read_csv("Data/2021/trials2021_QAQC.csv", 
                         col_types = cols(date = col_character(),  location = col_character(), 
                                          movement = col_character(), 
                                          timeBegin = col_character(), timeEnd = col_character()))

trials2021_algal_consumption <- read_csv("Data/2021/algal_consumption.csv")

trials2021_trackerDist_QAQC <- read_csv("Data/2021/trials2021_trackerDist_QAQC.csv")

trials2021_trackerCoord_QAQC <- read_csv("Data/2021/trials2021_trackerCoord_QAQC.csv")


#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 2020 URCHIN FEEDING                                                          ####
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# differences in urchin size between groups

diams_2020 <- trials2020_Q %>%
  filter(timepoint == 0)
diams_2020$diameter <- as.numeric(diams_2020$diameter)

t.test(diameter ~ pycno, data = diams_2020)

summary(diams_2020$diameter)

# Kelp confetti weights controls

# mean biomass ('before' confetti)
confetti_weights %>%
  filter(`before-after` == "before") %>%
  summarise(mean(weight_mg), sd(weight_mg))

# > pi*((2.1/2)^2)
# [1] 3.463606 cm^2

confetti_weights <- confetti_weights %>%
  rename("beforeafter" = "before-after")

# is weight different before and after? 
weightmix <- lmer(weight_mg ~ beforeafter + (1|trial/tank), data = confetti_weights)
summary(weightmix)




# TRIALS 2-5 HAVE TO BE TRUNCATED TO MATCH THE TIME SPAN OF TRIAL 1 FOR A
# REPRESENTATIVE 'END OF TRIAL' AMOUNT. cANNOT REMOVE TRIAL 1 WITHOUT LOSING
# STATISTICAL POWER, NEED THE REPS.  
# DATA WERE LOG+1 TRANSFORMED TO MEET ASSUMPTIONS OF HOMOSCEDASTICITY 

per_urchin_consumed_trunc <- trials2020_Q %>%
  filter(timepoint %notin% c(0, 8, 9)) %>%
  group_by(trial, bin, tank, ID, pycno) %>%
  summarise(cc = (sum(consumed) * 0.341)) %>%
  ungroup()

# ALL TRIALS TRUNCATED total consumption per urchin in grams Log for tests

per_urchin_consumed_trunc_log <- trials2020_Q %>%
  filter(timepoint %notin% c(0, 8, 9)) %>%
  group_by(trial, bin, tank, ID, pycno) %>%
  summarise(cc = log((sum(consumed) * 0.341)+1)) %>%
  ungroup()

# STATS - differences in total consumption per urchin between trials
library(lme4)

# TANK WAS NOT A SIGNIFICANT RANDOM FACTOR AND WAS REMOVED
feeding_lme <- lmer(cc ~ pycno + (1|trial), data = per_urchin_consumed_trunc_log)

library(DHARMa)
feeding_sim <- simulateResiduals(fittedModel = feeding_lme, plot = F)
plot(feeding_sim)
testDispersion(feeding_lme)

summary(feeding_lme)


per_urchin_consumed_trunc %>%
  group_by(pycno) %>%
  summarise(mean_total = mean(cc)) 

# difference between pycno and no pycno total consumption
5.67-2.86

# percentage reduction in feeding

2.81/5.67


# FIGURE - total consumed per urchin across all trials with standard error
per_urchin_consumed_trunc %>%
  mutate(Treatment = ifelse(pycno == 'yes', "Pycno present", "Pycno absent")) %>%
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



# QUANTITY CONSUMED across timepoints


# Change values failed all major assumptions, even with outliers removed


# only option is a Friedman Test - nonparametric repeated measure one-way ANOVA
# have to split out treatments and test separately, can't directly compare
# treatments to each other, but can detect differences in feeding rate within
# each treatment across time. 




# recreate hours column for visualizations and analyses
timepoint_consumption_change <- trials2020_Q %>%
  filter(timepoint %notin% c(8, 9)) %>%
  mutate(g_consumed = consumed * 0.341) %>%
  group_by(ID, pycno) %>%
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





# STATISTICS - Friedman test (nonparametric one-way ANOVA) and Nemeny Posthoc 
library(rstatix)
library(ggpubr)
# pycno present
pycno_change <- timepoint_consumption_change %>%
  filter(pycno == 'yes')
# make into dataframe for test
pycno_df <- as.data.frame(pycno_change)
# pycno present summary and friedman
pycno_df %>%
  group_by(hours) %>%
  get_summary_stats(change, type = "common")
ggboxplot(pycno_df, x = "hours", y = "change", add = "jitter")
pycno_fried <- pycno_df %>% 
  friedman_test(change ~ hours | ID)
pycno_fried
# test effect size 
pycno_df %>% 
  friedman_effsize(change ~ hours |ID)

pycno_change %>%
  ungroup() %>%
  summarise(mean(change))

# friedman and post-hoc in one
library(PMCMRplus)
frdAllPairsNemenyiTest(change ~ hours | ID, data = pycno_df)

# hour 3 change in consumption
pycno_df %>%
  filter(hours == 3) %>%
  summarise(mean(change))



# no pycno present
nopycno_change <- timepoint_consumption_change %>%
  filter(pycno == 'no')
# make into dataframe for test
nopycno_df <- as.data.frame(nopycno_change)
# no pycno present summary and friedman
nopycno_df %>%
  group_by(hours) %>%
  get_summary_stats(change, type = "common")
ggboxplot(nopycno_df, x = "hours", y = "change", add = "jitter")
nopycno_fried <- nopycno_df %>% 
  friedman_test(change ~ hours | ID)
nopycno_fried
# test effect size 
nopycno_df %>% 
  friedman_effsize(change ~ hours |ID)

nopycno_change %>%
  ungroup() %>%
  summarise(mean(change))

# friedman and post-hoc in one
library(PMCMRplus)
frdAllPairsNemenyiTest(change ~ hours | ID, data = nopycno_df)

# hour 3 change in consumption
nopycno_df %>%
  filter(hours == 3) %>%
  summarise(mean(change))








# pycno and pycno trunc together in one figure


# balance design by restricting to first 51 hours of trials
change_trunc <- timepoint_consumption_change %>%
  unite(urchintime, ID, hours, sep = "_", remove = FALSE)

# extract cumulative consumption at each time point per urchin to join

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
  select(-pycno)

change_cum_all <-  change_trunc %>%
  left_join(urchin_cum_join, by = "urchintime")
change_cum_all <- change_cum_all %>%
  mutate(pycno = case_when(pycno == "yes" ~ "Pycno present",
                           pycno == "no" ~ "Pycno absent"))

library(egg) # tag_facet
# plot of change per timepoint mean sterror, overlay with overall rate of consumption
ggplot(change_cum_all, aes(x = hours, y = change)) +
  geom_jitter(col = 'grey', size = 3, width = 1) +
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
  facet_grid(~pycno) +
  theme_bw() +
  theme(axis.title.y.right = element_text(margin = margin(t = 12, r = 12, b = 12, l = 12)),
        axis.title.y.left = element_text(margin = margin(t = 12, r = 12, b = 12, l = 12))) 



# CAN NOW TEST IF INITIAL CONSUMPTION (hour 3) were different between treatments!

per_urchin_consumed_initial <- trials2020_Q %>%
  filter(timepoint == 1) %>%
  mutate(g_consumed = consumed * 0.341)

# TANK NOT A SIGNIFICANT RANDOM FACTOR - REMOVED
initial_lme <- lmer(g_consumed ~ pycno + (1|trial), data = per_urchin_consumed_initial)

ggplot(per_urchin_consumed_initial) +
  geom_boxplot(aes(x = trial, y = g_consumed)) +
  geom_jitter(aes(x = trial, y = g_consumed, color = pycno), width = 0.2)

t.test(g_consumed ~ pycno, data = per_urchin_consumed_initial)

library(DHARMa)
initial_sim <- simulateResiduals(fittedModel = initial_lme, plot = F)
plot(initial_sim)
testDispersion(initial_sim)
outliers(initial_sim)
summary(initial_lme)


per_urchin_consumed_initial %>%
  group_by(pycno) %>%
  summarise(mean_total = mean(g_consumed)) 
# # A tibble: 2 x 2
# pycno mean_total
#  <chr>      <dbl>
#  1 no         0.572
#  2 yes        0.116

# difference between pycno and no pycno total consumption
5.67-2.86

# percentage reduction in feeding

2.81/5.67



#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 2021 URCHIN MOVEMENT                                                         ####
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# differences in urchin diameter between groups

diams_2021 <- trials2021_Q %>%
  select(urchinID, urchinGroup, pycnoTreat, algalTreat, urchinDiam_mm) %>%
  distinct(urchinID, urchinGroup, pycnoTreat, algalTreat, urchinDiam_mm) %>%
  unite("overallGroup", c("urchinGroup", "pycnoTreat", "algalTreat"), 
                              sep = "_", remove = FALSE)

# FIGURE - urchin diameters 2021

diams_2021 %>%
  ggplot(aes(x = overallGroup, y = urchinDiam_mm, fill = overallGroup)) +
  geom_boxplot() +
  scale_fill_viridis(discrete = TRUE)

# ANOVA - urchin diameter 2021

diam2021_anova <- aov(urchinDiam_mm ~ overallGroup, data = diams_2021)
Anova(diam2021_anova, type = "III")

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
  summarise(mean(eaten), sd(eaten))



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
  scale_x_discrete(labels=c("Pycno/Nereo" = "Pycno/Kelp", "Pycno" = "Pycno",
                              "Nereo" = "Kelp", "Control" = "Control")) +
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
  scale_x_discrete(labels=c("Pycno/Nereo" = "Pycno/Kelp", "Pycno" = "Pycno",
                              "Nereo" = "Kelp", "Control" = "Control")) +
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

data(sweetpotato)
model<-aov(yield~virus,data=sweetpotato)
out <- SNK.test(mod_feeding_1,"pycno", console=TRUE, 
                main="Yield of sweetpotato. Dealt with different virus")
print(SNK.test(model,"virus", group=FALSE))
# version old SNK.test()
df<-df.residual(model)
MSerror<-deviance(model)/df
out <- with(sweetpotato,SNK.test(yield,virus,df,MSerror, group=TRUE))
print(out$groups)

# two way repeated measures anova of feeding
mod_feeding_1 <- aov(cc ~ pycno * Hours + Error(ID/(pycno + Hours + pycno:Hours)), data = urchin_timeseries_lm)
summary(mod_feeding_1)
# mean comparison test
edf <- df.residual(mod_feeding_1$Within)
ems <- deviance(mod_feeding_1$Within)/edf
SNK.test1 <- SNK.test(y = mod_feeding_1,
                      trt = "pycno",
                      DFerror = edf,
                      MSerror = ems,
                      alpha = 0.05,
                      group = TRUE)