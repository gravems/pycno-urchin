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
library(adehabitatLT) # tracker movement analyses
library(tidyverse) # data manipulation, tidying

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
  summarise(mean(weight_mg))

# > pi*((2.1/2)^2)
# [1] 3.463606 cm^2

confetti_weights <- confetti_weights %>%
  rename("beforeafter" = "before-after")

# is weight different before and after? 
weightmix <- lme(weight_mg ~ beforeafter, random = ~1|trial/tank, data = confetti_weights)
anova(weightmix)

# total consumption per urchin in grams

per_urchin_consumed_total <- trials2020_Q %>%
  group_by(trial, bin, tank, ID, pycno) %>%
  summarise(cc = sum(consumed) * 0.341) %>%
  ungroup()

# means

per_urchin_consumed_total %>%
  group_by(pycno) %>%
  summarise(mean(cc))

# STATS - differences in total consumption per urchin between trials


summary(lme(cc ~ pycno, random = ~1|trial/tank, data = per_urchin_consumed_total))

per_urchin_consumed_total %>%
  group_by(pycno) %>%
  summarise(mean_total = mean(`total consumed`)) 

# difference between pycno and no pycno total consumption
7.37-4.36

# percentage reduction in feeding

3.01/7.37


# FIGURE - total consumed per urchin across all trials with standard error
per_urchin_consumed_total %>%
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
  ylim(0, 30) +
  labs(y = "Kelp consumed (g)", x = "Treatment") +
  theme_minimal(base_size = 15) +
  theme(axis.title.y = element_text(vjust = 3)) +
  theme(legend.position = "none") +
  annotate("text", label = "p = 0.014", x = 2.3, y = 29, size = 5)




# timeseries per timepoint MIGHT NEED TO CUT THIS

urchin_timeseries <- trials2020_Q %>%
  unite("datetime", date:time, sep = " ")

urchin_timeseries$datetime <-
  as.POSIXlt(urchin_timeseries$datetime)

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

urchin_timeseries <- urchin_timeseries %>%
  group_by(pycno, ID) %>%
  mutate(cc = cumsum(consumed * 0.341))










urchin_timeseries_lm <- urchin_timeseries%>%
  group_by(pycno, ID) %>%
  filter(timepoint != 0) %>%
  mutate(g_consumed =  consumed * 0.341) %>%
  mutate(Treatment = ifelse(pycno == 'yes', "Pycno", "Control")) 
urchin_timeseries_lm$treatment = as.factor(urchin_timeseries_lm$treatment)
urchin_timeseries_lm$Hours = as.factor(urchin_timeseries_lm$Hours)

# two way repeated measures anova of feeding

# assumptions - no significant outliers
library(rstatix)
outliers <- urchin_timeseries_lm %>%
  group_by(pycno) %>%
  identify_outliers(g_consumed)
# FAILED - run ANOVA with and without outliers. If result is same, keep outliers

# normality of dependent variable
library(ggpubr)
urchin_timeseries_lm %>%
  ggqqplot("g_consumed")
# FAILED - too many zeros

# trying to use repeated measures correlation (rmcorr)
urchin_timeseries_rmc <- urchin_timeseries%>%
  group_by(pycno, ID) %>%
  filter(timepoint != 0) %>%
  mutate(cc_g_consumed =  cumsum(consumed * 0.341)) %>%
  mutate(Treatment = ifelse(pycno == 'yes', "Pycno", "Control")) 

############ NOPE



 mod_feeding_1 <- aov(g_consumed ~ pycno * Hours + Error(ID/(pycno + Hours + pycno:Hours)), data = urchin_timeseries_lm)
summary(mod_feeding_1)
# mean comparison test
edf <- df.residual(mod_feeding_1$ID)
ems <- deviance(mod_feeding_1$ID)/edf
SNK.test1 <- SNK.test(y = mod_feeding_1,
                      trt = "pycno",
                      DFerror = edf,
                      MSerror = ems,
                      alpha = 0.05,
                      group = TRUE)


# compare slopes of treatment feeding

mod_feeding_1 <- aov(`cc` ~ pycno*ID, data = urchin_timeseries_lm)
summary(mod_feeding_1)

# FIGURE - cumulative confetti per timepoint

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

#### END OF POSSIBLE CUTS






# 1st half versus 2nd half feeding rates

consumption_change <- trials2020_Q


# calculating  change in consumption per timepoint per urchin
timepoint_consumption_change <- consumption_change %>%
  group_by(ID, pycno) %>%
  summarise(change = diff(consumed))

# recreate hours column
timepoint_consumption_change$hours <- NA
hourfill <- c(rep(c(1:7), 15), rep(c(1:9), 56))
timepoint_consumption_change$hours <- hourfill
timepoint_consumption_change$hours <- timepoint_consumption_change$hours %>%
  recode(
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

# create 1st half and 2nd half column
timepoint_consumption_change <- timepoint_consumption_change %>%
  mutate(half = case_when(hours %in% c(3, 9, 21, 27) ~ "first",
                          hours %in% c(33, 45, 51, 57, 69) ~ "second"))
  

# average change per treatment
timepoint_consumption_change %>%
  group_by(half, pycno) %>%
  summarise(mean_change = mean(change))


# two way repeated measures anova of feeding rates

# assumptions - no significant outliers
library(rstatix)
outliers <-timepoint_consumption_change %>%
  group_by(pycno, half) %>%
  identify_outliers(change)
# FAILED - run ANOVA with and without outliers. If result is same, keep outliers

# normality of dependent variable
library(ggpubr)
timepoint_consumption_change %>%
  ggqqplot("change")
ggplot(timepoint_consumption_change, aes(x = change)) +
  geom_histogram(binwidth = 0.1)
shapiro_test(timepoint_consumption_change$change)
100*sum(timepoint_consumption_change$change == 0)/nrow(timepoint_consumption_change)
# FAILED looks normal histogram but shapiro says NO



# remove outliers
`%notin%` <- Negate(`%in%`)
timepoint_data <- timepoint_consumption_change %>%
  unite(target, ID, hours, sep = "_", remove = FALSE)
removal <- outliers %>%
  select(ID, hours) %>%
  unite(target, ID, hours, sep = "_")
timepoint_normal <- timepoint_data %>%
  filter(target %notin% removal$target)

# assumptions - no significant outliers
library(rstatix)
outliers <- timepoint_normal %>%
  group_by(pycno, half) %>%
  identify_outliers(change)
# FAILED - run ANOVA with and without outliers. If result is same, keep outliers

# normality of dependent variable
library(ggpubr)
timepoint_normal %>%
  ggqqplot("change")
ggplot(timepoint_normal, aes(x = change)) +
  geom_histogram(binwidth = 0.1)
shapiro_test(timepoint_normal$change)
100*sum(timepoint_normal$change == 0)/nrow(timepoint_normal)
# FAILED histogram and shapiro says NO










# STATISTICS - Friedman test (nonparametric one-way ANOVA) test groups separately, not to each other 

# pycno present, make confetti in g
pycno_half <- timepoint_consumption_change %>%
  filter(pycno == 'yes') %>%
  mutate(change = change * 0.341)
pycno_df <- as.data.frame(pycno_half)
# balance design by restricting to first 51 hours of trials
trunc_pycno <- pycno_df %>%
  filter(hours %in% c(3, 9, 21, 27, 33, 45, 51))
# pycno present summary and friedman
trunc_pycno %>%
  group_by(hours) %>%
  get_summary_stats(change, type = "common")
ggboxplot(trunc_pycno, x = "hours", y = "change", add = "jitter")
pycno_fried <- trunc_pycno %>% 
  friedman_test(change ~ hours | ID)
pycno_fried
# test effect size 
trunc_pycno %>% 
  friedman_effsize(change ~ hours |ID)

# friedman and post-hoc in one
library(PMCMRplus)
frdAllPairsNemenyiTest(change ~ hours | ID, data = trunc_pycno)


# no pycno present
nopycno_half <- timepoint_consumption_change %>%
  filter(pycno == 'no') %>%
  mutate(change = change * 0.341)
nopycno_df <- as.data.frame(nopycno_half)
# balance design by restricting to first 51 hours of trials
trunc_nopycno <- nopycno_df %>%
  filter(hours %in% c(3, 9, 21, 27, 33, 45, 51))
# no pycno present summary and friedman
trunc_nopycno %>%
  group_by(hours) %>%
  get_summary_stats(change, type = "common")
ggboxplot(trunc_nopycno, x = "hours", y = "change", add = "jitter")
nopycno_fried <- trunc_nopycno %>% 
  friedman_test(change ~ hours | ID)
nopycno_fried

# friedman and post-hoc in one
library(PMCMRplus)
frdAllPairsNemenyiTest(change ~ hours | ID, data = trunc_nopycno)


# FRIEDMAN AND NEMENY POSTHOC - nonparametric (non-normal, zero inflated, outliers)

urchin_timeseries <- trials2020_Q %>%
  unite("datetime", date:time, sep = " ")

urchin_timeseries$datetime <-
  as.POSIXlt(urchin_timeseries$datetime)

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

urchin_timeseries <- urchin_timeseries %>%
  group_by(pycno, ID) %>%
  mutate(cc = cumsum(consumed * 0.341))


trunc_pycno <- trunc_pycno %>%
  unite(urchintime, ID, hours, sep = "_", remove = FALSE)
urchin_timeseries_join <- urchin_timeseries %>%
  unite(urchintime, ID, hours, sep = "_") %>%
  ungroup() %>%
  select(-pycno)


change_cum_pycno <-   trunc_pycno %>%
  left_join(urchin_timeseries_join, by = "urchintime")


# plot of change per timepoint mean sterror, overlay with overall rate of consumption
ggplot(change_cum_pycno, aes(x = hours, y = change)) +
  geom_jitter(col = "grey", size = 3, width = 1) +
  stat_summary(
    geom = "point",
    fun = "mean",
    size = 4,
    shape = 19
  ) +
  geom_errorbar(stat="summary", fun.data="mean_se", size = 1) +
  stat_summary(aes(y = cc), fun = mean, color = "red", geom = "line") +
  scale_y_continuous(sec.axis = sec_axis(~., name = "mean cumulative grams of kelp consumed")) +
  theme_minimal()

# NO PYCNO

trunc_nopycno <- trunc_nopycno %>%
  unite(urchintime, ID, hours, sep = "_", remove = FALSE)
urchin_timeseries_join <- urchin_timeseries %>%
  unite(urchintime, ID, hours, sep = "_") %>%
  ungroup() %>%
  select(-pycno)


change_cum_nopycno <-   trunc_nopycno %>%
  left_join(urchin_timeseries_join, by = "urchintime")

# plot of change per timepoint mean sterror, overlay with overall rate of consumption
ggplot(change_cum_nopycno, aes(x = hours, y = change)) +
  geom_jitter(col = "grey", size = 3, width = 1) +
  stat_summary(
    geom = "point",
    fun = "mean",
    size = 2,
    shape = 19
  ) +
  geom_errorbar(stat="summary", fun.data="mean_se", size = 1) +
  stat_summary(aes(y = cc), fun = mean, color = "red", geom = "line") +
  scale_y_continuous(sec.axis = sec_axis(~., labels = c("", "", "0", "2", "4", "6"), name = "mean cumulative grams of kelp consumed")) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "blue") +
  theme_minimal()






# pycno and pycno trunc together in one figure

biomass_timepoint <- timepoint_consumption_change %>%
  mutate(change = change * 0.341)
# balance design by restricting to first 51 hours of trials
biomass_trunc <- biomass_timepoint %>%
  filter(hours %in% c(3, 9, 21, 27, 33, 45, 51)) %>%
  unite(urchintime, ID, hours, sep = "_", remove = FALSE)

urchin_timeseries_join <- urchin_timeseries %>%
  filter(timepoint != 0) %>%
  unite(urchintime, ID, hours, sep = "_") %>%
  ungroup() %>%
  select(-pycno)


change_cum_all <-   biomass_trunc %>%
  left_join(urchin_timeseries_join, by = "urchintime")

# plot of change per timepoint mean sterror, overlay with overall rate of consumption
ggplot(change_cum_all, aes(x = hours, y = change)) +
  geom_jitter(col = "grey", size = 3, width = 1) +
  stat_summary(
    geom = "point",
    fun = "mean",
    size = 2,
    shape = 19
  ) +
  geom_errorbar(stat="summary", fun.data="mean_se", size = 1) +
  stat_summary(aes(y = cc), fun = mean, color = "red", geom = "line") +
  scale_y_continuous(sec.axis = sec_axis(~., labels = c("", "", "0", "2", "4", "6"), name = "mean cumulative grams of kelp consumed")) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "blue") +
  facet_grid(~pycno) +
  theme_minimal()


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