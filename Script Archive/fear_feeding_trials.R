#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#                                                                                ##
# Urchin-Pycno fear feeding trials                                               ##
# Data are current as of 2020-09-18                                              ##
# Data source: Ross Whippo - UO/OIMB                                             ##
# R code prepared by Ross Whippo                                                 ##
# Last updated 2022-06-15                                                        ##
#                                                                                ##
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# SUMMARY:


# Required Files (check that script is loading latest version):
# urchin_fear_feeding_pycno-trial-1.csv

# Associated Scripts:
# NA

# TO DO

# split change in consumption analysis into first half/second half

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
# 2022-06-15 Added change in consumption analysis
# 2022-10-12 Size by consumption

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# LOAD PACKAGES                                                                ####
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

library(tidyverse)
library(viridis)
library(nlme)
library(lme4)
library(vegan)
library(ggpubr)
library(pscl) # zero inflated poisson models (zero consumption of confetti)
library(boot)
library(lmerTest)

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# READ IN AND PREPARE DATA                                                     ####
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

urchin_fear_pycno <-
  read_csv('Data/2020/urchin_fear_feeding_pycno-trial-1.csv')
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
  group_by(trial, bin, tank, ID, pycno) %>%
  summarise(`total consumed` = sum(consumed)) %>%
  ungroup()



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
  theme_minimal(); size_plot

summary(urchin_size$diameter)
t.test(diameter ~ pycno, data = urchin_size)

# total consumed by each urchin across all trials

per_urchin_consumed_total %>%
  ggplot(aes(x = pycno, y = `total consumed`, fill = pycno)) +
  geom_boxplot() +
  scale_fill_viridis(discrete = TRUE, begin = 0.2, end = 0.9) +
  theme_minimal()


# FOR FHL TALK
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



per_trial_pycno <- per_urchin_consumed_total %>%
  filter(pycno == "yes")
summary(per_trial_pycno$`total consumed`)
sd(per_trial_pycno$`total consumed`)

per_trial_empty <- per_urchin_consumed_total %>%
  filter(pycno == "no")
summary(per_trial_empty$`total consumed`)
sd(per_trial_empty$`total consumed`)

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
  

# AMOUNT CONSUMED BY URCHIN SIZE

per_urchin_consumed_total_size <- urchin_fear_pycno %>%
  group_by(trial, bin, tank, ID, pycno, diameter) %>%
  summarise(`total consumed` = sum(consumed)) %>%
  ungroup()

ggplot(per_urchin_consumed_total_size, aes(x = as.numeric(diameter), y = `total consumed`, color = pycno)) +
  scale_color_viridis(discrete = TRUE, begin = 0.3, end = 0.8) +
  theme_minimal() +
  geom_point() +
  geom_smooth(method = "nls", formula = y ~ a * x + b, se = F,
              method.args = list(start = list(a = 0.1, b = 0.1)))





# TOTAL CONSUMED overall BOXPLOT
theme_set(theme_light(base_size = 18))


urchin_time %>%
    ggplot(aes(x = pycno, y = tot_consumed, fill = pycno)
  ) +
    geom_boxplot() +
    scale_fill_viridis(discrete = TRUE,
                       begin = 0.5,
                       end = 0.9,
                       option = "magma") +
    labs(y = "Pieces of Kelp Consumed")
  
urchin_time %>%
  group_by(pycno, trial) %>%
  summarise(total = sum(consumed)) %>%
  summarise(mean(total))
  
# ALGAE CONSUMED USING BIOMASS MEASUREMENTS CONFETTI WEIGHTS

# Algal consumption plots

# based on field notes from 2020-09-13 - confetti diameter = 21mm

# > pi*((2.1/2)^2)
# [1] 3.463606

confetti_weights <- read_csv("Data/2020/confetti_weights.csv", 
                             col_types = cols(trial = col_character()))
confetti_weights <- confetti_weights %>%
  rename("beforeafter" = "before-after")

# is weight different before and after? 
weightmix <- lme(weight_mg ~ beforeafter, random = ~1|trial/tank, data = confetti_weights)
summary(weightmix)
weightmix
anova(weightmix)


confetti_weights %>%
  filter(beforeafter == "before") %>%
  summarise(mean(weight_mg))

confetti_weights %>%
  filter(beforeafter == "after") %>%
  summarise(mean(weight_mg))

#  A tibble: 1 x 1
# `mean(weight_mg)`
# <dbl>
#   1              339.

# > 339/3.463606
# [1] 97.87487 mg per square cm
# each piece of confetti ~ 339mg or .339 g

mean(confetti_weights$weight_mg)

  
  
    
  per_exp_pycno <- urchin_time %>%
    select(-datetime) %>%
    filter(pycno == "yes")
  summary(per_exp_pycno$tot_consumed)
  sd(per_exp_pycno$tot_consumed)
  
  per_exp_empty <- urchin_time %>%
    select(-datetime) %>%
    filter(pycno == "no")
  summary(per_exp_empty$tot_consumed)
  sd(per_exp_pycno$tot_consumed)
  
  
  # change unknown diameters to 61
  
  urchin_timeseries$diameter <- urchin_timeseries$diameter %>%
    recode("nd" = "61")
  
  theme_set(theme_light(base_size = 18))
  
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
  

  
  hour_line 

  
  
  
  
# THIS IS THE CUMULATIVE CONFETTI FIGURE:
theme_set(theme_light(base_size = 18))  
  
ggplot(
    urchin_timeseries %>%
      group_by(pycno, ID) %>%
      mutate(cc = cumsum(consumed)),
    aes(
      x = Hours, # as.POSIXct(datetime),
      y = cc,
      color = pycno
    )
  ) +
    scale_color_viridis(discrete = TRUE,
                        begin = 0.5,
                        end = 0.9,
                        option = "magma") +
    geom_point(size = 5) +
    geom_line(aes(group = ID), alpha = 0.25) +
    geom_smooth(method = "lm", size = 2) +
    scale_y_continuous(name = "Pieces of Kelp Consumed")

# cumulative confetti with mean per time point

# FOR FHL TALK

urchin_timeseries %>%
  group_by(pycno, ID) %>%
  mutate(cc = cumsum(consumed) * 0.339) %>%
  mutate(Treatment = ifelse(pycno == 'yes', "Pycno", "Control")) %>%
  ggplot(aes(x = Hours, y = cc, color = Treatment)) +
  scale_color_viridis(discrete = TRUE,
                      begin = 0.3,
                      end = 0.7,
                      option = "magma") +
  geom_line(aes(group = ID), alpha = 0.25, lwd = 1) +
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

# new timeseries using slope and talk aesthetics

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

urchin_timeseries_lm <- urchin_timeseries%>%
  group_by(pycno, ID) %>%
  mutate(cc = cumsum(consumed) * 0.339) %>%
  mutate(Treatment = ifelse(pycno == 'yes', "Pycno", "Control")) 
  
# a model based on this data:

mod_1 <- lm(`total consumed` ~ pycno, data = per_urchin_consumed_total)
summary(mod_1)
mod_1

t.test(`total consumed` ~ pycno, data = per_urchin_consumed_total)





###### WSN STATS using lmerTest for repeated measures and p values


# repeated measures
# need to create dataset with cumulative consumption at each timepoint per urchin
cum_per_urchin <- urchin_fear_pycno %>%
  mutate(Treatment = ifelse(pycno == 'yes', "Pycno", "Control")) %>%
  group_by(ID) %>%
  mutate("Cumulative Consumption" = cumsum(consumed)) %>%
  mutate(ID = as.factor(ID),
         tank = as.factor(tank),
         pycno = as.factor(pycno)) %>%
  ungroup()
# remove timepoint zero
cum_nozero <- cum_per_urchin %>%
  filter(timepoint != 0)
str(cum_nozero)
cum_nozero <- cum_nozero %>%
  mutate(Cumulative = `Cumulative Consumption`)
cum_nozero <- cum_nozero %>%
    mutate(non_zero = ifelse(Cumulative > 0, 1, 0))


100*sum(cum_nozero$non_zero == 0)/nrow(cum_nozero)

sum(cum_nozero$non_zero != 0)
max(cum_nozero$Cumulative)

 # check for normal response
ggplot(cum_nozero, aes(Cumulative)) + geom_histogram(bins = 145) # data are right skewed (need ZIP model)
# cube root the consumption
cum_nozero$`Cumulative Consumption` <- (1 + cum_nozero$`Cumulative Consumption`)^(1/3)


summary(lmer(Cumulative ~ pycno + timepoint + (1 + timepoint | ID), data=cum_nozero))













# This finds the correlation coefficient 1 and -1 = strong relationship
coefficient <- cor.test(urchin_timeseries_lm$cc, urchin_timeseries_lm$Hours)
coefficient$estimate

# look for outliers
urchin_timeseries_lm %>%
  ggplot(aes(cc, pycno)) +
  geom_boxplot()

mod_2 <- lm(`cc` ~ pycno, data = urchin_timeseries_lm)
summary(mod_2)


  # mean number of confetti consumed per time point per treatment

theme_set(theme_light(base_size = 18))    

ggplot(urchin_timeseries, aes(x = pycno, y = consumed, fill = pycno)) +
    scale_fill_viridis(discrete = TRUE,
                       begin = 0.5,
                       end = 0.9,
                       option = "magma") +
    geom_boxplot() +
    facet_grid(. ~ hours) +
  scale_y_continuous(name = "Pieces of Kelp Consumed") +
  theme(axis.text.x=element_blank())
  
  
  
  # total confetti eaten per treatment
  urchin_fear_pycno %>%
    group_by(treatment) %>%
    summarise(sum(consumed))
  
  # total confetti eaten per urchin per treatment and plot
  perurchin <- urchin_fear_pycno %>%
    group_by(pycno, ID) %>%
    summarise(total = sum(consumed))
  
  theme_set(theme_light(base_size = 18))
  
  # function to make SE in boxplot
  MinMeanSEMMax <- function(x) {
    v <- c(min(x), mean(x) - sd(x)/sqrt(length(x)), mean(x), mean(x) + sd(x)/sqrt(length(x)), max(x))
    names(v) <- c("ymin", "lower", "middle", "upper", "ymax")
    v
  }
  
  
  
 perurchin %>%
    ggplot(aes(x = pycno, y = total*341, fill = pycno)
    ) +
   stat_summary(fun.data=MinMeanSEMMax, geom="boxplot") + 
    scale_fill_viridis(discrete = TRUE,
                       begin = 0.5,
                       end = 0.9,
                       option = "magma") +
    labs(y = "Estimated Kelp Consumed (mg)")
  
# calculating total change in consumption across trials
  timepoint_consumption_change <- urchin_fear_pycno %>%
    group_by(ID, pycno) %>%
    summarise(change = diff(consumed))      
# average change per treatment
  timepoint_consumption_change %>%
    group_by(pycno) %>%
    summarise(mean_change = mean(change))

# t.test for mean increase in consumption across timepoints by treatment
  t.test(change ~ pycno, data = timepoint_consumption_change)
  
  
  
  
  
  # DUMMY DATA
ID <- c(rep("A",7), rep("B", 7))
timepoint <- c(rep(seq(from = 0, to = 6),2))
set.seed(1234)
consumed <- sample.int(n = 6,
                       size = 14,
                       replace = TRUE)
df <- tibble(ID, timepoint, consumed)
df[1,3] <- 0
df[8,3] <- 0    
df    

df %>%
  group_by(ID) %>%
  summarise(diff(consumed))
  
    
    
    
    
  
  
  
  
  
  
  
  
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  # MANIPULATE DATA - STATS                                                      ####
  #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  
  urchin_fear_mix_data <- urchin_fear_pycno %>%
    filter(timepoint != '0')
  
  # is total amount of confetti consumed per trial different between groups?
  t.test(`total consumed` ~ pycno, data = per_urchin_consumed_total)
  
  # is amount of confetti consumed per time point different between groups (w/size)?
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


  # combine trial and bin to prevent combining urchins across trials
  urchin_fear_pycno <-
    unite(urchin_fear_pycno,
          'ID',
          trial,
          bin,
          sep = "_",
          remove = FALSE)
  
         