---
title: "ADAPT_study3"
author: "Rondeline M. Williams"
date: '2022-07-15'
output: html_document
---

#load libraries
library(dplyr)
library(janitor)
library(ggplot2)
library(forcats)
library(ggthemes)
library(stringr)
library(tidyr)
library(rstanarm)

## Load data
data_3b <- read.csv("childrendatalog3b.csv") #load data

View(data_3b)

## Tidy data
tidydata_3b <- data_3b %>%
  clean_names() %>%  #lowercase column names
  filter(arm == "Main",
         included == "Yes") %>% #exclude pilot participants and those who could not be retained for analysis failed attention and sound checks
  select(subject_id:rationale) %>%
  na.omit()

View(tidydata_3b)

#Remove extra spaces
tidydata_3b$activity <- str_trim(tidydata_3b$activity)
tidydata_3b$sound_type <- str_trim(tidydata_3b$sound_type)
tidydata_3b$race <- str_trim(tidydata_3b$race)
tidydata_3b$race <- str_replace(tidydata_3b$race, "$/$ ", "$/$")

#Turn response column into integers
tidydata_3b$response <- as.integer(tidydata_3b$response)

## Scale Age
tidydata_3b$age_centered <- scale(joineddata_2b$age_months, scale = FALSE) #age

## Get demographic data
completedata_2b$age <- as.integer(completedata_2b$age) #age
summary(completedata_2b)

demodata_3b <- tidydata_3b %>%
  select(subject_id,race, trial) %>%
  filter(trial == 1) %>%  #get one value per participant
  mutate(race_count = ifelse((str_detect(race, ",")), "Multiracial", race)) %>% 
  count(race_count)

View(demodata_3b)

## Calculate confidence intervals
cidata_3b <- tidydata_3b %>%
  group_by(activity, sound_type) %>%
  summarise(ci.l = binom::binom.bayes(x = sum(response), n = n())$lower,
            ci.u = binom::binom.bayes(x = sum(response), n = n())$upper,
            n = n(),
            mean_response = mean(response)) %>% 
  mutate(activity = fct_reorder(activity, mean_response, .desc = TRUE))

View(cidata_3b)

cidataage_3b <- tidydata_3b %>%
  group_by(activity, sound_type, age_years) %>%
  summarise(ci.l = binom::binom.bayes(x = sum(response), n = n())$lower,
            ci.u = binom::binom.bayes(x = sum(response), n = n())$upper,
            n = n(),
            mean_response = mean(response)) %>% 
  mutate(activity = fct_reorder(activity, mean_response, .desc = TRUE))

View(cidataage_3b)

## Data visualization and stats

### Aggregated Data
ggplot(data = cidata_3b, mapping = aes(x = sound_type, y = mean_response, fill = activity)) +
  geom_col() +
  geom_linerange(aes(ymin = ci.l,
                     ymax = ci.u)) +
  ylim(0,1) +
  xlab("Sound") +
  ylab("Rating") +
  facet_wrap(~activity) +
  geom_hline(yintercept = .5, lty =2) +
  scale_color_solarized() + 
  theme_few() +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 45, hjust=1),
        text = element_text(size=10)) 

### Facet Binned
ggplot(data = cidataage_3b, mapping = aes(x = sound_type, fill = as.factor(age_years))) +
  geom_bar(aes(y = mean_response),
           position = "dodge", stat = "identity") +
  geom_linerange(aes(ymin = ci.l,
                     ymax = ci.u),
                 position = position_dodge(width = 0.9)) +
  facet_wrap(~activity) +
  xlab("Sound") +
  ylim(0,1) +
  ylab("Rating") +
  scale_color_solarized() +
  geom_hline(yintercept = .5, lty =2) +
  theme_few() +
  theme(#legend.position = "none",
        legend.text = element_text(size=15),
        axis.text.x = element_text(angle = 45, hjust=1),
        text = element_text(size=15))

###  Binned
ggplot(data = tidydata_3b, 
       mapping = aes(x = factor(age_years), y = response)) +
  geom_violin(aes(fill = factor(sound_type))) +
  geom_point(position = "jitter") +
  facet_wrap(~activity) +
  xlab("Age") +
  ylim(0,1) +
  ylab("Rating") +
  scale_color_solarized() +
  theme_few()

### Model
glmer_3b <- stan_glmer(formula = response ~ sound_type * activity * age_years + (1 | subject_id),
                       family = binomial,
                       data = tidydata_3b)

glmer_3b_summary <- summary(glmer_3b, probs = c(0.025, 0.975))
print(glmer_3b, digits = 2)

View(glmer_3b_summary)
