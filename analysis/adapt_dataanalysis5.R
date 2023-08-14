---
  title: "ADAPT_study5"
author: "Rondeline M. Williams"
date: '2023-08-09'
output: html_document
---
  
# Load libraries
library(here)
library(tidyverse)
library(dplyr)
library(janitor)
library(stringr)
library(rstanarm)
library(ggplot2)
library(ggthemes)
library(wesanderson)
library(na.tools)

# Load data
data_5 <- read.csv(here("data", "childrendatalog5.csv"))

# Tidy data
tidydata_5 <- data_5 %>%
  clean_names() %>% #lowercase column names
  filter(arm == "Pilot_B",
         included == "Yes") %>% #exclude pilot participants and those who could not be retained for analysis failed attention and sound checks
  select(subject_id:rationale) %>% 
  na.omit(response)

## Remove extra spaces
tidydata_5$activity <- str_trim(tidydata_5$activity)
tidydata_5$sound_level_key <- str_trim(tidydata_5$sound_level_key)
tidydata_5$race <- str_trim(tidydata_5$race)
tidydata_5$race <- str_replace(tidydata_5$race, "$/$ ", "$/$")

## Turn response column into integers
tidydata_5$response <- as.integer(tidydata_5$response)

## Scale Age
tidydata_5$age_centered <- scale(tidydata_5$age_months, scale = FALSE) #age

## Get demographic data
tidydata_5$age_months <- as.integer(tidydata_5$age_months) #age

demodata_5 <- tidydata_5 %>%
  select(subject_id, race, trial) %>%
  filter(trial == 1) %>%  #get one value per participant
  mutate(race_count = ifelse((str_detect(race, ",")), "Multiracial", race)) %>% 
  count(race_count)

## Add condition and make sound level numeric
cleandata_5 <- tidydata_5 %>%
  separate(col = condition,
           into = c("order", "condition"),
           sep = 2) %>% #Split condition into order and condition
  mutate(sound_level = case_when(response == 1 ~ 0,
                                 response == 2 ~ 0,
                                 response == 3 ~ 1,
                                 response == 4 ~ 1))
#Calculate confidence intervals
cidata_5 <- cleandata_5 %>%
  group_by(activity, condition) %>%
  summarise(mean_soundlevel = mean(sound_level),
            se = 1.96*(sd(sound_level)/sqrt(n())),
            n = n())

#Visualization
##lm
ggplot(data = cleandata_5, mapping = aes(x = age_months, y = sound_level, col = condition)) +
  geom_point(position = "identity") +
  geom_smooth(method = "lm") + 
  facet_wrap(~activity) +
  theme_few()

##bar
xlabels <- c("successful", "unsuccessful")
facet_labels <- c(fraw = "Fraw: story before bed",
                  gobb = "Gobb: something to do",
                  plip = "Plip: spin to the beat",
                  terb = "Terb: hide tummy noises")

ggplot(data = cidata_5, mapping = aes(x = condition, fill = condition)) +
  geom_bar(aes(y = mean_soundlevel),
           position = "dodge",
           stat = "identity") +
  geom_errorbar(data = subset(cidata_5, condition == "S"),
                aes(ymin = mean_soundlevel - se,
                    ymax = mean_soundlevel + se),
                width=0.4,
                size=0.3,
                col = "orange") +
  scale_x_discrete(labels = xlabels) +
  xlab("Goal") +
  ylab("Proportion choosing dynamic sound") +
  facet_wrap(~activity) +
  theme_few() +
  theme(legend.position = "none",
        axis.text.x = element_text(size = 8))

# Aggregated Data
ggplot(data = tidydata_5, mapping = aes(x = sound_type, y = mean_response, fill = activity)) +
  geom_col() +
  geom_linerange(aes(ymin = ci.l,
                     ymax = ci.u)) +
  ylim(0,1) +
  xlab("Auditory Stimulus") +
  ylab("Rating") +
  scale_fill_manual(values = wes_palette("GrandBudapest1")) +
  scale_x_discrete(labels = xlabels) +
  facet_wrap(~activity) +
  geom_hline(yintercept = .5, lty =2) +
  scale_color_solarized() + 
  theme_few() +
  theme(legend.position = "none",
        title = element_text(size = 8),
        axis.text.x = element_text(angle = 45, hjust=1),
        text = element_text(size=10)) +
  labs(title = "Preschool children discriminate optimal auditory environments based on goals")
