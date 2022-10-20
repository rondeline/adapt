title: "ADAPT_study2_dataanalysis"
author: "Rondeline M. Williams"
date: 
  output: html_document
---
  
#Load libraries
library(dplyr)
library(janitor)
library(ggplot2)
library(tidyr)
library(stringr)
library(rstanarm)
library(binom)
library(forcats)

#Load data
read_data <- read.csv("adapt_study2_preference.csv") %>% 
  clean_names()
answers <- read.csv("adapt_study2_answers.csv") %>% 
  clean_names()

answers$activity <- tolower(str_trim(answers$activity))

#Tidy Data
selectrename <- read_data %>% 
  select(q6:experimental_session_dance_do) %>% #remove unnecessary columns 
  rename("sound_check" = "q6") #rename one of the columns

remove1strow <- selectrename[-c(1),] #remove 1st row

tidydata <- remove1strow %>% 
  mutate(subject_id = row_number()) %>% #add subject ids
  select(subject_id, everything(),
         -c("read_justification", 
            "talk_justification",
            "build_justification",
            "eat_justification",
            "dance_justification",
            "paint_justification",
            "learn_justification",
            "zerpie_justification",
            "reflection",
            "prolific_pid")) %>% #move subject id column to the front and remove other columns
  filter(attention_check_1 == "Fish",
         attention_check_2 == "Ryan") %>% #remove participants who failed attention checks 
  pivot_longer(cols = c(read, build, eat, talk, dance, paint, learn, zerpie),
               names_to = "activity",
               values_to = "response") %>%
  separate(col = experimental_session_build_do,
           into = c("a", "build"),
           sep = "[|]") %>% 
  separate(col = experimental_session_read_do,
           into = c("b", "read"),
           sep = "[|]") %>% 
  separate(col = experimental_session_learn_do,
           into = c("c", "learn"),
           sep = "[|]") %>% 
  separate(col = experimental_session_zerpie_do,
           into = c("d", "zerpie"),
           sep = "[|]") %>% 
  separate(col = experimental_session_paint_do,
           into = c("e", "paint"),
           sep = "[|]") %>% 
  separate(col = experimental_session_talk_do,
           into = c("f", "talk"),
           sep = "[|]") %>% 
  separate(col = experimental_session_dance_do,
           into = c("g", "dance"),
           sep = "[|]") %>% 
  separate(col = experimental_session_eat_do,
           into = c("h", "eat"),
           sep = "[|]") %>% 
  select(-c("a",
            "b",
            "c",
            "d",
            "e",
            "f",
            "g",
            "h")) %>% #remove "explanation" columns
  pivot_longer(read:dance,
               names_to = "activity_label",
               values_to = "snr") %>% 
  filter(activity == activity_label) %>% 
  pivot_longer(read_rationale:dance_rationale,
               names_to = "activity_rationale",
               values_to = "rationale") %>% 
  separate(col = activity_rationale,
           into = c("activity_rationale", "remove"),
           sep = "_") %>% 
  filter(activity == activity_rationale) %>% 
  select(-c("remove", "activity_rationale", "activity_label")) #remove "remove" column

#change snr character type to integer
tidydata$snr <- as.integer(tidydata$snr)
tidydata$age <- as.integer(tidydata$age)

summary(tidydata)

#Join data and answers columns 
leftjoin <- left_join(tidydata, answers, by = c("activity", "snr"))

#Combine louder and quieter columns
combinelouderquieter <- leftjoin %>% 
  pivot_longer(louder:quieter,
               names_to = "volume",
               values_to = "value") %>% 
  filter(response == value)

summary(combinelouderquieter)

#Count the number of louder vs. quieter responses
volumecount <- combinelouderquieter %>% 
  group_by(activity) %>% 
  count(volume)

#### RAW BY ACTIVITY

#Proportion of volume by activity
volume_by_activity <- combinelouderquieter %>% 
  group_by(activity) %>% 
  mutate(prop_quieter = (ifelse(volume == "quieter", 1,0))) %>%
  summarise(ci.l = binom.bayes(x = sum(prop_quieter), n = n())$lower,
          ci.u = binom.bayes(x = sum(prop_quieter), n = n())$upper,
          n = n(),
          prop_quieter = mean(prop_quieter)) %>%
  mutate(activity = fct_reorder(activity, prop_quieter, .desc = TRUE))

#Data Visualization
#Bar graph for proportion of volume by activity
ggplot(data = volume_by_activity, 
       mapping = aes(x = activity, 
                     y = prop_quieter, fill = activity)) +
  geom_col() +
  geom_linerange(aes(ymin = ci.l,
                     ymax = ci.u)) + 
  xlab("Activity") + 
  ylab("Proportion choosing quieter") + 
  ylim(0,1) +
  geom_hline(yintercept = .5, lty =2) + 
  ggthemes::scale_color_solarized() + 
  ggthemes::theme_few()

ggsave("adapt_study2_adults_plot.png", plot = last_plot())

#### SPLIT BY SNR

#Proportion of volume by activity
volume_by_activity <- combinelouderquieter %>% 
  group_by(activity, snr) %>% 
  mutate(prop_quieter = (ifelse(volume == "quieter", 1,0))) %>%
  summarise(ci.l = binom.bayes(x = sum(prop_quieter), n = n())$lower,
            ci.u = binom.bayes(x = sum(prop_quieter), n = n())$upper,
            n = n(),
            prop_quieter = mean(prop_quieter)) %>%
  mutate(activity = fct_reorder(activity, prop_quieter, .desc = TRUE))

#Data Visualization
#Bar graph for proportion of volume by activity
ggplot(data = volume_by_activity, 
       mapping = aes(x = snr, 
                     y = prop_quieter)) +
  geom_pointrange(aes(ymin = ci.l,
                     ymax = ci.u)) + 
  geom_smooth(method = "lm") +
  xlab("Activity") + 
  ylab("Proportion choosing quieter") + 
  ylim(0,1) +
  geom_hline(yintercept = .5, lty =2) + 
  ggthemes::scale_color_solarized() + 
  ggthemes::theme_few() + 
  facet_wrap(~activity)
