---
  title: "ADAPT_dataanalysis"
author: "Rondeline M. Williams"
date: 
  output: html_document
---
  
  #Load libraries
library(dplyr)
library(janitor)
library(ggplot2)
library(rstanarm)
library(scales)
library(bayesplot)
library(bayestestR)
library(ggthemes)
library(tidyr)
library(binom)
library(lme4)

#Load data
data <- read.csv("adultdatalog1a.csv") #load data
answers <- read.csv("adultanswerlog1a.csv") %>% #load answers
  clean_names()

#Tidy data
tidy_data <- data %>%
  clean_names() %>% 
  select(q6:q4) %>% 
  rename("sound_check" = "q6",
         "debrief" = "q39",
         "loudness_type" = "q49",
         "age" = "q1",
         "gender" = "q3",
         "race" = "q2",
         "english" = "q4",
         "25" = "ij_test",
         "15" = "gh_test",
         "20" = "ef_test",
         "5" = "cd_test",
         "10" = "ab_test")

#Remove first two rows (all junk)
tidy_data <- tidy_data[-c(1,2), ]

#Filter out those who failed the attention check, add a subject id column, and make it first
tidy_data <- tidy_data %>% 
  filter(attention_check_1 == "Madras Paneer") %>% 
  mutate(subject_id = row_number()) %>% 
  select(subject_id, everything())

#Pivot longer and make both snr and response numeric
tidy <- tidy_data %>% 
  filter(subject_id != 2 & subject_id != 3) %>% 
  pivot_longer(cols = c("5", "10", "15", "20", "25"),
               names_to = "snr",
               values_to = "response") %>% 
  mutate(snr = as.numeric(snr),
         response = as.numeric(response))

#Join the two dfs together
full <- left_join(tidy, answers, by = "snr")

View(full)


#Add a column for figuring out whether participants answered correctly
clean_data <- full %>%
  group_by(snr) %>% 
  mutate(correct = case_when(response == answer ~ 1,
                             response != answer ~ 0)) %>% 
  na.omit() 

View(clean_data)

clean_data$age <- as.integer(clean_data$age)
summary(clean_data)

count_race <- clean_data %>% 
  count(race)

View(count_race)

#Data Visualization
  
#DF for CIs
ci_data <- clean_data %>%
  group_by(snr) %>%
  summarise(ci.l = binom::binom.bayes(x = sum(correct), n = n())$lower,
            ci.u = binom::binom.bayes(x = sum(correct), n = n())$upper,
            n = n(),
            mean_correct = mean(correct))

View(ci_data)

ggplot(data = ci_data, mapping = aes(x = snr, y = mean_correct, fill = snr)) +
  geom_col() +
  geom_linerange(aes(ymin = ci.l,
                     ymax = ci.u)) +
  ylim(0,1) +
  xlab("SNR") +
  ylab("Proportion of Correct Responses") +
  geom_hline(yintercept = .5, lty =2) + 
  ggthemes::scale_color_solarized() + 
  ggthemes::theme_few()

ggsave("adapt_study1_adults.png", plot = last_plot())

#Logistic Regression

discrimination_glmer <- stan_glmer(correct ~ snr + (1|subject_id),
                                   data = clean_data,
                                   family = binomial)

summary(discrimination_glmer)
