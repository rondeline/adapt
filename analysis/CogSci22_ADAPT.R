#Load Libraries
library(ggplot2)
library(ggthemes)
library(janitor)
library(tidyverse)
library(rstanarm)
library(reprex)

# Experiment 1a

## Load data
data_1a <- read.csv("adultdatalog1a.csv") #load data
answers_1a <- read.csv("adultanswerlog1a.csv") #load answers

## Clean data
cleandata_1a <- data_1a %>% 
  clean_names() %>% #lowercase column names
  select(q6:q4) %>% #remove unnecessary columns
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
         "10" = "ab_test")  #rename all columns to something I understand 

## Remove first two rows (contain no data/are unnecessary)
cleandata_1a <- cleandata_1a[-c(1,2), ]
  
## Tidy data
tidydata_1a <- cleandata_1a %>% 
  filter(attention_check_1 == "Madras Paneer",
         sound_check == "Yes") %>% #exclude participants who failed attention and sound checks
  mutate(subject_id = row_number()) %>% #make a subject id column
  select(subject_id, everything()) %>%  #put subject id column first
  pivot_longer(cols = c("5", "10", "15", "20", "25"),
               names_to = "snr",
               values_to = "response") %>% #move all SNR values to a single column
  mutate(snr = as.numeric(snr),
         response = as.numeric(response)) #make SNR values and participant responses numeric for calculation

## Join tidydata_1a with answer log
joineddata_1a <- left_join(tidydata_1a, answers, by = "snr")

## Measure performance 
completedata_1a <- joineddata_1a %>% 
  group_by(snr) %>% 
  mutate(correct = case_when(response == answer ~ 1,
                             response != answer ~ 0)) %>%  #add a correct column 
  na.omit() #remove any NA values

## Get demographic data
completedata_1a$age <- as.integer(completedata_1a$age) #age
summary(completedata_1a)

demodata_1a <- completedata_1a %>%
  ungroup(snr) %>% 
  select(subject_id,race, snr) %>%
  filter(snr == 5) %>% 
  mutate(race_count = ifelse((str_detect(race, ",")), "Multiracial", race)) %>% 
  count(race_count)

## Calculate confidence intervals
cidata_1a <- completedata_1a %>%
  group_by(snr) %>%
  summarise(ci.l = binom::binom.bayes(x = sum(correct), n = n())$lower,
            ci.u = binom::binom.bayes(x = sum(correct), n = n())$upper,
            n = n(),
            mean_correct = mean(correct))

## Data visualization and stats

### Figure 2
ggplot(data = cidata_1a, mapping = aes(x = snr, y = mean_correct, fill = snr)) +
  geom_col() +
  geom_linerange(aes(ymin = ci.l,
                     ymax = ci.u)) +
  ylim(0,1) +
  xlab("SNR") +
  ylab("Proportion of Correct Responses") +
  geom_hline(yintercept = .5, lty =2) + 
  scale_color_solarized() + 
  theme_few() +
  theme(legend.position = "none") 

ggsave("figure2.png", plot = last_plot())

### Logistic regression
completedata_1a$snr_centered <- scale(completedata_1a$snr, scale = FALSE)

glmer_1a <- stan_glmer(formula = correct ~ snr_centered + (snr_centered | subject_id),
           family = binomial,
           data = completedata_1a)

summary(glmer_1a, probs = c(0.025, 0.975))
print(glmer_1a)

# Experiment 1b

## Load data
data_1b <- read.csv("childrendatalog1b.csv") #load data
answers_1b <- read.csv("childrenanswerlog1b.csv") %>%  #load answers
  clean_names()
  
## Clean data
cleandata_1b <- data_1b %>% 
  clean_names() #lowercase column names

## Tidy data
tidydata_1b <- cleandata_1b %>% 
  filter(arm == "Main",
         included == "Yes") #exclude pilot participants and those who could not be retained for analysis failed attention and sound checks
  
## Join tidydata_1b with answer log
joineddata_1b <- left_join(tidydata_1b, answers_1b, by = c("condition", "trial"))

## Scale SNR and Age
joineddata_1b$snr_centered <- scale(joineddata_1b$snr, scale = FALSE) #snr
joineddata_1b$age_centered <- scale(joineddata_1b$age_in_months, scale = FALSE) #age

## Measure performance
completedata_1b <- joineddata_1b %>% 
  group_by(snr) %>% 
  mutate(correct = case_when(response == answer ~ 1,
                             response != answer ~ 0)) %>%  #add a correct column 
  na.omit() #remove any NA values

## Get demographic data
completedata_1b$age <- as.integer(completedata_1b$age) #age

demodata_1b <- completedata_1b %>%
  ungroup(snr) %>% 
  select(subject_id,race, snr) %>%
  filter(snr == 5) %>% 
  mutate(race_count = ifelse((str_detect(race, ",")), "Multiracial", race)) %>% 
  count(race_count) #race

## Calculate confidence intervals
cidata_1b <- completedata_1b %>%
  group_by(snr) %>%
  summarise(ci.l = binom::binom.bayes(x = sum(correct), n = n())$lower,
            ci.u = binom::binom.bayes(x = sum(correct), n = n())$upper,
            n = n(),
            mean_correct = mean(correct))
  
## Data visualization and stats

### Figure 3
ggplot(data = cidata_1b, mapping = aes(x = snr, y = mean_correct, fill = snr)) +
  geom_col() +
  geom_linerange(aes(ymin = ci.l,
                     ymax = ci.u)) +
  ylim(0,1) +
  xlab("SNR") +
  ylab("Proportion of Correct Responses") +
  geom_hline(yintercept = .5, lty =2) + 
  scale_color_solarized() + 
  theme_few() +
  theme(legend.position = "none") 

ggsave("figure3.png", plot = last_plot())

### Figure 4
ggplot(data = completedata_1b, mapping = aes(age_centered, y = correct, col = as.factor(snr_centered))) +
  geom_point() +
  geom_smooth(method = "glm", 
              method.args = list(family = "binomial"),
              se = FALSE) +
  xlab("Age (Centered)") +
  ylab("Responses") +
  scale_color_solarized(name = "SNR (Centered)") + 
  theme_few()

ggsave("figure4.png", plot = last_plot())
                          
### Logistic regression
glmer_1b <- stan_glmer(formula = correct ~ age_in_years * snr_centered + (snr_centered | subject_id),
           family = binomial,
           data = completedata_1b)

summary(glmer_1b, probs = c(0.025, 0.975))
print(glmer_1b, digits = 2)

# Experiment 2a

## Load data
data_2a <- read.csv("adultdatalog2a.csv") #load data
answers_2a <- read.csv("adultanswerlog2a.csv") %>% #load answers
  clean_names()

## Clean data
cleandata_2a <- data_2a %>% 
  clean_names() %>% #lowercase column names
  select(q6:experimental_session_dance_do) %>% #remove unnecessary columns
  rename("sound_check" = "q6")  #rename column to something I understand

answers_2a$activity <- tolower(answers_2a$activity) #change activity column to lowercase
answers_2a$activity <- tolower(str_trim(answers_2a$activity)) #remove extra spaces
answers_2a$louder <- tolower(answers_2a$louder) #change louder column to lowercase
answers_2a$quieter <- tolower(answers_2a$quieter) #change quieter column to lowercase

## Remove first row (contains no data/are unnecessary)
cleandata_2a <- cleandata_2a[-c(1,2), ]

## Tidy data
tidydata_2a <- cleandata_2a %>% 
  filter(attention_check_1 == "Fish",
         attention_check_2 == "Ryan",
         sound_check == "Yes") %>% #exclude participants who failed attention and sound checks
  mutate(subject_id = row_number()) %>% #make a subject id column
  select(subject_id, everything()) %>% #put subject id column first
  select(-c(read_justification,
            build_justification,
            paint_justification,
            talk_justification,
            zerpie_justification,
            learn_justification,
            dance_justification,
            eat_justification,
            reflection,
            prolific_pid,
            fl_21_do)) %>%  #remove more unnecessary columns
  pivot_longer(cols = c(read, build, eat, talk, dance, paint, learn, zerpie),
               names_to = "activity",
               values_to = "response") %>%   #move all activities to a single column
  separate(col = experimental_session_build_do,
           into = c(NA, "build", NA, NA),
           sep = '[|]') %>%
  separate(col = experimental_session_read_do,
           into = c(NA, "read", NA, NA),
           sep = '[|]') %>%
  separate(col = experimental_session_paint_do,
           into = c(NA, "paint", NA, NA),
           sep = '[|]') %>%
  separate(col = experimental_session_learn_do,
           into = c(NA, "learn", NA, NA),
           sep = '[|]') %>%
  separate(col = experimental_session_zerpie_do,
           into = c(NA, "zerpie", NA, NA),
           sep = '[|]') %>%
  separate(col = experimental_session_talk_do,
           into = c(NA, "talk", NA, NA),
           sep = '[|]') %>%
  separate(col = experimental_session_dance_do,
           into = c(NA, "dance", NA, NA),
           sep = '[|]') %>%
  separate(col = experimental_session_eat_do,
           into = c(NA, "eat", NA, NA),
           sep = '[|]') %>% #extract SNR values from display order
  pivot_longer(cols = c(read, build, eat, talk, dance, paint, learn, zerpie),
               names_to = "activity_label",
               values_to = "snr") #extract SNR values into their own column

## Join tidydata_2a with answer log
tidydata_2a$response<- tolower(tidydata_2a$response) #change response column to lowercase

tidydata_2a$snr <- as.integer(tidydata_2a$snr) #make snr values in tidydata_2a integers

joineddata_2a <- left_join(tidydata_2a, answers_2a, by = c("snr", "activity"))

## Combine louder and quieter columns for analysis
completedata_2a <- joineddata_2a %>% 
  pivot_longer(cols = c("louder","quieter"),
               names_to = "volume",
               values_to = "value") %>%
  filter(response == value,
         activity == activity_label) %>% 
  mutate(quiet = ifelse(volume == "quieter", 1, 0),
         room = ifelse(value == "a", 1, 2)) %>%  #recode volume data
  na.omit() #remove any NA values

## Get demographic data
completedata_2a$age <- as.integer(completedata_2a$age) #age

demodata_2a <- completedata_2a %>%
  select(subject_id,race) %>%
  mutate(race_count = ifelse((str_detect(race, ",")), "Multiracial", race)) %>% 
  count(race_count)

## Calculate confidence intervals
cidata_2a <- completedata_2a %>%
  group_by(activity) %>% 
  summarise(ci.l = binom::binom.bayes(x = sum(quiet), n = n())$lower,
            ci.u = binom::binom.bayes(x = sum(quiet), n = n())$upper,
            n = n(),
            mean_quiet = mean(quiet)) %>% 
  mutate(activity = fct_reorder(activity, mean_quiet, .desc = TRUE))

## Data visualization and stats

### Figure 5
ggplot(data = cidata_2a, mapping = aes(x = activity, y = mean_quiet, fill = activity)) +
  geom_col() +
  geom_linerange(aes(ymin = ci.l,
                     ymax = ci.u)) +
  ylim(0,1) +
  xlab("Activity") +
  ylab("Proportion Choosing Quieter Room") +
  geom_hline(yintercept = .5, lty =2) + 
  scale_color_solarized() + 
  theme_few() +
  theme(legend.position = "none")

ggsave("figure5.png", plot = last_plot())

### Logistic regression

glmer_2a <- stan_glmer(formula = quiet ~ activity + (activity | subject_id),
                       family = binomial,
                       data = completedata_2a)

View(summary(glmer_2a, probs = c(0.025,0.975)))
print(glmer_2a)

# Experiment 2b

## Load data
data_2b <- read.csv("childrendatalog2b.csv") #load data
answers_2b <- read.csv("childrenanswerlog2b.csv") %>%  #load answers
  clean_names()

## Clean data
cleandata_2b <- data_2b %>% 
  clean_names() #lowercase column names

## Tidy data
tidydata_2b <- cleandata_2b %>% 
  filter(arm == "Main",
         included == "Yes") #exclude pilot participants and those who could not be retained for analysis failed attention and sound checks

## Join tidydata_2b with answer log
joineddata_2b <- left_join(tidydata_2b, answers_2b, by = c("condition", "trial"))

## Scale Age
joineddata_2b$age_centered <- scale(joineddata_2b$age_months, scale = FALSE) #age

## Measure performance
completedata_2b <- joineddata_2b %>% 
  pivot_longer(cols = c("louder","quieter"),
               names_to = "volume",
               values_to = "room") %>%
  filter(response == room) %>% 
mutate(quiet = ifelse(volume == "quieter", 1, 0)) %>%  #recode volume data
na.omit() #remove any NA values

## Get demographic data
completedata_2b$age <- as.integer(completedata_2b$age) #age
summary(completedata_2b)

demodata_2b <- completedata_2b %>%
  select(subject_id,race, activity) %>%
  filter(activity == "build") %>%  #get one value per participant
  mutate(race_count = ifelse((str_detect(race, ",")), "Multiracial", race)) %>% 
  count(race_count)

## Calculate confidence intervals
cidata_2b <- completedata_2b %>%
  group_by(activity) %>%
  summarise(ci.l = binom::binom.bayes(x = sum(quiet), n = n())$lower,
            ci.u = binom::binom.bayes(x = sum(quiet), n = n())$upper,
            n = n(),
            mean_quiet = mean(quiet)) %>% 
  mutate(activity = fct_reorder(activity, mean_quiet, .desc = TRUE))

## Data visualization and stats

### Figure 6
ggplot(data = cidata_2b, mapping = aes(x = activity, y = mean_quiet, fill = activity)) +
  geom_col() +
  geom_linerange(aes(ymin = ci.l,
                     ymax = ci.u)) +
  ylim(0,1) +
  xlab("Activity") +
  ylab("Proportion Choosing Quieter Room") +
  geom_hline(yintercept = .5, lty =2) + 
  scale_color_solarized() + 
  theme_few() +
  theme(legend.position = "none") 

ggsave("figure6.png", plot = last_plot())


### Figure 7
ggplot(data = completedata_2b, mapping = aes(x = age_centered, y = quiet, col = as.factor(activity))) +
  geom_point() +
  geom_smooth(method = "glm", 
              method.args = list(family = "binomial"),
              se = FALSE) +
  xlab("Age (Centered)") +
  ylab("Responses") +
  scale_color_solarized(name = "Activity") + 
  theme_few()

### Logistic regression
glmer_2b <- stan_glmer(formula = quiet ~ activity * age_centered + ( 1 | subject_id),
                       family = binomial,
                       data = completedata_2b)

View(completedata_2b)

View(summary(glmer_2b, probs = c(.025, .975)))
