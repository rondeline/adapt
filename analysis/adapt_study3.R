#Library
library(tidyverse)
library(janitor)
library(rstanarm)
library(na.tools)
library(here)

#Adult Data
#Load data
data_3a <- read.csv("adultdatalog3a.csv", na.strings = c("", "NA"))

View(data_3a)

## Clean data
cleandata_3a <- data_3a %>% 
  clean_names() %>% #lowercase column names
  select(q6:language_1) %>% #remove unnecessary columns
  rename("sound_check" = "q6")

## Remove first two rows (contain no data/are unnecessary)
cleandata_3a <- cleandata_3a[-c(1,2), ]

tidydata_3a <- cleandata_3a %>% 
  filter(attention_check_1 == 1,
         attention_check_2 == 1,
         sound_check == 1) %>% #exclude participants who failed attention and sound checks
  mutate(subject_id = row_number()) %>% #make a subject id column
  select(subject_id, everything())

View(tidydata_3a)

tidydata_3a$age <- as.integer(tidydata_3a$age)
tidydata_3a$race <- as.integer(tidydata_3a$race, na.omit)

count(tidydata_3a$race)

summary(tidydata_3a, decimal = 2)

tidydata_4 <- tidydata_3a %>%
  pivot_longer(cols = c("nap_vocal", "dance_vocal", "talk_vocal", "read_vocal", "party_vocal", "puppet_vocal", "learn_vocal", "brush_vocal",
                        "nap_instrumental", "dance_instrumental", "talk_instrumental", "read_instrumental", "party_instrumental", "puppet_instrumental", "learn_instrumental", "brush_instrumental",
                        "nap_construction", "dance_construction", "talk_construction", "read_construction", "party_construction", "puppet_construction", "learn_construction", "brush_construction",
                        "nap_silence", "dance_silence", "talk_silence", "read_silence", "party_silence", "puppet_silence", "learn_silence", "brush_silence",
                        "nap_white", "dance_white", "talk_white", "read_white", "party_white", "puppet_white", "learn_white", "brush_white",
                        "nap_mtb2", "dance_mtb2", "talk_mtb2", "read_mtb2", "party_mtb2", "puppet_mtb2", "learn_mtb2", "brush_mtb2",
                        "nap_mtb5", "dance_mtb5", "talk_mtb5", "read_mtb5", "party_mtb5", "puppet_mtb5", "learn_mtb5", "brush_mtb5"),
               names_to = "activity",
               values_to = "rating") %>% 
  separate(col = activity,
           into = c("activity", "noise_type"),
           sep = '[_]') %>% 
  na.omit()

View(tidydata_4)
print

## Make rating category numeric
tidydata_4$rating <- as.integer(tidydata_4$rating)

## Make activity and noise_type categories factor
#tidydata_4$activity <- as.factor(tidydata_4$activity)
#tidydata_4$noise_type <- as.factor(tidydata_4$noise_type)

## Calculate confidence intervals
cidata_3a <- tidydata_4 %>%
  group_by(activity, noise_type) %>% 
  summarise(n = n(),
            mean_rating = mean(rating, na.rm=TRUE), 
            sem = sd(rating) / sqrt(n),
            ci.l = mean_rating - sem * 1.96,
            ci.u = mean_rating + sem * 1.96)

View(cidata_3a)

# Data Visualization
ggplot(data = cidata_3a, 
       mapping = aes(x = noise_type, y = mean_rating)) +
  geom_pointrange(aes(ymin = ci.l,
                     ymax = ci.u)) +
  facet_wrap(~activity) +
  coord_flip() + 
  xlab("Activity") +
  ylim(1,7) +
  ylab("Rating") +
  ggthemes::scale_color_solarized() + 
  ggthemes::theme_few()

ggplot(data = tidydata_4, 
       mapping = aes(x = noise_type, y = rating, fill = activity)) +
  geom_bar() +
  xlab("Activity") +
  ylim(1,7) +
  ylab("Rating") +
  ggthemes::scale_color_solarized() + 
  ggthemes::theme_few()

### Regression

glmer_3a <- stan_glmer(formula = rating ~ activity * noise_type + (1 | subject_id),
                       family = gaussian,
                       data = tidydata_4)

summary_3a <- summary(glmer_3a, probs = c(0.025,0.975)))

saveRDS(summary_3a, here())

#Child Data
#Load data
data_3b <- read.csv("childrendatalog3b.csv", na.strings = c("", "NA"))

View(data_3b)

## Clean data
cleandata_3b <- data_3b %>% 
  clean_names() %>% 
  select(-c(notes))

View(cleandata_3b)

tidydata_3b <- cleandata_3b %>%  
  filter(included == "Yes") #exclude participants who did not pass comprehension check

View(tidydata_3b)

tidydata_3b$age_months <- as.integer(tidydata_3b$age_months) #make age column numeric
tidydata_3b$age_years <- as.numeric(tidydata_3b$age_years)
tidydata_3b$response <- as.integer(tidydata_3b$response) #make rating column numeric

summary(tidydata_3b, decimal = 2)

## Make activity and noise_type categories factor
#tidydata_4$activity <- as.factor(tidydata_4$activity)
#tidydata_4$noise_type <- as.factor(tidydata_4$noise_type)

## Calculate confidence intervals
cidata_3b <- tidydata_3b %>%
  group_by(activity, noise_type) %>% 
  summarise(n = n(),
            mean_response = mean(response, na.rm=TRUE), 
            sem = sd(response) / sqrt(n),
            ci.l = mean_response - sem * 1.96,
            ci.u = mean_response + sem * 1.96)

View(cidata_3b)

## Calculate confidence intervals by age
cidata_3bb <- tidydata_3b %>%
  group_by(activity, noise_type, age_years) %>% 
  summarise(n = n(),
            mean_response = mean(response, na.rm=TRUE), 
            sem = sd(response) / sqrt(n),
            ci.l = mean_response - sem * 1.96,
            ci.u = mean_response + sem * 1.96)

View(cidata_3bb)

# Data Visualization
ggplot(data = cidata_3b, 
       mapping = aes(x = noise_type, y = mean_response)) +
  geom_pointrange(aes(ymin = ci.l,
                      ymax = ci.u)) +
  facet_wrap(~activity) +
  coord_flip() + 
  xlab("Activity") +
  ylim(1,4) +
  ylab("Rating") +
  ggthemes::scale_color_solarized() + 
  ggthemes::theme_few()

ggplot(data = cidata_3bb, 
       mapping = aes(x = age_years, y = mean_response, fill = noise_type)) +
  geom_bar(stat = 'identity', position = 'dodge') +
  facet_wrap(~activity) +
  xlab("Activity") +
  ylab("Rating") +
  ggthemes::scale_color_solarized() + 
  ggthemes::theme_few()

### Regression

glmer_3a <- stan_glmer(formula = rating ~ activity * noise_type + (1 | subject_id),
                       family = gaussian,
                       data = tidydata_4)

summary_3a <- summary(glmer_3a, probs = c(0.025,0.975)))

saveRDS(summary_3a, here())

