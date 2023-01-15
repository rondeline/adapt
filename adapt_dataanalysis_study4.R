# Load libraries
library(here)
library(dplyr)
library(janitor)
library(stringr)
library(forcats)
library(rstanarm)
library(ggplot2)
library(ggthemes)
library(RColorBrewer)

## Load data
data_4b <- read.csv(here("data", "childrendatalog4b.csv")) #load data

## Tidy data
tidydata_4b <- data_4b %>%
  clean_names() %>%  #lowercase column names
  filter(arm == "Pilot_B",
         included == "Yes") %>% #exclude pilot participants and those who could not be retained for analysis failed attention and sound checks
  select(subject_id:rationale) %>%
  na.omit()

## Remove extra spaces
tidydata_4b$activity <- str_trim(tidydata_4b$activity)
tidydata_4b$sound_type <- str_trim(tidydata_4b$sound_type)
tidydata_4b$race <- str_trim(tidydata_4b$race)
tidydata_4b$race <- str_replace(tidydata_4b$race, "$/$ ", "$/$")

## Turn response column into integers
tidydata_4b$response <- as.integer(tidydata_4b$response)

## Scale Age
##tidydata_4b$age_centered <- scale(joineddata_2b$age_months, scale = FALSE) #age

## Get demographic data
##completedata_4b$age <- as.integer(completedata_4b$age) #age
##summary(completedata_4b)

demodata_4b <- tidydata_4b %>%
  select(subject_id,race, trial) %>%
  filter(trial == 1) %>%  #get one value per participant
  mutate(race_count = ifelse((str_detect(race, ",")), "Multiracial", race)) %>% 
  count(race_count)

demodata_4b

## Calculate confidence intervals

### Aggregate confidence intervals
cidata_4b <- tidydata_4b %>%
  group_by(activity, sound_type) %>%
  summarise(ci.l = binom::binom.bayes(x = sum(response), n = n())$lower,
            ci.u = binom::binom.bayes(x = sum(response), n = n())$upper,
            n = n(),
            mean_response = mean(response)) %>% 
  mutate(activity = fct_reorder(activity, mean_response, .desc = TRUE))

View(cidata_4b)

### Binned confidence intervals
cidataage_4b <- tidydata_4b %>%
  group_by(activity, sound_type, age_years) %>%
  summarise(ci.l = binom::binom.bayes(x = sum(response), n = n())$lower,
            ci.u = binom::binom.bayes(x = sum(response), n = n())$upper,
            n = n(),
            mean_response = mean(response)) %>% 
  mutate(activity = fct_reorder(activity, mean_response, .desc = TRUE))

View(cidataage_4b)

# Data visualization and stats

## x-axis labels
xlabels <- c("5-talker", "music", "silence", "white")

## Aggregated Data
ggplot(data = cidata_4b, mapping = aes(x = sound_type, y = mean_response, fill = activity)) +
  geom_col() +
  geom_linerange(aes(ymin = ci.l,
                     ymax = ci.u)) +
  ylim(0,1) +
  xlab("Auditory Stimulus") +
  ylab("Rating") +
  scale_fill_brewer(palette="Set2") +
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

## Binned Data
ggplot(data = cidataage_4b, mapping = aes(x = sound_type, fill = as.factor(age_years))) +
  geom_bar(aes(y = mean_response),
           position = "dodge", stat = "identity") +
  geom_linerange(aes(ymin = ci.l,
                     ymax = ci.u),
                 position = position_dodge(width = 0.9)) +
  facet_wrap(~activity) +
  xlab("Auditory Stimulus") +
  ylim(0,1) +
  ylab("Rating") +
  scale_fill_brewer(palette="Set2") +
  scale_x_discrete(labels = xlabels) +
  geom_hline(yintercept = .5, lty =2) +
  theme_few() +
  theme(title = element_text(size = 11),
        axis.text = element_text(size = 8),
        legend.text = element_text(size = 8),
        legend.title = element_text(size = 8),
        panel.spacing = unit(1.5, "lines"),
        legend.direction = "horizontal",
        legend.position = c(.5, .5),
        axis.text.x = element_text(angle = 45, hjust=1),
        text = element_text(size = 13)) +
  labs(fill = "Age (Years)",
       title = "Sensitivity to evironmental selection increased with age")